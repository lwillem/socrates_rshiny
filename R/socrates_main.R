#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => LOAD AND SELECT SOCIAL CONTACT SURVEY DATA
#
#  Copyright 2020, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________

# load packages and help functions
source('R/download_matrices.R')
source('R/load_config.R')
source('R/contact_matrix_fix.R')
source('R/plot_mean_number_contacts.R')
source('R/plot_social_contact_matrix.R')
source('R/survey_data_description.R')
source('R/config_comix.R')

# example
#contact_matrix(polymod, countries = "United Kingdom", age.limits = c(0, 1, 5, 15))

run_social_contact_analysis <- function(country,daytype,touch,duration,gender,
                                        cnt_location,cnt_matrix_features,age_breaks_text,
                                        max_part_weight,
                                        bool_transmission_param,age_susceptibility_text,age_infectiousness_text,
                                        cnt_reduction,
                                        wave){
  
  # get social contact matrix using all features, without interventions
  cnt_matrix_ui <- get_contact_matrix(country,daytype,touch,duration,gender,
                                      cnt_location,cnt_matrix_features,age_breaks_text,
                                      max_part_weight = max_part_weight,
                                      wave)
  
  # CLI
  fct_out <- cnt_matrix_ui
  
  # create option to add notes
  fct_out$notes <- NULL
  
  # if matrix contains NA's => reciproity is not possible ==>> add warning
  if(any(is.na(cnt_matrix_ui$matrix)) & 
     opt_matrix_features[[1]]  %in% cnt_matrix_features){
    fct_out$notes <- "Contact matrix contains NA, reciprocity is not possible."
  }
  
  # include physical distancing?
  bool_physical_distancing <- any(cnt_reduction!=0)
  if(bool_physical_distancing){
    if(any(is.na(cnt_matrix_ui$matrix))){
      fct_out$notes <- c(fct_out$notes,"Contact matrix contains NA, no distancing analysis possible.")
    } else {
      # get location specific contact matrix (no intervention)
      matrix_loc <- get_location_matrices(country,daytype,touch,duration,gender,
                                          cnt_location,
                                          cnt_matrix_features,
                                          age_breaks_text,
                                          max_part_weight,
                                          wave)

      # unlist contact reduction parameter
      cnt_reduction_df <- unlist(cnt_reduction)
      
      # account for the location-specific reductions
      matrix_total            <- NULL
      matrix_per_capita_total <- NULL
      i_loc <- cnt_location[1]
      for(i_loc in cnt_location){

        # check if dataset contains a matrix for this location
        if(all(is.na(matrix_loc[[i_loc]]$matrix))){
          # add UI note that this reduction is not possible
          fct_out$notes <- c(fct_out$notes,paste0("Physical distancing for ",i_loc,' not possible (category not present)'))  
        } else {
        
          # get relative contact reduction
          relative_reduction <- ifelse(i_loc %in% names(cnt_reduction_df),cnt_reduction_df[i_loc],0)
  
          # add UI note on reduction if > 0
          if(relative_reduction>0){
            fct_out$notes <- c(fct_out$notes,paste0("Physical distancing for ",i_loc,': ',
                                                    round(relative_reduction*100),"% reduction"))  
          }
          
          # get remaining contacts
          reduction_factor  <- (1 - relative_reduction)
          
          if(is.null(matrix_total)){
              matrix_total <- matrix_loc[[i_loc]]$matrix * reduction_factor
              matrix_per_capita_total <- matrix_loc[[i_loc]]$matrix_per_capita * reduction_factor
          } else{
              matrix_total <- matrix_total + matrix_loc[[i_loc]]$matrix * reduction_factor
              matrix_per_capita_total <- matrix_per_capita_total + matrix_loc[[i_loc]]$matrix_per_capita * reduction_factor
          } # end if-else  
        } #end if-clause: does location matrix exists?
      }

      # calculate impact on transmission
      model_comparison <- compare_contact_matrices(matrix_total,cnt_matrix_ui$matrix,
                                                   bool_transmission_param,
                                                   age_susceptibility_text,age_infectiousness_text)
      
      # update UI results
      cnt_matrix_ui$matrix            <- matrix_total
      cnt_matrix_ui$matrix_per_capita <- matrix_per_capita_total
      
      # copy notes
      if(length(fct_out$notes)>0){
        model_comparison$notes <- rbind(model_comparison$notes,
                                    matrix(fct_out$notes,ncol=1))
      }
      
      # combine cnt matrix and comparison
      fct_out <- c(cnt_matrix_ui[1],
                   model_comparison,
                   cnt_matrix_ui[-1])
      
    } # end else (no NA's present) 
  }# end if intervention
  
  # Add relative incidence (if possible)
  if(!any(is.na(cnt_matrix_ui$matrix))){
    # adjust for age-specific transmission?
    mij <- cnt_matrix_ui$matrix
    if(bool_transmission_param){
      mij <- adjust_mij_transmission(mij,age_susceptibility_text,age_infectiousness_text)
    }
    relative_incidence        <- standardize_RI(eigen(mij)$vectors[,1])
    names(relative_incidence) <- colnames(cnt_matrix_ui$matrix)
    
    # add relative incidence to output list
    fct_out <- c(fct_out[1],
                 relative_incidence=list(relative_incidence),
                 fct_out[-1])
  }
  
  # add meta data on matrix parameters
  meta_data <- data.frame(data_set = country,
                          day_type = unlist(daytype),
                          contact_intensity = unlist(touch),
                          contact_duration = unlist(duration),
                          contact_gender = unlist(gender),
                          contact_locations = paste(cnt_location,collapse=', '),
                          contact_features = paste(cnt_matrix_features,collapse=', '),
                          age_breaks = age_breaks_text,
                          max_part_weight = max_part_weight,
                          wave = wave,
                          row.names=NULL)
  
  
  # add distancing info, if present
  if(length(fct_out$notes)>0){
    meta_data[,paste('distancing info',1:length(fct_out$notes))] <- fct_out$notes
  }

  # add meta_data to function output
  fct_out$meta_data <- data.frame(parameter = names(meta_data),
                                  value = t(meta_data),
                                  row.names=NULL,
                                  stringsAsFactors = F)
  
  # reformat parameter names
  fct_out$meta_data$parameter <- gsub('_',' ',fct_out$meta_data$parameter)
  
  # return
  return(fct_out)
}

## MAIN FUNCTION ####
get_contact_matrix <- function(country,daytype,touch,duration,gender,
                               cnt_location,cnt_matrix_features,age_breaks_text,
                               max_part_weight,wave){
  
  # parse age intervals
  age_breaks_num <- parse_age_values(age_breaks_text,bool_unique = TRUE)
  
  # if no breaks specified, group all participants
  if(length(age_breaks_num)==0){
    age_breaks_num <- 0
  }
  
  bool_reciprocal      <- opt_matrix_features[[1]]  %in% cnt_matrix_features
  bool_weigh_age_group <- opt_matrix_features[[2]]  %in% cnt_matrix_features
  bool_weigh_dayofweek <- opt_matrix_features[[3]]  %in% cnt_matrix_features
  bool_age_range       <- opt_matrix_features[[4]]  %in% cnt_matrix_features
  bool_age_missing     <- opt_matrix_features[[5]]  %in% cnt_matrix_features
  bool_suppl_professional_cnt <- opt_matrix_features[[6]]  %in% cnt_matrix_features
  bool_hhmatrix_selection    <- opt_matrix_features[[7]]  %in% cnt_matrix_features
  
  # get specific social_mixr survey object
  survey_object <- get_survey_object(country      = country,
                                     daytype      = daytype,
                                     touch        = touch,
                                     duration     = duration,
                                     gender       = gender,
                                     cnt_location = cnt_location,
                                     bool_reciprocal   = bool_reciprocal,
                                     bool_suppl_professional_cnt =  bool_suppl_professional_cnt,
                                     bool_hhmatrix_selection = bool_hhmatrix_selection,
                                     wave         = wave)
  
  if(nrow(survey_object$participants)==0){
    return(list(matrix=NA,
                participants = NA,
                warning="Participant selection too strict... no data left!")
    )
  }
  
  if(nrow(survey_object$contacts) == 0){
    return(list(matrix=NA,
                participants = NA,
                warning="Contact selection too strict... no data left!")
    )
  }
  
  # (re)set rng seed (if ages are sampled from the reported range)
  set.seed(rng_seed)
  
  # run social_mixr function
  matrix_out <- contact_matrix(survey          = survey_object, 
                               age.limits      = age_breaks_num,
                               symmetric       = bool_reciprocal,
                               weigh.age.group = bool_weigh_age_group,
                               weigh.dayofweek = bool_weigh_dayofweek,
                               max.part.weight = max_part_weight,
                               estimated.contact.age = ifelse(bool_age_range,'sample','mean'),
                               missing.contact.age = ifelse(bool_age_missing,'remove','ignore'),
                               quiet           = TRUE)
  
  
  # add per capita contact rate (if demography data)
  if('demography' %in% names(matrix_out) && !any(is.na(matrix_out$matrix))){
    num_age_groups <- nrow(matrix_out$demography)
    pop_matrix     <- matrix(rep(matrix_out$demography$population,num_age_groups),ncol=num_age_groups,byrow = T)
    matrix_out$matrix_per_capita <- matrix_out$matrix / pop_matrix
  }
  
  ## TMP: remove weights from output
  if('weights' %in% names(matrix_out)){
    tmp <- matrix_out$weights
    matrix_out$weights <- NULL
    matrix_out$weights <- tmp
    
  }
  
  # ## add date
  dates_str <- paste0(survey_object$participants$year,'-',
                      survey_object$participants$month,'-',
                      survey_object$participants$day)
  dates_str <- dates_str[!grepl('NA',dates_str)]
  
  if(length(dates_str)==0){
    dates_all <- unique(survey_object$participants$year)
    matrix_out$survey_period <- paste(c('Survey period: ', paste(dates_all, collapse=', ')),collapse=' ')
  } else{
    dates_all <- as.Date(dates_str)
    matrix_out$survey_period <- paste(c('From', paste(range(dates_all), collapse=' to ')),collapse=' ')
  }
  

  # return
  matrix_out
}

## GET SURVEY DATA ####
get_survey_object <- function(country,
                              daytype,
                              touch,
                              duration,
                              gender,
                              cnt_location,
                              bool_reciprocal,
                              bool_suppl_professional_cnt,
                              bool_hhmatrix_selection,
                              missing.contact.age = "remove",  # adopted from socialmixr package
                              #missing.contact.age = "keep",  # adopted from socialmixr package
                              wave,
                              quiet = FALSE){
  
  # select dataset filename and load #####
  sel_dataset <- opt_country_admin[opt_country_admin$name == country,]
  
  # get original data
  survey_data <- readRDS(sel_dataset$dataset)
  data_part   <- survey_data$participants
  data_cnt    <- survey_data$contacts
  
  # option to select country-specific participant and contact data
  if(nchar(sel_dataset$country)>0){
    bool_country <- (data_part$country == sel_dataset$country)
    data_part    <- data_part[bool_country,]
    data_cnt     <- data_cnt[data_cnt$part_id %in% data_part$part_id,]
  }
  
  # select type of day ####
  if(!daytype %in% names(opt_day_type[c(1,6)])){
    bool_dayofweek <- data_part$dayofweek >= 0 # all
    if(daytype == opt_day_type[[3]]){ # weekend
      bool_dayofweek <- data_part$dayofweek %in% c(0,6)
      data_part      <- data_part[bool_dayofweek,]
    } else{
      bool_dayofweek <- data_part$dayofweek %in% 1:5
      data_part      <- data_part[bool_dayofweek,]
    }
  }
  
  # select period ####
  if(daytype %in% names(opt_day_type[4:6])){
    if(!any(data_part$holiday)){
      load('data/holiday_all.RData')
      country_iso3 <- countrycode(unlist(country), 'country.name', 'iso3c')
      country_holiday_data <- holiday_all[holiday_all$iso3 == country_iso3,]
      data_part$date <- as.Date(paste(data_part$day,
                                      data_part$month,
                                      data_part$year,
                                      sep='/')
                                ,'%d/%m/%Y')
      data_part$holiday <- data_part$date %in% country_holiday_data$date
    }
    
    # check if holiday is a boolean... if not, try to convert and throw warning
    if(typeof(data_part$holiday) != 'logical'){
      warning("holiday variable is not a 'logical', try to convert binary 0/1 to FALSE/TRUE")
      data_part$holiday <- data_part$holiday == 1
    }
    
    if(daytype == opt_day_type[[4]]){
      # if(!any(data_part$is_holiday)){ # if no holiday period data
      #   print("NO HOLIDAY DATA... USE REGULAR PERIOD DATA")
      # } else{ # select holiday period data
      data_part <- data_part[data_part$holiday,]
      # }
    } else{ # select regular period data
      data_part <- data_part[!data_part$holiday,]
    }
    # select cnt data of remaining participants
    data_cnt <- data_cnt[data_cnt$part_id %in% data_part$part_id]
  }
  
  # select contact duration ####
  if(duration != opt_duration[[1]]){
    #print(duration)
    duration_code <- which(opt_duration == duration)-1
    
    if(duration %in% names(opt_duration[2:3]) ){
      bool_duration <- !is.na(data_cnt$duration_multi)  & data_cnt$duration_multi <= duration_code
      data_cnt      <- data_cnt[bool_duration,]
    } else{
      bool_duration <- !is.na(data_cnt$duration_multi)  & data_cnt$duration_multi >= duration_code
      data_cnt      <- data_cnt[bool_duration,]
    }
  }
  
  # select contact intensity ####
  if(touch != opt_touch[[1]]){
    touch_code    <- which(opt_touch == touch)-1
    bool_touching <- !is.na(data_cnt$phys_contact) & data_cnt$phys_contact == touch_code
    data_cnt      <- data_cnt[bool_touching,]
    #print(touch)
  }
  
  # select gender ####
  if(gender != opt_gender[[1]]){
    
    # first select cnt data of remaining participants
    data_cnt <- data_cnt[data_cnt$part_id %in% data_part$part_id]
    
    # set gender-specific booleans
    bool_cnt_female  <- data_cnt$cnt_gender   == 'F'
    bool_part_female <- data_part$part_gender == 'F'
    
    # merge dataset to compare participant and contact gender
    data_cnt_gender  <- merge(data_cnt,data_part[,c('part_id','part_gender')],by='part_id')
    bool_gender_diff <- data_cnt_gender$cnt_gender != data_cnt_gender$part_gender
    
    if(gender == opt_gender[[2]]){                  # female-female
      data_cnt       <- data_cnt[bool_cnt_female,]
      data_part      <- data_part[bool_part_female,]
    } else if(gender == opt_gender[[5]]){           # male-male
      data_cnt       <- data_cnt[!bool_cnt_female,]
      data_part      <- data_part[!bool_part_female,]
    } else if(bool_reciprocal){
      data_cnt       <- data_cnt[bool_gender_diff,]
    } else {
      if(gender == opt_gender[[3]]){                # female-male
        data_cnt       <- data_cnt[bool_gender_diff,]
        data_part      <- data_part[bool_part_female,]
      } else if(gender == opt_gender[[4]]){         # male-female
        data_cnt       <- data_cnt[bool_gender_diff,]
        data_part      <- data_part[!bool_part_female,]
      }
    }
  }
  
  ## select wave (optional) ----
  if(wave != opt_waves[[1]]){
    if(!is.null(data_part$wave) & wave %in% data_part$wave){
      # print(table(data_part$wave))
      # print(table(data_part$wave == wave))
      
      bool_part_wave <- data_part$wave == wave
      data_part <- data_part[bool_part_wave,]
      
      bool_cnt_wave <- data_cnt$part_id %in%  data_part$part_id
      data_cnt  <- data_cnt[bool_cnt_wave,]
      # print(paste('select wave', wave))
      # print(table(data_part$wave))
    }
  }
  
  #__________________________________________________________________________
  # adjust location data: missing and multiple locations ####
  if(nrow(data_cnt)>0){
    # set data.table to data.frame
    data_cnt_tmp <- data.frame(data_cnt)
    
    # select all location-specific columns
    cnt_location_colnames <- c(paste0('cnt_',tolower(opt_location)))
    data_cnt_tmp <- data_cnt_tmp[,cnt_location_colnames]
    dim(data_cnt_tmp)
    
    # replace value 'NA' for a location to 'false' (=not-present)
    data_cnt_tmp[is.na(data_cnt_tmp)] <- 0
    
    # add missing location to "other"
    cnt_loc_missing <- rowSums(data_cnt_tmp,na.rm=T) == 0
    data_cnt_tmp$cnt_otherplace  <- as.numeric(data_cnt_tmp$cnt_otherplace | cnt_loc_missing)
    
    # 1. calculate cumulative sum (from left to right)
    tmp_loc_cumsum <- t(apply(data_cnt_tmp,1,cumsum))
    
    # 2. set locations with cummulative sum >1 (== not unique and not the "main location") to 0
    data_cnt_tmp[tmp_loc_cumsum>1] <- 0
    
    # 3. copy adjusted location data back
    data_cnt[,cnt_location_colnames] <- data_cnt_tmp
  }

  #__________________________________________________________________________
  
  # household members: get matrix with only household members? ####
  if('is_hh_member' %in% names(data_cnt) && !is.na(bool_hhmatrix_selection) && 
     bool_hhmatrix_selection == TRUE){
    flag_cnt_adapt                       <- data_cnt$cnt_home == 1 & data_cnt$is_hh_member == FALSE
    data_cnt$cnt_home[flag_cnt_adapt]    <- 0
    data_cnt$cnt_leisure[flag_cnt_adapt] <- 1
  }
  
  #select location ####
  if(length(cnt_location)==0){
    print("WARNING: NO LOCATIONS SPECIFIED...")
    data_cnt <- data_cnt[0,]
  } else if(!identical(as.character(cnt_location),as.character(opt_location))
            && nrow(data_cnt)>0){
    
    # set data.table to data.frame
    data_cnt_tmp <- data.frame(data_cnt)
    
    # select requested location-specific columns
    cnt_location_colnames <- c(paste0('cnt_',tolower(cnt_location)))
    
    # select columns
    if(length(cnt_location)>1){
      is_present <- rowSums(data_cnt_tmp[,cnt_location_colnames] == 1,na.rm=T)
    } else{
      is_present <- data_cnt_tmp[,cnt_location_colnames]
    }
    
    # select contact at specified location(s) 
    bool_location <- is_present > 0
    
    # select
    data_cnt <- data_cnt[bool_location,]
    
    # add warning
    if(!any(bool_location)){
      print("WARNING: NO CONTACTS LEFT AFTER LOCATION SELECTION...")
    } 
  }
  
  
  # remove temporal data from contact data.frame
  if('dayofweek' %in% names(data_cnt)){
    data_cnt$dayofweek <- NULL
  }
 
  # suppl. professional contact? ####
  # ==>> remove imputed supplementary professional contacts?
  if('is_imputed' %in% names(data_cnt) && !is.na(bool_suppl_professional_cnt) && 
     bool_suppl_professional_cnt == FALSE){
    data_cnt <- data_cnt[data_cnt$is_imputed == 0,]
  }
  
  # create new survey object
  mixr_survey <- survey(data_part, data_cnt)
  
  # return
  return(mixr_survey)
  
}


#mija <- contact_matrix(polymod, countries = "Belgium", age.limits = c(0, 18, 45,65))$matrix*c(1,0.5,0.6,1)
#mijb <- contact_matrix(polymod, countries = "Belgium", age.limits = c(0, 18, 45, 65))$matrix
compare_contact_matrices <- function(mija,mijb,
                                     bool_transmission_param,age_susceptibility_text,age_infectiousness_text){
  
  # mij ratio
  mij_ratio     <- mija/mijb
  
  # adjust for age-specific transmission?
  if(bool_transmission_param){
    mija <- adjust_mij_transmission(mija,age_susceptibility_text,age_infectiousness_text)
    mijb <- adjust_mij_transmission(mijb,age_susceptibility_text,age_infectiousness_text)
  }
  
  if(any(is.na(mija))|any(is.na(mijb))){
    warning('Social contact matrix contains NA... no comparison possible!')
    out <- list(notes='Social contact matrix contains NA... no comparison possible!')
  } else{
    R0_ratio      <- max(eigen(mija)$values)/max(eigen(mijb)$values)
    
    # relative incidence 
    RIa             <- standardize_RI(eigen(mija)$vectors[,1])
    RIb             <- standardize_RI(eigen(mijb)$vectors[,1])
    RI_ratio        <- RIa/RIb
    names(RI_ratio) <- colnames(mija)
    
    #output 
    out <- list(R0_ratio=R0_ratio,mij_ratio=mij_ratio,RI_ratio=RI_ratio,
                notes="ratio = with intervention / without intervention")
    
    # fix NA-results
    if(identical(mija,mijb)){ # set 1 if mija == mijb
      for(i in 1:length(out)) { 
        out[[i]][] <- 1 
      }
    } else if(sum(mija) == 0){ # set 0 if mija[] == 0
      for(i in 1:length(out)) { 
        out[[i]][] <- 0 
      }
    }
  }
  
  return(out)
}

# help function to standardize the relative incidence
standardize_RI<-function(vec){return(vec/sum(vec))}


# convert character string with age-specific values into (positive) numeric values
parse_age_values <- function(age_values_text,bool_unique = TRUE){
  
  # make sure age_values_text is a character string
  age_values_text <- as.character(age_values_text)
  
  # set age intervals
  age_values_num <- as.numeric(unlist(strsplit(age_values_text,",")))
  
  # remove missing values (eg. by typing ',,')
  age_values_num <- age_values_num[!is.na(age_values_num)]
  
  # make sure the ages are postive, unique and increasing 
  if(any(age_values_num<0)){
    warning('Negative values are removed')
  }
  
  if(bool_unique){
    age_values_num <- unique(age_values_num[age_values_num>=0])
    age_values_num <- sort(age_values_num)    
  }

  return(age_values_num)
}


# adjust mij for transmission parameters (if possible)
adjust_mij_transmission <- function(mij,age_susceptibility_text,age_infectiousness_text){
  
  # parse transmission parameters
  age_susceptibility_num <- parse_age_values(age_susceptibility_text,bool_unique = FALSE)
  age_infectiousness_num    <- parse_age_values(age_infectiousness_text,bool_unique = FALSE)
  
  # check dimensions
  if(nrow(mij) == length(age_susceptibility_num) && 
     nrow(mij) == length(age_infectiousness_num)){
    
    num_age_groups        <- length(age_infectiousness_num)
    susceptibility_matrix <- matrix(rep(age_susceptibility_num,num_age_groups),ncol=num_age_groups,byrow =F)
    infectiousness_matrix    <- matrix(rep(age_infectiousness_num,num_age_groups),ncol=num_age_groups,byrow =T)
    
    mij <- mij * susceptibility_matrix * infectiousness_matrix
  } else {
    warning("Transmission parameters do not align with age groups")
  }
  
  return(mij)
}

parse_input_list <- function(input_list,column_tag){
  
  # get column names
  sel_colnames <- c(names(input_list)[grepl(column_tag,names(input_list))])
  
  # aggregate
  if(length(sel_colnames)==0){
    age_out <- 1
  } else{
    age_out <- NULL
    for (i in sel_colnames) {
      age_out <- c(age_out, input_list[[i]])
    }
  }
  
  # make string
  age_out <- paste(age_out,collapse=',')
  
  # return
  return(age_out)
}


get_location_matrices <- function(country,daytype,touch,duration,gender,
                                  cnt_location,
                                  cnt_matrix_features,
                                  age_breaks_text,
                                  max_part_weight,
                                  wave){
  
  
  # location specific ==> NOT reciprocal
  sel_cnt_matrix_features <- cnt_matrix_features[!grepl('recipocal',cnt_matrix_features,ignore.case = T)]
  
  # initialise list
  matrix_list <- list()
  
  for(i_loc in 1:length(cnt_location)){
    matrix_list[i_loc] <- list(get_contact_matrix(country,daytype,touch,duration,gender,
                                                  cnt_location = cnt_location[i_loc],
                                                  sel_cnt_matrix_features,
                                                  age_breaks_text,
                                                  max_part_weight,
                                                  wave = wave))
  }
  
  # add location names
  names(matrix_list) <- cnt_location
  
  return(matrix_list)
}

