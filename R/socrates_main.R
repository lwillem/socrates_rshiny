#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => LOAD AND SELECT SOCIAL CONTACT SURVEY DATA
#
#  Copyright 2024, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________

# load packages and help functions
source('R/download_matrices.R')
source('R/load_country_admin.R')
source('R/load_config_base.R')
source('R/contact_matrix_fix.R')
source('R/plot_mean_number_contacts.R')
source('R/plot_social_contact_matrix.R')
source('R/plot_mean_number_infected.R')

# example
#contact_matrix(polymod, countries = "United Kingdom", age.limits = c(0, 1, 5, 15))

run_social_contact_analysis <- function(country,daytype,touch,duration,gender,
                                        cnt_location,cnt_matrix_features,age_breaks_text,
                                        weight_threshold,
                                        cnt_reduction,
                                        wave,
                                        age_susceptibility_text = NA,
                                        age_infectiousness_text = NA,
                                        bool_NGA_analysis       = FALSE,
                                        q_text                  = 1,
                                        delta_p_text            = 0.1,
                                        nrgen_text              = 3
                                        ){
  
  # check age-specific parameters
  num_age_groups <- length(parse_age_values(age_breaks_text))
  if(is.na(age_susceptibility_text) || age_susceptibility_text == '1'){ age_susceptibility_text <- paste(rep(1,num_age_groups),collapse=',') }
  if(is.na(age_infectiousness_text) || age_infectiousness_text == '1'){ age_infectiousness_text <- paste(rep(1,num_age_groups),collapse=',') }

  # Use withCallingHandlers to capture warnings
  warnings_list <- list()
  withCallingHandlers(
    {
      # get social contact matrix using all features, without interventions
      cnt_matrix_ui <- get_contact_matrix(country,
                                          daytype,
                                          touch,
                                          duration,
                                          gender,
                                          cnt_location,
                                          cnt_matrix_features,
                                          age_breaks_text,
                                          weight_threshold = weight_threshold,
                                          wave)
    },
    warning = function(w) {
      warnings_list <<- c(warnings_list, conditionMessage(w)) # Append warning to the list
    }
  )
  
  # CLI
  fct_out <- cnt_matrix_ui
  
  # create option to add notes
  fct_out$notes <- NULL
  
  # add warnings (except the linear estimation of age groups)
  # if matrix contains NA's => reciprocity is not possible ==>> warning is given by contact_matrix()
  warnings_list <- unlist(warnings_list[!grepl('Linearly estimating age group',warnings_list)])
  if(length(warnings_list) > 0){
    fct_out$notes <- warnings_list    
  }
 
  # if there is a contact matrix but data sparseness forced to join age groups ==>> add warning
  if(!is.null(nrow(cnt_matrix_ui$matrix)) && (num_age_groups != nrow(cnt_matrix_ui$matrix) || 
                                             num_age_groups != nrow(cnt_matrix_ui$matrix))){
    fct_out$notes <- c(fct_out$notes,"The oldest age groups are merged to prevent fatal errors due to data sparseness.")  
    fct_out$notes <- c(fct_out$notes,"Relative incidence cannot be calculated with merged age groups.")  
  } 
  
  # reciprocity has also effect on the analysis by gender
  if(gender %in% unlist(opt_gender[3:4]) &
     opt_matrix_features[[1]]  %in% cnt_matrix_features){
    fct_out$notes <- c(fct_out$notes,"Gender-specific data selection accounts for reciprocity, so Male-Female == Female-Male.")
  }
  
  # include physical distancing?
  bool_physical_distancing <- any(cnt_reduction!=0)
  if(bool_physical_distancing){
    if(any(is.na(cnt_matrix_ui$matrix))){
      fct_out$notes <- c(fct_out$notes,"Contact matrix contains NA, no distancing analysis possible.")
    } else {
      # get location specific contact matrix (no intervention)
      matrix_loc <- get_location_matrices(country,
                                          daytype,
                                          touch,
                                          duration,
                                          gender,
                                          cnt_location,
                                          cnt_matrix_features,
                                          age_breaks_text,
                                          weight_threshold,
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
  
  # # Add relative incidence (if possible)
  if(any(is.na(cnt_matrix_ui$matrix))){
    warning("NGA analysis is not possible because contact matrix contains NA")
    fct_out$notes <- c(fct_out$notes,"NGA analysis is not possible because contact matrix contains NA")  
  } else {
    
    mij    = cnt_matrix_ui$matrix
    qs     = as.numeric(parse_age_values(age_susceptibility_text,bool_unique = FALSE))
    qi     = as.numeric(parse_age_values(age_infectiousness_text,bool_unique = FALSE))
    q      = as.numeric(parse_age_values(q_text,bool_unique = FALSE))
    p      = as.numeric(parse_age_values(delta_p_text,bool_unique = FALSE))
    nr_gen = as.numeric(parse_age_values(nrgen_text,bool_unique = FALSE))
    
    if(length(qs)==nrow(mij) & length(qi)==nrow(mij)){
    
      # calculate relative incidence
      NGM                       <- NGM_SIR(q=q,a=qs,M=t(mij),h=qi) # compute the NGM
      relative_incidence        <- standardize_RI(eigen(NGM)$vectors[,1])
      
      # sensitivity and elasticity
      if(bool_NGA_analysis){
        fct_out$NGA=run_NGA(M=mij,a=qs,h=qi,q=q,p=p,nr_gen=nr_gen)
      } 
    } else {
      relative_incidence <- rep(NA,nrow(mij))
      warning("NGA analysis is not possible because age groups do not match")
      fct_out$notes <- c(fct_out$notes,"NGA analysis is not possible because age groups do not match")  
    } # end if-else-clause to check age groups

    # add relative incidence to output list
    names(relative_incidence) <- colnames(cnt_matrix_ui$matrix)
    fct_out <- c(fct_out[1],
                 relative_incidence=list(relative_incidence),
                 fct_out[-1])    
    
  } # end if-else-clause to check on NA's in contact matrix

  
  # add meta data on matrix parameters
  meta_data <- data.frame(data_set          = country,
                          day_type          = unlist(daytype),
                          contact_intensity = unlist(touch),
                          contact_duration  = unlist(duration),
                          contact_gender    = unlist(gender),
                          contact_locations = paste(cnt_location,collapse=', '),
                          contact_features  = paste(cnt_matrix_features,collapse=', '),
                          age_breaks        = age_breaks_text,
                          age_groups        = paste(colnames(cnt_matrix_ui$matrix),collapse=', '),
                          weight_threshold  = weight_threshold,
                          wave              = wave,
                          age_specific_infectiousness = age_infectiousness_text,
                          age_specific_susceptibility = age_susceptibility_text,
                          row.names         = NULL)
  
  
  # add distancing info, if present
  if(length(fct_out$notes)>0){
    meta_data[,paste('distancing info',1:length(fct_out$notes))] <- fct_out$notes
  }
  
  # add NGA info, if NGA is active
  if(bool_NGA_analysis){
    meta_data$q_factor         <- q_text
    meta_data$delta_p          <- delta_p_text
    meta_data$nr_generations   <- nrgen_text
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
                               weight_threshold,wave){
  
  # parse age intervals
  age_breaks_num <- parse_age_values(age_breaks_text,bool_unique = TRUE)
  
  # if no breaks specified, group all participants
  if(length(age_breaks_num)==0){
    age_breaks_num <- 0
  }
  
  bool_reciprocal      <- opt_matrix_features[[1]]  %in% cnt_matrix_features
  bool_weigh_age       <- opt_matrix_features[[2]]  %in% cnt_matrix_features
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
                                     bool_reciprocal             = bool_reciprocal,
                                     bool_suppl_professional_cnt =  bool_suppl_professional_cnt,
                                     bool_hhmatrix_selection     = bool_hhmatrix_selection,
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
                               weigh.age       = bool_weigh_age,
                               weigh.dayofweek = bool_weigh_dayofweek,
                               weight.threshold      = weight_threshold,
                               estimated.contact.age = ifelse(bool_age_range,'sample','mean'),
                               missing.contact.age   = ifelse(bool_age_missing,'remove','ignore'),
                               return.part.weights   = TRUE)
  
  # make sure the row names are included
  rownames(matrix_out$matrix) <- colnames(matrix_out$matrix)

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
  dates_str <- survey_object$participants$date_str
  dates_str <- dates_str[!grepl('NA',dates_str)]
  
  if(length(dates_str)==0){
    dates_all <- unique(survey_object$participants$year)
    matrix_out$survey_period <- paste(c('Survey period: ', paste(dates_all, collapse=', ')),collapse=' ')
  } else{
    dates_all <- range(survey_object$participants$date,na.rm = TRUE)
    matrix_out$survey_period <- paste(c('From', paste(dates_all, collapse=' to ')),collapse=' ')
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
                              wave,
                              quiet = FALSE){
  
  # select dataset file name and load #####
  sel_dataset <- opt_country_admin[opt_country_admin$name == country,]
  
  # get original data
  survey_data <- load_survey_data(sel_dataset$dataset,sel_dataset$has_waves)
  data_part   <- survey_data$participants
  data_cnt    <- survey_data$contacts
  
  # option to select country-specific participant and contact data
  if(nchar(sel_dataset$country)>0){
    data_part    <- data_part[country == tolower(sel_dataset$country),]
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
      data_part <- data_part[data_part$holiday,]
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
    
    # make sure the gender variable is used as character
    data_cnt$cnt_gender   <- as.character(data_cnt$cnt_gender)
    data_part$part_gender <- as.character(data_part$part_gender)
    
    # set gender-specific booleans
    bool_cnt_female  <- data_cnt$cnt_gender   == 'F'
    bool_part_female <- data_part$part_gender == 'F'
    bool_cnt_male    <- data_cnt$cnt_gender   == 'M'
    bool_part_male   <- data_part$part_gender == 'M'
    
    # merge dataset to compare participant and contact gender
    data_cnt_gender  <- merge(data_cnt[,c('part_id','cnt_gender')],data_part[,c('part_id','part_gender')],by='part_id')
    data_cnt_gender$cnt_gender[!data_cnt_gender$cnt_gender %in% c('M','F')] <- NA
    data_cnt_gender$part_gender[!data_cnt_gender$part_gender %in% c('M','F')] <- NA
    bool_gender_diff <- data_cnt_gender$cnt_gender != data_cnt_gender$part_gender
    bool_gender_diff <- bool_gender_diff & !is.na(bool_gender_diff)
    
    if(gender == opt_gender[[2]]){                  # female-female
      data_cnt       <- data_cnt[bool_cnt_female,]
      data_part      <- data_part[bool_part_female,]
    } else if(gender == opt_gender[[5]]){           # male-male
      data_cnt       <- data_cnt[bool_cnt_male,]
      data_part      <- data_part[bool_part_male,]
    } else if(bool_reciprocal){
      data_cnt       <- data_cnt[bool_gender_diff,]
    } else {
      if(gender == opt_gender[[3]]){                # female-male
        data_cnt       <- data_cnt[bool_cnt_male,]
        data_part      <- data_part[bool_part_female,]
      } else if(gender == opt_gender[[4]]){         # male-female
        data_cnt       <- data_cnt[bool_cnt_female,]
        data_part      <- data_part[bool_part_male,]
      }
    }
  }
  
  # select wave (optional) ----
  if(!is.null(data_part$wave) & wave %in% data_part$wave){
      bool_part_wave <- data_part$wave == wave
      data_part <- data_part[bool_part_wave,]
      
      bool_cnt_wave <- data_cnt$part_id %in%  data_part$part_id
      data_cnt      <- data_cnt[bool_cnt_wave,]
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
                                     age_susceptibility_text,age_infectiousness_text){
  
  # mij ratio
  mij_ratio     <- mija/mijb
  
  # adjust for age-specific transmission
  mija <- adjust_mij_transmission(mija,age_susceptibility_text,age_infectiousness_text)
  mijb <- adjust_mij_transmission(mijb,age_susceptibility_text,age_infectiousness_text)
  
  if(any(is.na(mija))|any(is.na(mijb))){
    warning('Social contact matrix contains NA... no comparison possible!')
    out <- list(notes='Social contact matrix contains NA... no comparison possible!')
  } else if (is.complex(eigen(mija)$values) | is.complex(eigen(mijb)$values)){
    warning('Social contact matrix has a complex eigenvalue, making it impossible to estimate the R0 ratio, relative incidence, or conduct the NGA')
    out <- list(notes='Social contact matrix has a complex eigenvalue, making it impossible to estimate the R0 ratio, relative incidence, or conduct the NGA')
  } else {
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

#TODO: default value 1 is error prone!
parse_input_list <- function(input_list,column_tag,max_dept=NA){
  
  # get column names
  # note: 'sort' is required since they are aggregated consecutively in the following code block
  sel_colnames <- sort(unlist(names(input_list)[grepl(column_tag,names(input_list))]))

  if(!is.na(max_dept) & max_dept < length(sel_colnames)){
    sel_colnames <- sel_colnames[1:max_dept]
  }
  
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
                                  weight_threshold,
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
                                                  weight_threshold,
                                                  wave = wave))
  }
  
  # add location names
  names(matrix_list) <- cnt_location
  
  return(matrix_list)
}

# generate a dummy plot for the UI with a warning message
get_dummy_plot_for_ui <- function(warning_message){
    plot(0,col=0,axes=F,xlab='',ylab='')
    text(1,0,warning_message)    
}

# print notes to terminal
print_notes <- function(notes_vector,txt_width){
  cat("$notes:",fill = TRUE)
  for(i_note in 1:length(notes_vector)){
    cat(paste0('[',i_note,'] ',paste(strwrap(notes_vector[i_note], width = txt_width), collapse = "\n")),fill = TRUE)
  }
    cat(fill = TRUE)
}
