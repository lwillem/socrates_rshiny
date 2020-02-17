#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => LOAD AND SELECT SOCIAL CONTACT SURVEY DATA
#
#  Copyright 2020, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________

# load packages and help functions
source('R/load_config.R')
source('R/contact_matrix_fix.R')
source('R/plot_social_contact_matrix.R')

# example
#contact_matrix(polymod, countries = "United Kingdom", age.limits = c(0, 1, 5, 15))

run_social_contact_analysis <- function(country,daytype,touch,duration,gender,
                                        cnt_location,symmetric,age_breaks_text,
                                        bool_schools_closed,telework_reference,telework_target){
  
  # get social contact matrix, using all features
  cnt_matrix_ui <- get_contact_matrix(country,daytype,touch,duration,gender,
                                      cnt_location,symmetric,age_breaks_text,
                                      bool_schools_closed,
                                      bool_exclusive = FALSE)
  
  # CLI
  fct_out <- cnt_matrix_ui
  
  # include telework features?
  bool_telework <- telework_target > telework_reference
  
  if(bool_schools_closed | bool_telework){
    
    # get reference social contact matrix (no intervention)
    cnt_matrix_ref <- get_contact_matrix(country,daytype,touch,duration,gender,
                                         cnt_location,
                                         symmetric,
                                         age_breaks_text,
                                         bool_schools_closed = FALSE,
                                         bool_exclusive = FALSE)
    
    if(bool_telework){
      
      # get contact matrix with work-contacts (exclusive)
      if('Work' %in% cnt_location){
        cnt_matrix_work_excl <- get_contact_matrix(country,daytype,touch,duration,gender,
                                                   cnt_location = "Work",
                                                   symmetric,age_breaks_text,
                                                   bool_schools_closed = FALSE,
                                                   bool_exclusive = FALSE)$matrix
      } else{
        cnt_matrix_work_excl <- cnt_matrix_ui$matrix * 0
      }
      
      # apply reduction
      telework_increase  <- telework_target/100 - telework_reference/100
      telework_reduction <- telework_increase / (1-telework_reference/100)
      cnt_matrix_work_reduction <- cnt_matrix_work_excl * telework_reduction
      
      # calculate final matrix
      cnt_matrix_ui$matrix <- cnt_matrix_ui$matrix - cnt_matrix_work_reduction
    }
    
    model_comparison <- compare_contact_matrices(cnt_matrix_ui$matrix,cnt_matrix_ref$matrix)
    #print(model_comparison)
    
    fct_out <- c(cnt_matrix_ui,model_comparison)
    
    # add note(s)
    fct_out <- c(fct_out,notes="ratio = with intervention / without intervention")
    if(bool_schools_closed) { 
      fct_out$notes <- rbind(fct_out$notes,"All schools are closed")
      }
    if(bool_telework) {   
      fct_out$notes <- rbind(fct_out$notes, paste0("Increased telework (",
                                                  telework_target,'% instead of ',
                                                  telework_reference,'%)'))
    }
  }
   return(fct_out)
}

## MAIN FUNCTION ####
get_contact_matrix <- function(country,daytype,touch,duration,gender,
                               cnt_location,symmetric,age_breaks_text,
                               bool_schools_closed,bool_exclusive){
  
  # REACTIVE STRATEGY: SCHOOL CLOSURE
  if(bool_schools_closed){
    cnt_location <- cnt_location[!grepl('School',cnt_location)]
  }
  
  # set age intervals
  age_breaks_num <- as.numeric(unlist(strsplit(age_breaks_text,",")))
  
  # make sure the ages are increasing 
  age_breaks_num <- sort(age_breaks_num)
  
  # get specific social_mixr survey object
  survey_object <- get_survey_object(country      = country,
                                     daytype      = daytype,
                                     touch        = touch,
                                     duration     = duration,
                                     gender       = gender,
                                     cnt_location = cnt_location,
                                     bool_exclusive   = bool_exclusive)  # remove contacts at multiple loations
  # run social_mixr function
  matrix_out <- contact_matrix(survey     = survey_object, 
                                age.limits = age_breaks_num,
                                symmetric  = symmetric,
                                quiet      = TRUE)
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
                              bool_exclusive){
  
  # get original polymod data
  data_part <- polymod$participants
  data_cnt  <- polymod$contacts
  
  # select country: participant and contact data
  bool_country <- (data_part$country == country)
  data_part    <- data_part[bool_country,]
  data_cnt     <- data_cnt[data_cnt$part_id %in% data_part$part_id,]
  
  # select type of day
  if(daytype != opt_day_type[[1]]){
    bool_dayofweek <- data_part$dayofweek >= 0 # all
    if(daytype == opt_day_type[[3]]){ # weekend
      bool_dayofweek <- data_part$dayofweek %in% c(0,6)
      data_part      <- data_part[bool_dayofweek,]
    } else{
      bool_dayofweek <- data_part$dayofweek %in% 1:5
      data_part      <- data_part[bool_dayofweek,]
    }
  }
  
  # select period
  if(daytype %in% names(opt_day_type[4:5])){
    load('data/holiday_all.RData')
    country_iso3 <- countrycode(unlist(country), 'country.name', 'iso3c')
    country_holiday_data <- holiday_all[holiday_all$iso3 == country_iso3,]
    data_part$date <- as.Date(paste(data_part$day,
                                    data_part$month,
                                    data_part$year,
                                    sep='/')
                              ,'%d/%m/%Y')
    data_part$is_holiday <- data_part$date %in% country_holiday_data$date

    if(daytype == opt_day_type[[4]]){
      if(!any(data_part$is_holiday)){ # if no holiday period data
        print("NO HOLIDAY DATA... USE REGULAR PERIOD DATA")
      } else{ # select holiday period data
        data_part <- data_part[data_part$is_holiday,]
      }
    } else{ # select regular period data
      data_part <- data_part[!data_part$is_holiday,]
    }
  }
  
  # select contact duration
  if(duration != opt_duration[[1]]){
    print(duration)
  
    duration_code <- which(opt_duration == duration)-1
    
    if(duration %in% names(opt_duration[2:3]) ){
      bool_duration <- !is.na(data_cnt$duration_multi)  & data_cnt$duration_multi <= duration_code
      data_cnt      <- data_cnt[bool_duration,]
    } else{
      bool_duration <- !is.na(data_cnt$duration_multi)  & data_cnt$duration_multi >= duration_code
      data_cnt      <- data_cnt[bool_duration,]
    }
  }
  
  # select touching
  if(touch != opt_touch[[1]]){
    touch_code    <- which(opt_touch == touch)-1
    bool_touching <- !is.na(data_cnt$phys_contact) & data_cnt$phys_contact == touch_code
    data_cnt      <- data_cnt[bool_touching,]
    print(touch)
  }
  
  # select gender
  if(gender != opt_gender[[1]]){
    bool_cnt_female  <- data_cnt$cnt_gender   == 'F'
    bool_part_female <- data_part$part_gender == 'F'
    if(gender == opt_gender[[2]]){         # female-female
      data_cnt       <- data_cnt[bool_cnt_female,]
      data_part      <- data_part[bool_part_female,]
    } else if(gender == opt_gender[[3]]){  # male-male
      data_cnt       <- data_cnt[!bool_cnt_female,]
      data_part      <- data_part[!bool_part_female,]
    } else {                               # female-male
      
      data_cnt_gender  <- merge(data_cnt,
                                data_part[,c('part_id','part_gender')],
                                by='part_id')
      bool_gender_diff <- data_cnt_gender$cnt_gender != data_cnt_gender$part_gender
      data_cnt         <- data_cnt[bool_gender_diff,]
     }
  }
  
  #select location
  if(length(cnt_location)==0){
    print("WARNING: NO LOCATIONS SPECIFIED...")
    data_cnt <- data_cnt[0,]
  } else if(!identical(cnt_location,opt_location)){
    
    # fix: change data.table to data.frame
    data_cnt_tmp <- data.frame(data_cnt)
    
    # get column names
    cnt_location_colnames <- c(paste0('cnt_',tolower(cnt_location)))
    
    # select columns
    if(length(cnt_location)>1){
      is_present <- rowSums(data_cnt_tmp[,cnt_location_colnames] == 1)
    } else{
      is_present <- data_cnt_tmp[,cnt_location_colnames]
    }
      
    # select contact at specified location(s) 
    bool_location <- is_present > 0
    
    # option to select only exclusive contacts
    if(bool_exclusive){
      bool_exclusive <- rowSums(data_cnt_tmp[,c(paste0('cnt_',tolower(opt_location)))]) == 1
      bool_location  <- bool_location & bool_exclusive
    }
    
    # select
    data_cnt <- data_cnt[bool_location,]
    
    # add warning
    if(!any(bool_location)){
      print("WARNING: NO LOCATIONS LEFT AFTER LOCATION SELECTION...")
    } 
  }
      
  # create new survey object
  mixr_survey <- survey(data_part, data_cnt)
  
  # return
  return(mixr_survey)
  
}


#mija <- contact_matrix(polymod, countries = "Belgium", age.limits = c(0, 18, 45,65))$matrix*c(1,0.5,0.6,1)
#mijb <- contact_matrix(polymod, countries = "Belgium", age.limits = c(0, 18, 45, 65))$matrix
compare_contact_matrices <- function(mija,mijb){
  R0_ratio      <- max(eigen(mija)$values)/max(eigen(mijb)$values)
  mij_ratio     <- mija/mijb
  
  # relative incidence 
  RIa             <- standardize_RI(eigen(mija)$vectors[,1])
  RIb             <- standardize_RI(eigen(mijb)$vectors[,1])
  RI_ratio        <- RIa/RIb
  names(RI_ratio) <- colnames(mija)
  
  #output 
  out <- list(R0_ratio=R0_ratio,mij_ratio=mij_ratio,RI_ratio=RI_ratio)
  
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
  
  return(out)
}

# help function to standardize the relative incidence
standardize_RI<-function(vec){return(vec/sum(vec))}
