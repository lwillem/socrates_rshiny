#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => LOAD AND SELECT SOCIAL CONTACT SURVEY DATA
#
#  Copyright 2020, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________

# install.packages('socialmixr')

# load 'socialmixr' package
#suppressPackageStartupMessages(library('socialmixr'))
library('socialmixr')
# example
#contact_matrix(polymod, countries = "United Kingdom", age.limits = c(0, 1, 5, 15))

library('countrycode')
source('R/contact_matrix_fix.R')
source('R/plot_social_contact_matrix.R')

## MAIN FUNCTION ####
get_contact_matrix <- function(country,daytype,period,touch,duration,
                               cnt_home,cnt_school,cnt_work,cnt_other,cnt_unknown,
                               symmetric,age_breaks_text,
                               bool_schools_closed,bool_exclusive){
  
  # REACTIVE STRATEGY: SCHOOL CLOSURE
  if(bool_schools_closed){
    cnt_school <- FALSE
  }
  
  # set age intervals
  age_breaks_num <- as.numeric(unlist(strsplit(age_breaks_text,",")))
  
  # make sure the ages are increasing 
  age_breaks_num <- sort(age_breaks_num)
  
  # get specific social_mixr survey object
  survey_object <- get_survey_object(country      = country,
                                     daytype      = daytype,
                                     period       = period,
                                     touch        = touch,
                                     duration     = duration,
                                     cnt_home     = cnt_home,
                                     cnt_school   = cnt_school,
                                     cnt_work     = cnt_work,
                                     cnt_other    = cnt_other,
                                     cnt_unknown  = cnt_unknown,
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
get_survey_object <- function(country,daytype,period,touch,duration,
                              cnt_home,cnt_school,cnt_work,cnt_other,cnt_unknown,
                              bool_exclusive){
  
  # get original polymod data
  data_part <- polymod$participants
  data_cnt  <- polymod$contacts
  
  # select country
  print(country)
  bool_country <- (data_part$country == country)
  data_part    <- data_part[bool_country,]
  
  # select type of day
  bool_dayofweek <- data_part$dayofweek >= 0 # all
  if(daytype == opt_day_type[[2]]){ # weekday
    bool_dayofweek <- data_part$dayofweek %in% 1:5
    data_part      <- data_part[bool_dayofweek,]
    print(opt_day_type[[2]])
  }
  if(daytype == opt_day_type[[3]]){ # weekend
    bool_dayofweek <- data_part$dayofweek %in% c(0,6)
    data_part      <- data_part[bool_dayofweek,]
    print(opt_day_type[[3]])
  }
  
  # select period
  if(period != opt_period[[1]]){
    load('data/holiday_all.RData')
    country_iso3 <- countrycode(unlist(country), 'country.name', 'iso3c')
    country_holiday_data <- holiday_all[holiday_all$iso3 == country_iso3,]
    data_part$date <- as.Date(paste(data_part$day,
                                    data_part$month,
                                    data_part$year,
                                    sep='/')
                              ,'%d/%m/%Y')
    data_part$is_holiday <- data_part$date %in% country_holiday_data$date

    if(period == opt_period[[2]]){
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
  
  # create temporary category for "other"
  bool_cnt_other <- data_cnt$cnt_transport  == 1 |
                data_cnt$cnt_leisure    == 1 |
                data_cnt$cnt_otherplace == 1
  
  # select contact location
  bool_cnt    <- rep(FALSE,nrow(data_cnt))
  if(cnt_home)   { bool_cnt <- bool_cnt | data_cnt$cnt_home}
  if(cnt_school) { bool_cnt <- bool_cnt | data_cnt$cnt_school}
  if(cnt_work)   { bool_cnt <- bool_cnt | data_cnt$cnt_work}
  if(cnt_other)  { bool_cnt <- bool_cnt | bool_cnt_other  }
  if(cnt_unknown){ 
    # create temporary category for 'location unknown'
    loc_unknown <- data_cnt$cnt_home   == 0 &
                   data_cnt$cnt_school == 0 &
                   data_cnt$cnt_work   == 0 &
                   data_cnt$cnt_other  == 0 
    bool_cnt    <- bool_cnt | loc_unknown
  }
  
  if(bool_exclusive){
    loc_single <- ((data_cnt$cnt_home   == 1) +
                     (data_cnt$cnt_school == 1) +
                     (data_cnt$cnt_work   == 1) +
                     (data_cnt$cnt_other  == 1) ) <= 1
    bool_cnt   <- bool_cnt & loc_single
  }
  
  # select... if any contact left
  if(any(bool_cnt)){
    data_cnt <- data_cnt[bool_cnt,]
  } else{
    print("NO LOCATION BASED SELECTION POSSIBLE... NO CONTACTS LEFT")
  }
  
  # create new survey object
  mixr_survey <- survey(data_part, data_cnt)
  
  # return
  return(mixr_survey)
  
}


# mija <- contact_matrix(polymod, countries = "United Kingdom", age.limits = c(0, 1, 5, 15))$matrix
# mijb <- contact_matrix(polymod, countries = "Belgium", age.limits = c(0, 1, 5, 15))$matrix

compare_contact_matrices <- function(mija,mijb){
  R0_ratio      <- max(eigen(mija)$values)/max(eigen(mijb)$values)
  mij_ratio     <- mija/mijb
  RIa           <- standardize_RI(eigen(mija)$vectors[,1])
  RIb           <- standardize_RI(eigen(mijb)$vectors[,1])
  RI_ratio      <- RIa/RIb
  return(list(R0_ratio=R0_ratio,mij_ratio=mij_ratio,RI_ratio=RI_ratio))
}

# help function to standardize the relative incidence
standardize_RI<-function(vec){return(vec/sum(vec))}
