


##########################
##  DATA AND METHODS    ##
##########################

#install.packages('socialmixr')

# load 'socialmixr' package
#suppressPackageStartupMessages(library('socialmixr'))
library('socialmixr')

#contact_matrix(polymod, countries = "United Kingdom", age.limits = c(0, 1, 5, 15))
#list_surveys()

# set all options
# note: the first is the default
opt_gender   <- list("all","female","male")
opt_day_type <- list("all days", "weekday","weekend")
opt_period   <- list("regular and holiday","holiday period","regular period") 
#opt_location <- list("all locations","home", "work", "school","leisure","transport","other","multiple","missing")
opt_touch    <- list("all contacts", "physical contacts","non-physical contacts")
opt_duration <- list("all contacts","less than 5 minutes", "less than 15 minutes","more than 15 minutes","more than 1 hour","more than 4 hours")
opt_country  <- as.list(levels(unique(polymod$participants$country)))

# make named lists
names(opt_gender)   <- unlist(opt_gender)
names(opt_day_type) <- unlist(opt_day_type)
names(opt_period)   <- unlist(opt_period)
#names(opt_location) <- unlist(opt_location)
names(opt_touch)    <- unlist(opt_touch)
names(opt_duration) <- unlist(opt_duration)
names(opt_country)  <- unlist(opt_country)

#?contact_matrix(polymod, countries = "United Kingdom", age.limits = c(0, 1, 5, 15))

country      <- opt_country[[1]]
sel_weekday  <- opt_day_type[[2]]
sel_touch    <- opt_touch[[2]]
sel_duration <- opt_duration[[4]]
cnt_home <- TRUE
get_survey_object <- function(country,sel_weekday,sel_touch,sel_duration,
                              cnt_home,cnt_school,cnt_work,cnt_other,cnt_unknown){
  
  # get original polymod data
  data_part <- polymod$participants
  data_cnt  <- polymod$contacts
  
  #select country
  print(country)
  bool_country <- (data_part$country == country)
  data_part    <- data_part[bool_country,]
  
  # select type of day
  bool_dayofweek <- data_part$dayofweek >= 0 # all
  if(sel_weekday == opt_day_type[[2]]){ # weekday
    bool_dayofweek <- data_part$dayofweek %in% 1:5
    data_part      <- data_part[bool_dayofweek,]
    print(opt_day_type[[2]])
  }
  if(sel_weekday == opt_day_type[[3]]){ # weekend
    bool_dayofweek <- data_part$dayofweek %in% c(0,6)
    data_part      <- data_part[bool_dayofweek,]
    print(opt_day_type[[3]])
  }
  
  # # select duration
  # if(sel_duration != opt_duration[[1]]){
  #   duration_code <- which(opt_duration == sel_duration)-1
  #   bool_duration <- !is.na(data_cnt$duration_multi) & data_cnt$duration_multi == duration_code
  #   data_cnt      <- data_cnt[bool_duration,]
  #   print(sel_duration)
  # }
  if(sel_duration != opt_duration[[1]]){
    print(sel_duration)
  
    duration_code <- which(opt_duration == sel_duration)-1
    
    if(sel_duration %in% names(opt_duration[2:3]) ){
      bool_duration <- !is.na(data_cnt$duration_multi)  & data_cnt$duration_multi <= duration_code
      data_cnt      <- data_cnt[bool_duration,]
    } else{
      bool_duration <- !is.na(data_cnt$duration_multi)  & data_cnt$duration_multi >= duration_code
      data_cnt      <- data_cnt[bool_duration,]
    }
  }
  
  # select touching
  if(sel_touch != opt_touch[[1]]){
    touch_code    <- which(opt_touch == sel_touch)-1
    bool_touching <- !is.na(data_cnt$phys_contact) & data_cnt$phys_contact == touch_code
    data_cnt      <- data_cnt[bool_touching,]
    print(sel_touch)
  }
  
  # contact location
  # add temporary category for 'other locations'
  data_cnt$cnt_other <- data_cnt$cnt_transport == 1 |
                        data_cnt$cnt_leisure == 1 |
                        data_cnt$cnt_otherplace == 1
  
  # add temporary category for 'location unknown'
  data_cnt$loc_unknown <- data_cnt$cnt_home == 0 &
                          data_cnt$cnt_school == 0 &
                          data_cnt$cnt_work == 0 &
                          data_cnt$cnt_other == 0 
    
  # select contact location
  bool_cnt <- data_cnt$cnt_home == -1
  if(cnt_home)   { bool_cnt <- bool_cnt | data_cnt$cnt_home}
  if(cnt_school) { bool_cnt <- bool_cnt | data_cnt$cnt_school}
  if(cnt_work)   { bool_cnt <- bool_cnt | data_cnt$cnt_work}
  if(cnt_other)  { bool_cnt <- bool_cnt | data_cnt$cnt_other}
  if(cnt_unknown){ bool_cnt <- bool_cnt | data_cnt$loc_unknown}
  data_cnt <- data_cnt[bool_cnt,]
  
  
  
  # create new survey object
  mixr_survey <- survey(data_part, data_cnt)
  
  # return
  return(mixr_survey)
  
}


