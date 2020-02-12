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
library('countrycode')
source('R/contact_matrix_fix.R')

# example
#contact_matrix(polymod, countries = "United Kingdom", age.limits = c(0, 1, 5, 15))


get_survey_object <- function(country,daytype,period,touch,duration,
                              cnt_home,cnt_school,cnt_work,cnt_other,cnt_unknown){
  
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
  
  # select contact location
  bool_cnt <- data_cnt$cnt_home == -1
  if(cnt_home)   { bool_cnt <- bool_cnt | data_cnt$cnt_home}
  if(cnt_school) { bool_cnt <- bool_cnt | data_cnt$cnt_school}
  if(cnt_work)   { bool_cnt <- bool_cnt | data_cnt$cnt_work}
  if(cnt_other){ 
    cnt_other <- data_cnt$cnt_transport  == 1 |
                 data_cnt$cnt_leisure    == 1 |
                 data_cnt$cnt_otherplace == 1
    bool_cnt <- bool_cnt | data_cnt$cnt_other
    }
  if(cnt_unknown){ 
    # create temporary category for 'location unknown'
    loc_unknown <- data_cnt$cnt_home   == 0 &
                   data_cnt$cnt_school == 0 &
                   data_cnt$cnt_work   == 0 &
                   data_cnt$cnt_other  == 0 
    bool_cnt    <- bool_cnt | loc_unknown
    }
  data_cnt <- data_cnt[bool_cnt,]
  
  # create new survey object
  mixr_survey <- survey(data_part, data_cnt)
  
  # return
  return(mixr_survey)
  
}

mij <- contact_matrix(polymod, countries = "United Kingdom", age.limits = c(0, 1, 5, 15))$matrix
# plot_title <- 'dummy'
plot_cnt_matrix_raster <- function(mij)
{
  # set maximum (and apply to mij)
  z_max      <- 8
  mij[mij>z_max] <- z_max
  
  #setup ranges, colors and margins
  z_lim      <- c(0,z_max)
  num_breaks <- 9
  z_col      <- rev(RColorBrewer::brewer.pal(num_breaks,'YlOrRd'))
  par(mar=c(5,5,0,10))
  
  # plot matrix
  image(mij,
        zlim=z_lim,
        col=z_col,
        xaxt='n',yaxt='n',
        xlab='\nAge participant (years)',
        ylab='\nAge contact (years)\n')
  
  # add axis labels
  tick_step <- seq(0,1,length=nrow(mij))
  axis(1,tick_step,rownames(mij),tick = F)
  axis(2,tick_step,rownames(mij),tick = F,las=2)
  
  # add legend
  par(xpd=TRUE)
  legend_text <- paste(rev(round(seq(0,z_max,length=num_breaks))))
  legend_text[1] <- paste0(legend_text[1],'+')
  legend('right',
         inset=c(-0.2,0),
         legend_text,
         fill=rev(z_col),
         title="Mean number\nof contacts\nper day",
         ncol=1,
         box.lwd = 0,
         box.col = 0)
  par(xpd=FALSE)
}


