#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => PREPROCESS AND SAVE COUNTRY-SPECIFIC SURVEY DATA
# 
# Retrieving data from ZENODO is time consuming, so we make a local copy
#
#  Copyright 2020, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________
#TODO: add missing columns etc...

# clear workspace
rm(list=ls())

# load social mixr package
library(socialmixr)

# get list with all availabe datasets
# survey_meta_data <- list_surveys()
# saveRDS(survey_meta_data,file='data/survey_meta_data.rds')
survey_meta_data <- readRDS('data/survey_meta_data.rds')

# create tag: country + publiation
survey_meta_data$survey_name <- c('POLYMOD (Mossong 2007)',
                                  'Peru (Grijalva 2011)',
                                  'Zimbabwe (Melegaro 2013)',
                                  'France (Beraud 2012)',
                                  'Hong Kong (Lyung 2015)',
                                  'Vietnam (Horby 2007)',
                                  'United Kingdom (van Hoek 2012)',
                                  'Zambia & South Africa (Dodd 2011)',
                                  'Russia (Litvinova 2019)',
                                  'China (Zhang 2019)')
# list all datasets
survey_opt <- c("polymod",'peru','zimbabwe','france','hong_kong',
                'vietnam','uk','zambia_south_africa','russia','china',
                'zambia','south_africa')

# duplicate the zambia & South Africa reference... to split them
survey_meta_data <- rbind(survey_meta_data,survey_meta_data[8,],survey_meta_data[8,])
  
# get dataset from ZENODO and save as RDS
i <- 3
for(i in 7:nrow(survey_meta_data)){
  survey_data <- get_survey(survey_meta_data$url[i])
  
  # head(survey_data$participants)
  # table(survey_data$participants$country)
  # head(survey_data$contacts)
  # table(survey_data$contacts$cnt_age_est_max)
  # table(survey_data$contacts$cnt_age_est_min)
  # table(survey_data$contacts$cnt_age_exact)
  # typeof(survey_data$contacts$cnt_age_est_max)
  # typeof(survey_data$contacts$cnt_age_est_min)
  # typeof(survey_data$contacts$cnt_age_exact)
  # 
  
  if(survey_opt[i] == 'france'){
    survey_data$participants
    # select first day
    bool_day_one <- survey_data$contacts$sday_part_number == 1
    survey_data$contacts <- survey_data$contacts[bool_day_one,]
    
    # select his/her first wave
    part_id_wave_1 <- unique(survey_data$contacts$part_id[survey_data$contacts$wave == 1])
    part_id_wave_2 <- unique(survey_data$contacts$part_id[survey_data$contacts$wave == 2])
    exclude_wave_2 <- part_id_wave_2[part_id_wave_2 %in% part_id_wave_1]
    bool_two_waves <- survey_data$contacts$wave == 2 & survey_data$contacts$part_id %in% exclude_wave_2
    survey_data$contacts <- survey_data$contacts[!bool_two_waves,]
    table(bool_two_waves)
    
    # convert holiday variable into boolean
    survey_data$contacts$holiday <- survey_data$contacts$holiday == 1

    part_date <- unique(survey_data$contacts[,c('part_id','day','month','year','dayofweek','holiday')])
    survey_data$participants <- merge(survey_data$participants,part_date,by='part_id')
    
    table(survey_data$contacts$cnt_school,survey_data$contacts$wave)
    table(survey_data$contacts$cnt_school)
    table(survey_data$contacts$cnt_transport)
    cnt_tmp <- survey_data$contacts$cnt_school
    survey_data$contacts$cnt_school <- survey_data$contacts$cnt_transport
    survey_data$contacts$cnt_transport <- cnt_tmp
    
    }
  
  if(survey_opt[i] == 'zambia_south_africa'){
    typeof(survey_data$contacts$cnt_age_exact)
    table(is.na(survey_data$contacts$cnt_age_exact))
    survey_data$contacts$cnt_age_exact <- NULL
    survey_data$contacts$cnt_age_exact <- NA_integer_
  }
  
  if(survey_opt[i] == 'uk'){
   next
  }
  
  if(survey_opt[i] == 'russia'){
    table(survey_data$contacts$cnt_age_est_max)
    table(survey_data$contacts$cnt_age_est_min)
    table(survey_data$contacts$cnt_age_exact)
    
    cnt_age_est_min <- gsub('-','',survey_data$contacts$cnt_age_est_min)
    cnt_age_est_min <- as.integer(cnt_age_est_min)
    survey_data$contacts$cnt_age_est_min <- NULL
    survey_data$contacts$cnt_age_est_min <- cnt_age_est_min
  
    survey_data$participants$year <- 2016
    #TODO: holiday
    
  }
  
  if(survey_opt[i] == 'vietnam'){
    
    # include year (from paper)
    survey_data$participants$year <- 2007
    
    # other date info: unknown
    survey_data$participants$day       <- NA
    survey_data$participants$month     <- NA
    survey_data$participants$dayofweek <- NA
    
    # transform age group into age min and max
    levels(survey_data$contacts$cnt_age_group)
    cnt_age_group_char <-  as.character(survey_data$contacts$cnt_age_group)
    cnt_age_group_char[cnt_age_group_char == '65+'] <- '65-90' # arbitrary!!
    age_min_max <- strsplit(cnt_age_group_char,"-")
    age_min_max <- matrix(unlist(age_min_max),ncol=2,byrow = T)
    typeof(survey_data$contacts$cnt_age_est_min)
    survey_data$contacts$cnt_age_est_min <- as.integer(age_min_max[,1])
    survey_data$contacts$cnt_age_est_max <- as.integer(age_min_max[,2])
    survey_data$contacts$cnt_age_exact <- NULL
    survey_data$contacts$cnt_age_exact <- NA_integer_
  }
  
  if(survey_opt[i] == 'china'){
    survey_data$participants$country <- 'China'
    survey_data$contacts$cnt_otherplace <- survey_data$contacts$cnt_other
    survey_data$contacts$cnt_age_exact  <- as.integer(floor(survey_data$contacts$cnt_age_exact))
    survey_data$contacts$cnt_leisure <- NULL
    survey_data$contacts$cnt_leisure <- 0
    head(survey_data$contacts)
  }
  
  if(survey_opt[i] == 'zimbabwe'){
    
    # include year (from paper)
    survey_data$participants$year <- 2013
    dim(survey_data$participants)
    dim(survey_data$contacts)
    names(survey_data$contacts)  
    
    names(survey_data$participants)
    
    
  }
  
  saveRDS(survey_data, file=paste0('data/survey_',survey_opt[i],'.rds'))
}


# finished
library(data.table)
library(httr)
library(jsonlite)
library(XML)
library(curl)
source('R/socrates_main.R')

data_dir  <- '../socrates_covid/data/datasets_full/'
data_dir  <- '../socrates_covid/data/datasets_28_Feb/'
dir(data_dir)
survey_opt <- dir(data_dir)
i <- 12
for(i in 1:length(survey_opt)){
  survey_data <- get_survey(survey = dir(file.path(data_dir,survey_opt[i]),pattern = '.csv',full.names = T),quiet = T)
  
  
  if(tolower(survey_opt[i]) == 'zimbabwe'){
    names(survey_data$contacts)
    #bool_day_one <- survey_data$contacts$sday_part_number == 1
    bool_day_one <- survey_data$contacts$sday_part_id == 1
    table(bool_day_one)
    survey_data$contacts <- survey_data$contacts[bool_day_one,]
    
    survey_data$participants
    part_date <- unique(survey_data$contacts[,c('part_id','day','month','year','dayofweek')])
    survey_data$participants <- merge(survey_data$participants,part_date,by='part_id')
    
    
   }
  
  if(tolower(survey_opt[i]) == 'china'){
    # include year (from paper)
    names(survey_data$contacts)
    survey_data$contacts$cnt_otherplace <- survey_data$contacts$cnt_otherplace | survey_data$contacts$cnt_otherpublicplace
    survey_data$contacts$cnt_otherpublicplace <- NULL
  }
  
  if(survey_opt[i] == 'france'){
    survey_data$participants
    bool_day_one <- survey_data$contacts$sday_part_number == 1
    survey_data$contacts <- survey_data$contacts[bool_day_one,]
    
    survey_data$participants
    part_date <- unique(survey_data$contacts[,c('part_id','day','month','year','dayofweek')])
    survey_data$participants <- merge(survey_data$participants,part_date,by='part_id')
    
    table(survey_data$contacts$cnt_school)
    table(survey_data$contacts$cnt_transport)
    
    
  }
  
  
  saveRDS(survey_data, file=paste0('data/survey_',tolower(survey_opt[i]),'.rds'))
}


range(survey_data$participants$part_age,na.rm=T)

data_fr <- read.table('../socrates_covid/data/datasets_full/France/ComesF_contacts.txt',sep=';',header=T)
table(data_fr$cnt_school)
table(data_fr$cnt_transport)
