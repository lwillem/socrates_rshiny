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

## ZENODO DATA ----  

ind_data_zenodo <- c(1)

# get dataset from ZENODO and save as RDS
i <- 2
for(i in ind_data_zenodo){
  
  # load data
  survey_data <- get_survey(survey_meta_data$url[i])
  
  # save as .rds file
  saveRDS(survey_data, file=paste0('data/survey_',survey_opt[i],'.rds'))
}

## ADJUSTED DATA  ----
# note: to be updated on Zenodo

# finished
library(shiny)
library(data.table)
library(httr)
library(jsonlite)
library(XML)
library(curl)
source('R/socrates_main.R')

#data_dir  <- '../socrates_covid/data/datasets_full/'
#data_dir  <- '../socrates_covid/data/datasets_28_Feb/'
data_dir  <- '../socrates_covid/data/datasets_23_May/'

survey_opt <- dir(data_dir)                        # get file and directory names
survey_opt <- survey_opt[!grepl('\\.',survey_opt)] # remove file names (with an extension)

survey_opt <- survey_opt[!grepl('Belgium',survey_opt)] # remove Belgium2010 data (for now)
survey_opt <- survey_opt[!grepl('France',survey_opt)] # remove French data (for now)

survey_opt

i <-2
for(i in 1:length(survey_opt)){
  survey_data <- get_survey(survey = dir(file.path(data_dir,survey_opt[i]),pattern = '.csv',full.names = T),quiet = T)
  
  
  if(tolower(survey_opt[i]) == 'zimbabwe'){
    names(survey_data$contacts)
    bool_day_one <- grepl('-1',survey_data$contacts$diary_id)
    table(bool_day_one)
    survey_data$contacts <- survey_data$contacts[bool_day_one,]
    
    survey_data$participants
    bool_day_one <- grepl('-1',survey_data$participants$diary_id)
    table(bool_day_one)
    survey_data$participants<- survey_data$participants[bool_day_one,]
    
    part_date <- unique(survey_data$contacts[,c('part_id','day','month','year','dayofweek')])
    survey_data$participants <- merge(survey_data$participants,part_date,by='part_id')
    
    names(survey_data$contacts)
    summary(survey_data$contacts)
    
    lapply(survey_data,dim)
   }
  
 if(grepl('france',tolower(survey_opt[i]))){

    # Conservative approach: select one diary per participant
    part_data          <- survey_data$participants
    part_data          <- part_data[order(part_data$sday_id),]
    part_data          <- part_data[!duplicated(part_data$part_id), ]
    selection_diary_id <- part_data$diary_id

    # create subset for each data set
    survey_data$participants     <- survey_data$participants[survey_data$participants$diary_id %in% selection_diary_id,]
    survey_data$contacts         <- survey_data$contacts[survey_data$contacts$diary_id %in% selection_diary_id,]

    # convert holiday variable into boolean
    survey_data$participants$holiday <- survey_data$participants$holiday == 1
    
    # convert is_imputed variable into boolean
    survey_data$contacts$is_imputed <- survey_data$contacts$is_imputed == 'Y'
    
    # converst location columns into boolean
    col_names <- paste0('cnt_',tolower(opt_location))
    survey_data$contacts[, cnt_home:= cnt_home=="true",]
    survey_data$contacts[, cnt_work:= cnt_home=="true",]
    survey_data$contacts[, cnt_school:= cnt_home=="true",]
    survey_data$contacts[, cnt_transport:= cnt_transport=="true",]
    survey_data$contacts[, cnt_leisure:= cnt_leisure=="true",]
    survey_data$contacts[, cnt_otherplace:= cnt_otherplace=="true",]
    survey_data$contacts[,..col_names]  
    
  
  
  # store survey object
  saveRDS(survey_data, file=paste0('data/survey_',tolower(survey_opt[i]),'.rds'))
  
}


# ## COMPARE DATA SETS ----
# 
# data_new <- readRDS('data/survey_france.rds')
# data_prev <- readRDS('data/survey_france_spc.rds')
# 
# lapply(data_new,dim)
# lapply(data_prev,dim)
# 
# table(data_new$participants == data_prev$participants)
# table(data_new$contacts == data_prev$contacts)
# 
# summary(data_new$contacts)
# summary(data_prev$contacts)




