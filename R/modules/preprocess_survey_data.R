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
#survey_meta_data <- list_surveys()
#saveRDS(survey_meta_data,file='data/survey_meta_data.rds')
survey_meta_data <- readRDS('data/survey_meta_data.rds')

# add colum for country
survey_meta_data[grepl('Peru',title),country:='peru']
survey_meta_data[grepl('Hong Kong',title),country:='hong_kong']
survey_meta_data[grepl('Russia',title),country:='russia']
survey_meta_data[grepl('Vietnam',title),country:='vietnam']
survey_meta_data[grepl('POLYMOD',title),country:='polymod']
survey_meta_data[grepl('CODA',title),country:='zambia_south_africa']
survey_meta_data[grepl('China',title),country:='china']
survey_meta_data[grepl('Zimbabwe',title),country:='zimbabwe']
survey_meta_data[grepl('France',title),country:='france']
survey_meta_data[grepl('UK',title),country:='uk']
survey_meta_data[grepl('Belgium',title),country:='belgium']

# Remove data for France (Béraud et al 2015)
# ==>> this is handled in a separate script
survey_meta_data[survey_meta_data$creator != 'Guillaume Béraud',]

# get dataset from ZENODO and save as RDS
i <- 10
for(i in 1:nrow(survey_meta_data)){
  
  # load data
  survey_data <- get_survey(survey_meta_data$url[i])
  
  if(survey_meta_data$country[i] == 'zimbabwe'){

    # select first survey day
    survey_data$participants     <- survey_data$participants[survey_data$participants$studyDay == 1,]
    survey_data$contacts         <- survey_data$contacts[survey_data$contacts$studyDay == 1,]
  }
  
  # save as .rds file
  saveRDS(survey_data, file=paste0('data/survey_',survey_meta_data$country[i],'.rds'))
}





