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
survey_meta_data <- list_surveys()

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
                'vietnam','uk','zambia_south_africa','russia','china')

# get dataset from ZENODO and save as RDS
for(i in 1:nrow(survey_meta_data)){
  survey_data <- get_survey(survey_meta_data$url[i])
  saveRDS(survey_data, file=paste0('data/survey_',survey_opt[i],'.rds'))
}

# finished

