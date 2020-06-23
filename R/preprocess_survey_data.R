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

# get dataset from ZENODO and save as RDS
i <- 10
for(i in 1:nrow(survey_meta_data)){
  
  # load data
  survey_data <- get_survey(survey_meta_data$url[i])
  
  if(survey_meta_data$country[i] == 'france'){

    # note: one participant could have participated 2x in wave 1 and 2x in wave 2
    # ==>> and can contain "surveyDay == 1" twice

    # create diary id
    survey_data$contacts[,diary_id:= paste(part_id,wave,studyDay,sep='_') ]
    survey_data$participants[,diary_id:= paste(part_id,wave,studyDay,sep='_') ]
    
    # Conservative approach: select one diary per participant
    table(table(survey_data$participants$part_id))
    part_data              <- survey_data$participants
    part_data              <- part_data[order(part_id),]
    part_data              <- part_data[!duplicated(part_id), ]
    table(table(part_data$part_id))
    
    # subset data
    survey_data$participants     <- survey_data$participants[diary_id %in% part_data$diary_id,]
    survey_data$contacts         <- survey_data$contacts[diary_id %in% part_data$diary_id,]

  }

  if(survey_meta_data$country[i] == 'zimbabwe'){

    # select first survey day
    survey_data$participants     <- survey_data$participants[survey_data$participants$studyDay == 1,]
    survey_data$contacts         <- survey_data$contacts[survey_data$contacts$studyDay == 1,]
  }
  
  # save as .rds file
  saveRDS(survey_data, file=paste0('data/survey_',survey_meta_data$country[i],'.rds'))
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




