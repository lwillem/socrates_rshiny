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

library(shiny)
library(data.table)
library(httr)
library(jsonlite)
library(XML)
library(curl)
library('readtext')
source('R/socrates_main.R')

## BELGIUM 2010-2011

# data folder
data_folder <- '../socrates_covid/data/flanders2010/'

# load data
part_data <- read.table(file.path(data_folder,"individualsurvey_participants_full.txt"), sep=",",header = TRUE)
cnt_data  <- read.table(file.path(data_folder,"individualsurvey_contacts.txt"), sep=",",header = TRUE)

# load meta data
meta_data <- readtext(file.path(data_folder,"metadata.txt"))

# adapt meta data and add survey object details
meta_data <- meta_data[[2]]
meta_data <- unlist(strsplit(meta_data,'\n'))
meta_data <- meta_data[grepl('VERSION',meta_data) | grepl('UPDATE',meta_data)]
reference <- list(author = 'Hoang, V.T. and Coletti, P. and Kifle, Y.W., Van Kerckhove, K. and Vercruyse, S. and Willem, L. and Beutels, P. and Hens, N.',
                  year   = '2019',
                  title  = 'The Flemish social contact survey',
                  journal = 'TBA',
                  bibtype = 'Article',
                  note_dataset        = tolower(paste('SURVEY DATA',paste(meta_data,collapse=' with '))),
                  note_survey_object  = tolower(paste('SURVEY OBJECT GENERATED ON',Sys.time())))


# add school and kindergarden contacts
cnt_data$cnt_school <- as.numeric(cnt_data$cnt_school == 1 | cnt_data$cnt_kindergarden)
cnt_data$cnt_kindergarden <- NULL

# reformat holiday variable
part_data$holiday   <- (part_data$holiday == 'Y')
part_data$weekday   <- (part_data$weekday == 'Y')
cnt_data$is_imputed <- (cnt_data$is_imputed == 'Y')


names(part_data)
# get socialmixr 'participants' object
db_participants   <- data.frame(part_id     = part_data$local_id,
                                part_age    = part_data$participant_age,
                                part_gender = part_data$participant_gender,
                                country     = "Belgium",
                                day         = part_data$day,
                                month       = part_data$month,
                                year        = part_data$year,
                                dayofweek   = part_data$dayofweek,
                                holiday     = part_data$holiday,
                                weekday     = part_data$weekday,
                                stringsAsFactors = F)

# get socialmixr 'contacts' object
db_contacts       <- data.frame(part_id         = cnt_data$local_id,
                                cnt_age_exact   = as.integer(round(cnt_data$cnt_age_mean)),
                                cnt_age_est_min = as.integer(cnt_data$cnt_age_l),
                                cnt_age_est_max = as.integer(cnt_data$cnt_age_r),
                                cnt_data[,c("cnt_home","cnt_work","cnt_school",
                                            "cnt_transport","cnt_leisure","cnt_otherplace","is_imputed")])

# add contact info on intensity, duration, frequency and gender
db_contacts$phys_contact    <- ifelse(cnt_data$cnt_touch == 'Y',1,2)
db_contacts$frequency_multi <- cnt_data$cnt_frequency
db_contacts$duration_multi  <- cnt_data$cnt_duration
db_contacts$cnt_gender      <- cnt_data$cnt_sex
db_contacts$is_hh_member    <- cnt_data$cnt_hh_member == 'Y'

names(cnt_data)
names(db_contacts)
head(db_contacts)
names(db_participants)
head(db_participants)


# get socialmixr 'survey' object
survey_flanders2010 <- survey(participants = db_participants,
                              contacts     = db_contacts,
                              reference    = reference)

# store survey object
saveRDS(survey_flanders2010, file=paste0('data/survey_belgium2010.rds'))

cite(survey_flanders2010)

