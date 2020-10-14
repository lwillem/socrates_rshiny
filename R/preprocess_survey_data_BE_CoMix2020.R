#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => PREPROCESS AND SAVE BE-COMIX-COVID19 SURVEY DATA
# 
#  Copyright 2020, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________

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

## BELGIUM 2020 CoMix

#library(socialmixr)
comix_output_dir <- '../survey_BE_2020_CoMix/output_dataset_zenodo_aggregated/'
survey_files <- dir(comix_output_dir,full.names = T,pattern = 'csv')
survey_be2020_comix <- get_survey(survey_files)  

#contact_matrix(survey_be2020_comix)

reference <- list(author = 'Coletti, P. and Wambua, J. and Gimma, A. and Willem, L. and Vercruyse, S. and Vanhoutte, B. and Jarvis, C.I. and van Zandvoort, K. and Edmunds, J. and Beutels, P. and Hens, N.',
                  year   = '2020',
                  title  = 'CoMix: comparing mixing patterns in the Belgian population during and after lockdown',
                  journal = 'TBA',
                  bibtype = 'Article',
                  note_survey_object  = tolower(paste('SURVEY OBJECT GENERATED ON',Sys.time())))

# get socialmixr 'survey' object
survey_be2020_comix$reference    = reference

# store survey object
saveRDS(survey_be2020_comix, file=paste0('data/survey_belgium2020_comix.rds'))

cite(survey_be2020_comix)

lapply(survey_be2020_comix,dim)

set.seed(20180216)
contact_matrix(survey_be2020_comix,estimated.contact.age = "sample",age.limits = c(0,20,40))$matrix
?contact_matrix

#library(socialmixr)
comix_output_dir <- '../survey_BE_2020_CoMix/output_dataset_zenodo_aggregated/'
survey_files <- dir(comix_output_dir,full.names = T,pattern = 'csv')
survey_be2020_comix <- get_survey(survey_files)  

dim(survey_be2020_comix$participants)
dim(survey_be2020_comix$contacts)

table(survey_be2020_comix$participants$wave)
c_data <- merge(survey_be2020_comix$contacts,survey_be2020_comix$participants[,c('part_id','wave')],by='part_id')
table(c_data$wave)

table(c_data$wave)/table(survey_be2020_comix$participants$wave)

contact_matrix(survey_be2020_comix,missing.contact.age = 'remove',age.limits = c(0))
contact_matrix(survey_be2020_comix,missing.contact.age = 'sample',age.limits = c(0))
contact_matrix(survey_be2020_comix,missing.contact.age = 'keep',age.limits = c(0))
contact_matrix(survey_be2020_comix,missing.contact.age = 'ignore',age.limits = c(0))


survey_be2020_comix_w1 <- survey_be2020_comix
survey_be2020_comix_w1$participants <- survey_be2020_comix_w1$participants[survey_be2020_comix_w1$participants$wave == 1,]
contact_matrix(survey_be2020_comix_w1,missing.contact.age = 'remove',age.limits = c(0))
contact_matrix(survey_be2020_comix_w1,missing.contact.age = 'sample',age.limits = c(0))


survey_be2020_comix_w6 <- survey_be2020_comix
survey_be2020_comix_w6$participants <- survey_be2020_comix_w6$participants[survey_be2020_comix_w6$participants$wave == 6,]
contact_matrix(survey_be2020_comix_w6,missing.contact.age = 'remove',age.limits = c(0))
contact_matrix(survey_be2020_comix_w6,missing.contact.age = 'sample',age.limits = c(0))
contact_matrix(survey_be2020_comix_w6,missing.contact.age = 'keep',age.limits = c(0))


