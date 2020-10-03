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


