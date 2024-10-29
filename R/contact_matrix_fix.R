#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => ADAPTED contact_matrix() function
#
#  Copyright 2024, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________

# temporary to use get_survey outside the SocialMixr package
library(httr)
library(jsonlite)
library(XML)
library(curl)
library(wpp2019)
library(countrycode)

# load adapted socialmixr::contact_matrix function
source('R_socialmixr/check.r')
source('R_socialmixr/contact_matrix.r')
source('R_socialmixr/survey.r')
source('R_socialmixr/wpp_age.r')
source('R_socialmixr/get_survey.r')
source('R_socialmixr/load_survey.r')
source('R_socialmixr/clean.r')





