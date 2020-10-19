#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => ADAPTED contact_matrix() function
#
#  Copyright 2020, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________

# load adapted socialmirx::contact_matrix function
source('R_socialmixr/check.r')
source('R_socialmixr/contact_matrix.r')
source('R_socialmixr/survey.r')
source('R_socialmixr/wpp_age.r')

# temporary to use get_survey outside the SocialMixr package
library(httr)
library(jsonlite)
library(XML)
library(curl)
library(wpp2019)
source('R_socialmixr/get_survey.r')



