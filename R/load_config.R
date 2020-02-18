#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => DEFINE USER INTERFACE AND MODELLING OPTIONS 
#
#  Copyright 2020, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________

#__________________________#
##  R-PACKAGES          ####
#__________________________#
# socialmixr    to process social contact data
# npsp          to use 'simage' in plot_cnt_matrix
# countrycode   to convert country names into iso3 codes
# data.table    to adapt socialmixr::contact_matrix

# Explicit loading of the packages (fix for www.shinyapps.io)
library('socialmixr')
library('npsp')
library('countrycode')
library('data.table')

#__________________________#
##  UI PANEL OPTIONS    ####
#__________________________#

# set all options
# note: the first is the default
opt_gender   <- list("All","Female-Female","Male-Male",'Male-Female')
opt_day_type <- list("All contacts",
                     "Monday-Friday",
                     "Saturday-Sunday",
                     "Monday-Friday (holidays)",
                     "Monday-Friday (excl. holidays)") 

opt_touch    <- list("All contacts", "Physical contacts","Non-physical contacts")
opt_duration <- list("All contacts","Less than 5 minutes", "Less than 15 minutes","More than 15 minutes","More than 1 hour","More than 4 hours")
opt_country  <- as.list(levels(unique(polymod$participants$country)))

opt_location         <- c("Home","Work","School","Transport","Leisure","Otherplace")
opt_matrix_features   <- c("Reciprocal","Weighted by age","Weighted by week/weekend")

# make named lists
names(opt_gender)   <- unlist(opt_gender)
names(opt_day_type) <- unlist(opt_day_type)
names(opt_touch)    <- unlist(opt_touch)
names(opt_duration) <- unlist(opt_duration)
names(opt_country)  <- unlist(opt_country)
names(opt_location) <- unlist(opt_location)
names(opt_matrix_features) <- unlist(opt_matrix_features)





