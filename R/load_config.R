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

# temporary to use the get_survey script outside the SocialMixr package
library(httr)
library(jsonlite)
library(XML)
library(curl)

#__________________________#
##  UI PANEL OPTIONS    ####
#__________________________#

# set all options
# note: the first is the default
opt_gender   <- list("All","Female-Female",'Female-Male','Male-Female',"Male-Male")
opt_day_type <- list("All contacts",
                     "Monday-Friday",
                     "Saturday-Sunday",
                     "Monday-Friday (holidays)",
                     "Monday-Friday (excl. holidays)") 

opt_touch    <- list("All contacts", "Physical contacts","Non-physical contacts")
opt_duration <- list("All contacts","Less than 5 minutes", "Less than 15 minutes","More than 15 minutes","More than 1 hour","More than 4 hours")
opt_location          <- c("Home","Work","School","Transport","Leisure","Otherplace")
opt_matrix_features   <- c("Reciprocal","Weighted by age","Weighted by week/weekend")


# get polymod countries
polymod_countries <- survey_countries(polymod,quiet = T)

# add other dataset (and reference)
opt_country       <- c(paste(polymod_countries,'(Mossong 2008)'),
                        'Peru (Grijalva 2011)',
                        'Zimbabwe (Melegaro 2013)',
                        'France (Beraud 2012)',
                        'Hong Kong (Lyung 2015)',
                        'Vietnam (Horby 2007)',
                        'United Kingdom (van Hoek 2012)',
                        'Russia (Litvinova 2019)',
                        'China (Zhang 2019)',
                        'Zambia (Dodd 2011)',
                        'South Africa (Dodd 2011)')

# set country admin => filenames and country names
opt_country_admin <- data.frame(name = opt_country,
                                dataset = c(rep("polymod",8),'peru','zimbabwe','france',
                                            'hong_kong','vietnam','uk',
                                            'russia','china','zambia_south_africa','zambia_south_africa'),
                                country =  c(polymod_countries, 'Peru','Zimbabwe','France',
                                             '','Vietnam','UK',
                                             'Russia','China','Zambia','South Africa'),
                                stringsAsFactors = FALSE)

# add with holiday and dayofweek boolean
opt_country_admin$has_holiday_data <- TRUE
opt_country_admin$has_holiday_data[opt_country_admin$country %in% c('Italy','Netherlands','Poland',
                                                                    'Russia','South Africa','Vietnam',
                                                                    'Zambia')] <- FALSE
opt_country_admin$has_holiday_data[opt_country_admin$dataset %in% c('hong_kong')] <- FALSE

# complete filenames with relative path
opt_country_admin$dataset <- paste0('data/survey_',opt_country_admin$dataset,'.rds')

# complete with holiday boolean
opt_country_admin$has_dayofweek_data <- TRUE
opt_country_admin$has_dayofweek_data[opt_country_admin$country %in% c('Russia')] <- FALSE


# exclude some datasets (TEMP)
opt_country <- opt_country[!grepl('van Hoek',opt_country)]
opt_country <- opt_country[!grepl('Beraud',opt_country)]
opt_country <- opt_country[!grepl('Zimbabwe',opt_country)]

# reformat and sort opt_country
opt_country <- sort(opt_country)

# make named lists
names(opt_gender)   <- unlist(opt_gender)
names(opt_day_type) <- unlist(opt_day_type)
names(opt_touch)    <- unlist(opt_touch)
names(opt_duration) <- unlist(opt_duration)
names(opt_country)  <- unlist(opt_country)
names(opt_location) <- unlist(opt_location)
names(opt_matrix_features) <- unlist(opt_matrix_features)

## MAXIMUM WEIGHT
max_part_weight <- 3



