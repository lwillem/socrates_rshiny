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
# countrycode   to convert country names into iso3 codes
# data.table    to adapt socialmixr::contact_matrix

# Explicit loading of the packages (fix for www.shinyapps.io)
library('socialmixr')
library('countrycode')
library('data.table')
library('markdown')
library(shiny)

# temporary to use the get_survey script outside the SocialMixr package
library(httr)
library(jsonlite)
library(XML)
library(curl)

# Package ‘npsp’ was removed from the CRAN repository on 2020-04-23
# ==>> include simage function directly
source('R/npsp/simage.R')
source('R/npsp/splot.R')


#__________________________#
##  UI PANEL OPTIONS    ####
#__________________________#

# set default age breaks (string)
opt_age_breaks <- "0,18,60"

# set all 'select input' options (list)
# note: the first is the default
opt_gender   <- list("All","Female-Female",'Female-Male','Male-Female',"Male-Male")
opt_day_type <- list("All contacts",
                     "Monday-Friday",
                     "Saturday-Sunday",
                     "Monday-Friday (holidays)",
                     "Monday-Friday (excl. holidays)",
                     "All contacts (excl. holidays)") 

opt_touch    <- list("All contacts", "Physical contacts","Non-physical contacts")
opt_duration <- list("All contacts","Less than 5 minutes", "Less than 15 minutes","More than 15 minutes","More than 1 hour","More than 4 hours")

# location note: this sequence affects the "contact hierarchy", as such, if a contact is reported 
# at multiple locations, we use only the first location in this sequence 
opt_location          <- c("Home","Work","School","Transport","Leisure","Otherplace")

# contact reformatting and weights
opt_matrix_features   <- c("Reciprocal","Weigh by age","Weigh by week/weekend",
                           "Age range imputation: sample at random (alternative: midpoint)",
                           "Missing contact age: remove participant",
                           "Suppl. professional contacts (see 'Data sets' tab)",
                           "Set contacts at Home with non-household members as Leisure")

# get polymod countries
polymod_countries <- survey_countries(polymod,quiet = T)

# add other dataset (and reference)
opt_country       <- c(paste(polymod_countries,'(Mossong 2008)'),
                        'Peru (Grijalva 2015)',
                        'Zimbabwe (Melegaro 2013)',
                        'France* (Béraud 2015)',
                        'Hong Kong (Leung 2017)',
                        'Vietnam (Horby 2007)',
                        'United Kingdom (van Hoek 2012)',
                        'Russia (Litvinova 2019)',
                        'China (Zhang 2019)',
                        'Zambia (Dodd 2011)',
                        'South Africa (Dodd 2011)',
                        'Belgium 2010* (Van Hoang 2020)',
                        'Belgium 2020 CoMix (Coletti 2020)'
                       )

# fix for Belgium polymod
opt_country[grepl('Belgium \\(',opt_country)] <- "Belgium 2006 (Mossong 2008)"

# set country admin => filenames and country names
opt_country_admin <- data.frame(name = opt_country,
                                dataset = c(rep("polymod",8),'peru','zimbabwe','france2012_spc',
                                            'hong_kong','vietnam','uk',
                                            'russia','china','zambia_south_africa','zambia_south_africa',
                                            'belgium2010',
                                            #'belgium2020_comix_with_wave9'),
                                             'belgium2020_comix'),
                                country =  c(polymod_countries, 'Peru','Zimbabwe','France',
                                             '','Vietnam','UK',
                                             'Russia','China','Zambia','South Africa',
                                             'Belgium',
                                             'Belgium'),
                                stringsAsFactors = FALSE)

# add with holiday boolean
opt_country_admin$has_holiday_data <- TRUE
opt_country_admin$has_holiday_data[opt_country_admin$country %in% c('Italy','Netherlands','Poland',
                                                                    'Russia','South Africa','Vietnam',
                                                                    'Zambia','Zimbabwe')] <- FALSE
opt_country_admin$has_holiday_data[grepl('hong_kong',opt_country_admin$dataset)] <- FALSE
opt_country_admin$has_holiday_data[grepl('comix',opt_country_admin$dataset)] <- FALSE

# add dayofweek boolean
opt_country_admin$has_dayofweek_data <- TRUE
opt_country_admin$has_dayofweek_data[opt_country_admin$country %in% c('Russia')] <- FALSE

# add contact duration boolean
opt_country_admin$has_cnt_duration_data <- TRUE
opt_country_admin$has_cnt_duration_data[opt_country_admin$country %in% c('Zimbabwe','Russia')] <- FALSE
opt_country_admin$has_cnt_duration_data[grepl('comix',opt_country_admin$dataset)] <- FALSE

# add contact intensity boolean
opt_country_admin$has_cnt_touch_data <- TRUE
opt_country_admin$has_cnt_touch_data[grepl('Dodd',opt_country_admin$name)] <- FALSE

# add "supplementary professional contacts" boolean
opt_country_admin$has_suppl_professional_cnt_data <- FALSE
opt_country_admin$has_suppl_professional_cnt_data[grepl('\\*',opt_country_admin$name)] <- TRUE

# add "has household member contact info" boolean
opt_country_admin$has_hhmember_cnt_data <- FALSE
opt_country_admin$has_hhmember_cnt_data[grepl('Belgium 2010',opt_country_admin$name)] <- TRUE

# add "has wave info" boolean
opt_country_admin$has_waves <- FALSE
opt_country_admin$has_waves[grepl('comix',opt_country_admin$dataset)] <- TRUE
opt_country_admin$has_waves[grepl('france',opt_country_admin$dataset)] <- FALSE
opt_country_admin$num_waves <- 1
opt_country_admin$num_waves[grepl('comix',opt_country_admin$dataset)] <- 8
opt_country_admin$num_waves[grepl('france',opt_country_admin$dataset)] <- 2

# add "comix boolean"
opt_country_admin$bool_comix <- FALSE
opt_country_admin$bool_comix[grepl('comix',opt_country_admin$dataset)] <- TRUE


# complete filenames with relative path
opt_country_admin$dataset <- paste0('data/survey_',opt_country_admin$dataset,'.rds')

# exclude some datasets (TEMP)
opt_country <- opt_country[!grepl('van Hoek',opt_country)]
#opt_country <- opt_country[!grepl('Beraud',opt_country)]
#opt_country <- opt_country[!grepl('Zimbabwe',opt_country)]
opt_country <- opt_country[!grepl('China',opt_country)]

# reformat and sort opt_country
opt_country <- sort(opt_country)

# waves
opt_waves <- (c("All waves",1:max(opt_country_admin$num_waves,na.rm=T))) # 0 is 'no'

# make named lists
names(opt_gender)   <- unlist(opt_gender)
names(opt_day_type) <- unlist(opt_day_type)
names(opt_touch)    <- unlist(opt_touch)
names(opt_duration) <- unlist(opt_duration)
names(opt_country)  <- unlist(opt_country)
names(opt_location) <- unlist(opt_location)
names(opt_matrix_features) <- unlist(opt_matrix_features)
names(opt_waves)    <- unlist(opt_waves)

#__________________________#
##  WEIGHTS             ####
#__________________________#

## MAXIMUM WEIGHT
weight_threshold <- 3


#__________________________#
##  FORMATTING          ####
#__________________________#

# version number
version_id <- paste0('v',read.table('./DESCRIPTION.txt',nrows=1,sep=' ',colClasses='character')[2])


# project website url
url        <- a("socialcontactdata.org", href="http://www.socialcontactdata.org",target="_blank")

# socrates data tool url
url_socrates <- a("SOCRATES initiative", href="http://socialcontactdata.org/socrates/",target="_blank")

# socrates comix url
url_socrates_comix <- a("SOCRATES CoMix", href="http://socialcontactdata.org/socrates-comix/",target="_blank")

# SPC doc url
url_doc_spc <- a("is provided here.", href="https://github.com/lwillem/socrates_rshiny/blob/master/doc/doc_spc_france.md",target="_blank")

# weights doc url
url_doc_weights <- a("More info is provided here.", href="https://github.com/lwillem/socrates_rshiny/blob/master/doc/doc_weights.pdf",target="_blank")

# number of digits to round
format_num_digits <- 2

# UserInterface title
ui_title <- 'Social Contact Rates (SOCRATES) Data Tool'

# boolean for selectInput "duration"
bool_selectInput_duration <- "true"

#__________________________#
##  AGE RANGE           ####
#__________________________#

rng_seed <- 20200101
#method_estimated_contact_age <- 'mean'
method_estimated_contact_age <- 'sample'

#__________________________#
##  REFERENCES          ####
#__________________________#

strsplit_reference <- function(x){
  if(length(x)>1){
    warning('get_reference() only uses the first element')
  }
  x <- as.character(x)
  x <- unlist(strsplit(x,'\\('))[2]
  x <- unlist(strsplit(x,'\\)'))[1]
  return(x) 
}

get_reference <- function(x_list){
  return(unlist(lapply(x_list,strsplit_reference)))
}

# add reference
opt_country_admin$reference <- get_reference(opt_country_admin$name)


#__________________________#
##  DATA DESCRIPTION    ####
#__________________________#

# initialise list
data_description <- list()

# add info
data_description['POLYMOD countries: Italy, Germany, Luxembourg, The Netherlands, Poland, The United Kingdom, Finland, Belgium (Mossong 2008)'] <- 'Mossong et al. (2008) Social Contacts and Mixing Patterns Relevant to the Spread of Infectious Diseases. PLOS Medicine 5(3): e74.'
data_description[opt_country[grepl('Peru',opt_country)]] <- 'Grijalva et al. (2015) A Household-Based Study of Contact Networks Relevant for the Spread of Infectious Diseases in the Highlands of Peru. PLoS One 10(3).'
data_description[opt_country[grepl('Zimbabwe',opt_country)]] <- 'Melegaro et al. (2017) Social Contact Structures and Time Use Patterns in the Manicaland Province of Zimbabwe. PLoS One 12(1).'
data_description[opt_country[grepl('France',opt_country)]] <- 'Béraud et al. (2015) The French Connection: The First Large Population-Based Contact Survey in France Relevant for the Spread of Infectious Diseases. PLoS One 10(7).'
data_description[opt_country[grepl('Hong Kong',opt_country)]] <- 'Leung et al (2017) Social contact patterns relevant to the spread of respiratory infectious diseases in Hong Kong. Sci Rep 7(1), 1–12.'
data_description[opt_country[grepl('Vietnam',opt_country)]] <- '	Horby et al. (2011) Social Contact Patterns in Vietnam and Implications for the Control of Infectious Diseases. PLoS One.'
data_description[opt_country[grepl('Zambia & South Africa',opt_country)]] <- 'Dodd et al. (2016); Age- and Sex-Specific Social Contact Patterns and Incidence of Mycobacterium tuberculosis Infection. Am J Epidemiol.'
data_description[opt_country[grepl('Russia',opt_country)]] <- 'Litvinova et al. (2019); Reactive school closure weakens the network of social interactions and reduces the spread of influenza. PNAS.'

# add info for Zimbabwe
data_description[opt_country[grepl('Zimbabwe',opt_country)]] <- paste(data_description[opt_country[grepl('Zimbabwe',opt_country)]],'We selected one diary per participant.')

# add info for France
data_description[opt_country[grepl('France',opt_country)]] <- paste(data_description[opt_country[grepl('France',opt_country)]],'This dataset contains supplementary professional contacts (SPC) and we selected one diary per participant.')

# add info for Belgium2010
data_description[opt_country[grepl('Belgium 2010\\*',opt_country)]] <- 'Van Hoang et al. (2020). Close contact infection dynamics over time: insights from a second large-scale social contact survey in Flanders, Belgium, in 2010-2011. MedRxiv. This dataset contains supplementary professional contacts (SPC) and whether a contact is a household member.'

# add info for CoMix-BE
data_description['Belgium 2020 CoMix (Coletti 2020)'] <- 'Coletti et al. (2020) CoMix: comparing mixing patterns in the Belgian population during and after lockdown. Scientific Reports 10, 21885'

# reformat to table
data_description <- data.frame('Name' = names(data_description),
                               'Description' = unlist(data_description))

# sort
data_description <- data_description[order(as.character(data_description$Name)),]

# start with POLYMOD
d_order <- c(grep('POLYMOD',data_description$Name),which(!grepl('POLYMOD',data_description$Name)))
data_description <- data_description[d_order,]

# make data table
data_table_description <- setDT(data_description)



