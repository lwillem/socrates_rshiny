#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => DEFINE USER INTERFACE AND MODELLING OPTIONS 
#
#  Copyright 2024, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________

#__________________________#
##  SET CoMix BOOLEAN   ####
#__________________________#

if(!'bool_is_comix_ui' %in% ls()) {
  bool_is_comix_ui <- FALSE
}

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
library('wpp2015')
library(shiny)
library(tidyverse) #
library(ggthemes)  #
library(ggpubr)    #
library(shinyjs)       # For disabling/enabling inputs

# temporary to use the get_survey script outside the SocialMixr package
library(httr)
library(jsonlite)
library(XML)
library(curl)

# Package ‘npsp’ was removed from the CRAN repository on 2020-04-23
# ==>> include simage function directly
source('R/npsp/simage.R')
source('R/npsp/splot.R')

# loading help functions on country admin
source('R/load_country_admin.R')

# loading help functions on wave id
source('R/wave_lib.R')

#load NGA SCRIPTS functions
source('R/next_gen_lib.R')
source('R/plot_next_gen.R')

#__________________________#
##  UI PANEL OPTIONS    ####
#__________________________#

# UserInterface title
ui_title <- 'Social Contact Rates (SOCRATES) Data Tool'

# boolean for selectInput "duration"
bool_selectInput_duration <- "true"

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

opt_touch    <- list("All", "Physical contacts","Non-physical contacts")
opt_duration <- list("All","Less than 5 minutes", "Less than 15 minutes","More than 15 minutes","More than 1 hour","More than 4 hours")

# location note: this sequence affects the "contact hierarchy", as such, if a contact is reported 
# at multiple locations, we use only the first location in this sequence 
opt_location          <- c("Home","Work","School","Transport","Leisure","Otherplace")

# contact reformatting and weights
opt_matrix_features   <- c("Reciprocal","Weigh by age","Weigh by week/weekend",
                           "Age range imputation: sample at random (alternative: use mean)",
                           "Missing contact age: sample from peers (alternative: remove participant)",
                           "Suppl. professional contacts (see 'Data sets' tab)",
                           "Set contacts at Home with non-household members as Leisure")

# get country details 
opt_country_admin <- get_country_admin()

# remove 'all' for Comix
# for(i in 1:length(opt_country_admin$opt_wave)){
for(i in which(opt_country_admin$bool_comix)){
  opt_country_admin$opt_wave[[i]] <- opt_country_admin$opt_wave[[i]][!opt_country_admin$opt_wave[[i]] %in% 'All']
}

# # get country names
opt_country <- opt_country_admin$name

#__________________________#
##  CoMiX    OPTIONS    ####
#__________________________#
if('bool_is_comix_ui' %in% ls() &&
   !is.na(bool_is_comix_ui)){
  
  # select countries
  opt_country_admin <- opt_country_admin[opt_country_admin$bool_comix == bool_is_comix_ui,]

  # update opt_country
  opt_country <- opt_country[opt_country %in% opt_country_admin$name]
  
  # UserInterface title
  if(bool_is_comix_ui){
    ui_title <- "SOCRATES CoMix"
    bool_selectInput_duration <- "false"
  }
}

# waves (survey specific options)
opt_waves <<- opt_country_admin$opt_wave[[1]]

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

# weights doc url
url_doc_weights <- a("More info is provided here.", href="https://github.com/lwillem/socrates_rshiny/blob/master/doc/doc_weights.pdf",target="_blank")

# data doc url
url_doc_data <- a("More info on the data curation is provided here.", href="https://github.com/lwillem/socrates_rshiny/blob/master/doc/doc_data_updates.md",target="_blank")

# number of digits to round
format_num_digits <- 2



#__________________________#
##  AGE RANGE           ####
#__________________________#

rng_seed <- 20200101
#method_estimated_contact_age <- 'mean'
method_estimated_contact_age <- 'sample'

#__________________________#
##  DATA DESCRIPTION    ####
#__________________________#

# initialise list
data_description <- list()

# add info
data_description[opt_country[grepl('Mossong',opt_country)]] <- 'Mossong et al. (2008) Social Contacts and Mixing Patterns Relevant to the Spread of Infectious Diseases. PLOS Medicine 5(3): e74.'
data_description[opt_country[grepl('Peru',opt_country)]] <- 'Grijalva et al. (2015) A Household-Based Study of Contact Networks Relevant for the Spread of Infectious Diseases in the Highlands of Peru. PLoS One 10(3).'
data_description[opt_country[grepl('Zimbabwe',opt_country)]] <- 'Melegaro et al. (2017) Social Contact Structures and Time Use Patterns in the Manicaland Province of Zimbabwe. PLoS One 12(1).'
data_description[opt_country[grepl('France',opt_country)]] <- 'Béraud et al. (2015) The French Connection: The First Large Population-Based Contact Survey in France Relevant for the Spread of Infectious Diseases. PLoS One 10(7).'
data_description[opt_country[grepl('Hong Kong',opt_country)]] <- 'Leung et al (2017) Social contact patterns relevant to the spread of respiratory infectious diseases in Hong Kong. Sci Rep 7(1), 1–12.'
data_description[opt_country[grepl('Vietnam',opt_country)]] <- '	Horby et al. (2011) Social Contact Patterns in Vietnam and Implications for the Control of Infectious Diseases. PLoS One.'
data_description[opt_country[grepl('Zambia & South Africa',opt_country)]] <- 'Dodd et al. (2016); Age- and Sex-Specific Social Contact Patterns and Incidence of Mycobacterium tuberculosis Infection. Am J Epidemiol.'
data_description[opt_country[grepl('Russia',opt_country)]] <- 'Litvinova et al. (2019); Reactive school closure weakens the network of social interactions and reduces the spread of influenza. PNAS.'

# add info for Zimbabwe
data_description[opt_country[grepl('Zimbabwe',opt_country)]] <- paste(data_description[opt_country[grepl('Zimbabwe',opt_country)]],'We selected one diary per participant.')

# add info for France* (Beraud)
data_description[opt_country[grepl('France\\*',opt_country)]] <- paste(data_description[opt_country[grepl('France\\*',opt_country)]],
                                                                       'This dataset contains supplementary professional contacts (SPC) and we selected one diary per participant. <a href="https://github.com/lwillem/socrates_rshiny/blob/master/doc/doc_spc_france.md" target="_blank">More info on the SPC is provided here.</a>')

# add info for Belgium2010
data_description[opt_country[grepl('Belgium 2010\\*',opt_country)]] <- 'Van Hoang et al. (2020). Close contact infection dynamics over time: insights from a second large-scale social contact survey in Flanders, Belgium, in 2010-2011. BMC Infectious Diseases 21: 274. This dataset contains supplementary professional contacts (SPC) and whether a contact is a household member.'

# # add info for CoMix-BE
# data_description['Belgium 2020 CoMix (Coletti 2020)'] <- 'Coletti et al. (2020) CoMix: comparing mixing patterns in the Belgian population during and after lockdown. Scientific Reports 10, 21885'

# add info for CoMix-EU
#data_description[opt_country[grepl('(Verelst 2021)',opt_country)]] <- 'Verelst et al. (2021) SOCRATES-CoMix: A platform for timely and open-source contact mixing data during and in between COVID-19 surges and interventions in over 20 European countries. BMC Medicine 19(1):254.'
data_description[opt_country[grepl('CoMix',opt_country)]] <- 'Wong et al. (2023) Social contact patterns during the COVID-19 pandemic in 21 European countries – evidence from a two-year study. BMC Infect Dis 23, 268.'

# add info for CoMix-UK
data_description[opt_country[grepl('UK CoMix',opt_country)]] <- 'Gimma et al. (2022) Changes in social contacts in England during the COVID-19 pandemic between March 2020 and March 2021 as measured by the CoMix survey: A repeated cross-sectional study. PLOS Medicine 19(3): e1003907.'

# add info for CoMix-NL
data_description[opt_country[grepl('Netherlands CoMix',opt_country)]] <- 'Backer et al (2022): Dynamics of non-household contacts during the COVID-19 pandemic in 2020 and 2021 in the Netherlands.Scientific Reports 13:5166'

# add info for CoMix-IT
data_description[opt_country[grepl('Italy CoMix',opt_country)]] <- 'Tizzani et al. (2023). Impact of tiered measures on social contact and mixing patterns of in Italy during the second wave of COVID-19. BMC Public Health 23, 906.'

# add info for CoMix-Norway
data_description[opt_country[grepl('Norway CoMix',opt_country)]] <- 'Veneti et al. (2024) Social contact patterns during the early COVID-19 pandemic in Norway: insights from a panel study, April to September 2020. BMC Public Health 24, 1438.'

# add info for CoMix-Switzerland
data_description[opt_country[grepl('Switzerland CoMix',opt_country)]] <- 'Reichmuth et al. (2024) Social contacts in Switzerland during the COVID-19 pandemic: Insights from the CoMix study. Epidemics 47, 100771.'

# reformat to table
data_description <- data.frame('Name' = names(data_description),
                               'Description' = unlist(data_description))

# sort
data_description <- data_description[order(as.character(data_description$Name)),]

# start with POLYMOD
d_order <- c(grep('Mossong',data_description$Name),which(!grepl('Mossong',data_description$Name)))
data_description <- data_description[d_order,]

# make data table
data_table_description <- setDT(data_description)



