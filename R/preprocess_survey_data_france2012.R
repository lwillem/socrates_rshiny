#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => PREPROCESS AND SAVE COUNTRY-SPECIFIC SURVEY DATA
# 
#  Copyright 2020, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________

# clear workspace
rm(list=ls())

library(openxlsx)
source('R/socrates_main.R')

## LOAD DATA ----

# # load French data from 2012 (from ZENODO)
#survey_data <- get_survey('https://doi.org/10.5281/zenodo.1157918')
#saveRDS(survey_data, file=paste0('data/survey_france_zenodo.rds'))
survey_data <- readRDS('data/survey_france_zenodo.rds')

# fix type of 'holiday' variable
survey_data$participants$holiday <-  survey_data$participants$holiday == 1

## IMPUTE SUPPLEMENTAIRY PROFESSIONAL CONTACTS ----
#' Supplementary professional contacts were defined when participant i reported having n_i^w (>20) contacts 
#' at work made in a specific set of age-categories Iia. We used the age, gender, duration of contact and 
#' whether the contact involved skin-to-skin touching of the reported contacts at work when 10 <  n_i^w < 20 
#' as a basis for imputation. This set of contacts was resampled with probabilities according to the reported 
#' age distribution, taking into account the French population age structure in 2012 (INSEE) and implemented 
#' into the data set when the day of study was a weekday.
#' 
#' Source: Supplementairy information: https://doi.org/10.1371/journal.pone.0133203.s006

db_participants <- survey_data$participants
db_contacts     <- survey_data$contacts

# number of contacs
names(db_participants)

# select participants with more than 20 spc
db_participants <- db_participants[db_participants$work_contacts_nr > 20,]

# censoring at 134 contacts (the 95% percentile) to limit the impact of outliers
table(db_participants$work_contacts)
table(db_participants$work_contacts_nr)
threshold_work_contacts_nr <- 134
db_participants$work_contacts_nr[db_participants$work_contacts_nr>134] <- threshold_work_contacts_nr

# select surveys during weekdays
db_participants <- db_participants[db_participants$dayofweek %in% 1:5,]
dim(db_participants)

# select working individuals
table(db_participants$participant_occupation)
table(db_participants$part_age)
# remove participants with missing data about their work contacts (ages)
missing_age_info <- rowSums(db_participants[,c(paste0('AgeContactPro',1:5))]) > 0
db_participants <- db_participants[missing_age_info,]
table(missing_age_info)

# contact ages
table(db_participants$work_contacts_nr)
table(db_participants$AgeContactPro1)   # 0-2 years  # the paper mentiones 0-3 years
table(db_participants$AgeContactPro2)   # 3-10 years
table(db_participants$AgeContactPro3)   # 11-17 years
table(db_participants$AgeContactPro4)   # 18-64 years
table(db_participants$AgeContactPro5)   # 65-... years

# load French demography from 2012
#demo_france <- wpp_age('france',2012)
demo_france <- openxlsx::read.xlsx(xlsxFile='data/demo_france_insee_ip1429.xlsx',
                                   sheet='Graphique complémentaire 1',
                                   rows = 5:105,
                                   cols = c(3,5))
demo_france <- data.frame(age = 0:99,
                          population = rowSums(abs(demo_france)))

#' Other contact characteristics were imputed by resampling the characteristics of 
#' professional contacts from participants who had between 10 and 20 professional 
#' contacts

# select participants who had between 10 and 20 professional contacts
aggr_contacts_work <- aggregate(cnt_work ~ part_id, data = db_contacts,sum)
aggr_contacts_work <- aggr_contacts_work[aggr_contacts_work$cnt_work %in% 10:20,]

# create database with work contacts to sample from
db_contact_work <- db_contacts[db_contacts$part_id %in% aggr_contacts_work$part_id &
                                 db_contacts$cnt_work == 1,]

# clean database to sample from
db_contact_work[,cont_id:=NA_integer_]
db_contact_work[,part_id:=NA_integer_]
db_contact_work[,cnt_home:=FALSE]
db_contact_work[,cnt_school:=FALSE]
db_contact_work[,cnt_transport:=FALSE]
db_contact_work[,cnt_leisure:=FALSE]
db_contact_work[,cnt_otherplace:=FALSE]
db_contact_work[,wave:=NA_integer_]
db_contact_work[,studyDay:=NA_integer_]
db_contact_work[,is_imputed :=TRUE]



# IMPUTE...

db_spc_all <-NULL

i_part <- 1
for(i_part in 1:nrow(db_participants)){
  
  # get number of spc
  num_work_contacts <- db_participants$work_contacts_nr[i_part]

  # contacts details: random semectop, from subset with work contacts between 10:20
  db_spc <- db_contact_work[sample(nrow(db_contact_work),num_work_contacts),]

  # set age based on given range and country demography
  sample_prob_ages<-c(rep(db_participants$AgeContactPro1[i_part],length(0:2)),   # 0-2 years  # the paper mentiones 0-3 years
                      rep(db_participants$AgeContactPro2[i_part],length(3:10)),  # 3-10 years
                      rep(db_participants$AgeContactPro3[i_part],length(11:17)), # 11-17 years
                      rep(db_participants$AgeContactPro4[i_part],length(18:64)), # 18-64 years
                      rep(db_participants$AgeContactPro5[i_part],length(65:99))) # 65-... years
  
  sample_prob_ages <- demo_france[sample_prob_ages == 1,]
  db_spc$cnt_age_exact <- sample(sample_prob_ages$age,num_work_contacts,prob = sample_prob_ages$population,replace = T)
  
  # add part info
  db_spc[,part_id  := db_participants$part_id[i_part]]
  db_spc[,wave     := db_participants$wave[i_part]]
  db_spc[,studyDay := db_participants$studyDay[i_part]]
  
  db_spc_all <- rbind(db_spc_all,db_spc)
}

dim(db_spc_all)
sum(db_participants$work_contacts_nr)

# merge dataset
db_contacts$is_imputed <- FALSE
survey_data$contacts <- rbind(db_contacts,db_spc_all)
dim(db_contacts)

# DIARY SELECTION = ONE SURVEY PER PARTICIPANT ----
# note: one participant could have participated 2x in wave 1 and 2x in wave 2
# ==>> so one participant can contain "surveyDay == 1" twice

# create diary id
survey_data$contacts[,diary_id:= paste(part_id,wave,studyDay,sep='_') ]
survey_data$participants[,diary_id:= paste(part_id,wave,studyDay,sep='_') ]

# Conservative approach: select one diary per participant
table(table(survey_data$participants$part_id))
part_data              <- survey_data$participants
part_data              <- part_data[order(part_id),]
part_data              <- part_data[!duplicated(part_id), ]
table(table(part_data$part_id))

# select subset based on participant selection above
survey_data$participants     <- survey_data$participants[diary_id %in% part_data$diary_id,]
survey_data$contacts         <- survey_data$contacts[diary_id %in% part_data$diary_id,]
dim(survey_data$contacts)

## SAVE DATA ----

# get socialmixr 'survey' object
survey_france2012 <- survey_data

saveRDS(survey_france2012, file=paste0('data/survey_france2012_spc.rds'))

cite(survey_france2012)
survey_data$reference


## COMPARE DATA SETS ----

data_new  <- readRDS('data/survey_france.rds')       # Raw ZENODO dataset
data_prev <- readRDS('data/survey_france_spc.rds')   # Hackathon dataset from Guillaume

lapply(data_new,dim)
lapply(data_prev,dim)
lapply(survey_france2012,dim)

names(survey_france2012$participants)
names(data_new$participants)

survey_france2012$participants$holiday
survey_france2012$participants$dayofweek

table(data_prev$participants$part_id == survey_france2012$participants$part_id)
table(data_prev$participants$holiday == survey_france2012$participants$holiday)
table(data_prev$participants$dayofweek == survey_france2012$participants$dayofweek)

table(survey_france2012$contacts$duration_multi,useNA = 'ifany')
table(data_prev$contacts$duration_multi,useNA = 'ifany')

table(data_prev$contacts$phys_contact,useNA = 'ifany')
table(survey_france2012$contacts$phys_contact,useNA = 'ifany')

survey_france2012$participants$part_age[survey_france2012$participants$holiday==1]

summary(data_prev$participants$holiday)
summary(survey_france2012$participants$holiday)

survey_france2012$participants$diary_id <- NULL
unlist(lapply(survey_france2012$participants,typeof)) == unlist(lapply(data_prev$participants,typeof))
# table(data_new$participants == data_prev$participants)
# table(data_new$contacts == data_prev$contacts)
# 
# summary(data_new$contacts)
# summary(data_prev$contacts)
# 
# contact_matrix(survey_france2012)






