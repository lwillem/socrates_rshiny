#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => TEST MODELLING OPTION AND LOGIC
#
#  Copyright 2020, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________

# clear environment
rm(list=ls())

#library(simid.rtools)

# load model logic
source('R/socrates_main.R')

opt_ages <- c('0,18,50','3,4,50')

# make named lists
input_design <- expand.grid(age_breaks_text = opt_ages, 
                            country         = opt_country,
                            daytype         = opt_day_type,
                            touch           = opt_touch,
                            duration        = opt_duration,
                            gender          = opt_gender,
                            bool_schools_closed = c(TRUE,FALSE),
                            telework_reference  = c(16),
                            telework_target     = c(16,40),
                            max_part_weight     = c(3),
                            stringsAsFactors = F)


dim(input_design)

# start parallel clusters
par_nodes_info <- smd_start_cluster()

i_test <-192
out <-  foreach(i_test = 1:nrow(input_design),
        .packages = c('data.table','socialmixr','npsp','countrycode','simid.rtools')
        ) %dopar%{
  
  smd_print_progress(i_test,nrow(input_design),par_nodes_info = par_nodes_info)        
          
  length(run_social_contact_analysis(country = input_design$country[[i_test]],
                              daytype = input_design$daytype[[i_test]],
                              touch = input_design$touch[[i_test]],
                              duration = input_design$duration[[i_test]],
                              gender = input_design$gender[[i_test]],
                              cnt_location = opt_location,
                              cnt_matrix_features = opt_matrix_features,
                              age_breaks_text = input_design$age_breaks_text[[i_test]],
                              bool_schools_closed = input_design$bool_schools_closed[[i_test]],
                              telework_reference = input_design$telework_reference[[i_test]],
                              telework_target = input_design$telework_target[[i_test]],
                              max_part_weight = input_design$max_part_weight[[i_test]]))
}
cli::cat_print(summary(warnings()))

table(unlist(out))

smd_stop_cluster()



## FOR DEBUGGING ONLY
if(0==1){
  for(i_test in 18:200){
    print(i_test)
    length(run_social_contact_analysis(country = input_design$country[[i_test]],
                                       daytype = input_design$daytype[[i_test]],
                                       touch = input_design$touch[[i_test]],
                                       duration = input_design$duration[[i_test]],
                                       gender = input_design$gender[[i_test]],
                                       cnt_location = opt_location,
                                       cnt_matrix_features = opt_matrix_features,
                                       age_breaks_text = input_design$age_breaks_text[[i_test]],
                                       bool_schools_closed = input_design$bool_schools_closed[[i_test]],
                                       telework_reference = input_design$telework_reference[[i_test]],
                                       telework_target = input_design$telework_target[[i_test]],
                                       max_part_weight = input_design$max_part_weight[[i_test]]))
    
  }
  summary(warnings())
  
  
  input <- list(country = input_design$country[[i_test]],
                daytype = input_design$daytype[[i_test]],
                touch = input_design$touch[[i_test]],
                duration = input_design$duration[[i_test]],
                gender = input_design$gender[[i_test]],
                cnt_location = opt_location,
                cnt_matrix_features = opt_matrix_features,
                age_breaks_text = input_design$age_breaks_text[[i_test]],
                bool_schools_closed = input_design$bool_schools_closed[[i_test]],
                telework_reference = input_design$telework_reference[[i_test]],
                telework_target = input_design$telework_target[[i_test]],
                max_part_weight = input_design$max_part_weight[[i_test]])
  
  attach(input,warn.conflicts = F)
  
  survey <- get_survey_object(country,
                              daytype,
                              touch,
                              duration,
                              gender,
                              cnt_location,
                              bool_reciprocal = TRUE,
                              bool_exclusive =  FALSE)
  
  
}


