#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => DOWNLOAD LOCATION SPECIFIC MATRICES: AVERAGE AND PER CAPITA 
#
#  Copyright 2020, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________


download_contact_matrices <- function(country,daytype,touch,duration,gender,
                                      cnt_location,cnt_matrix_features,age_breaks_text,
                                      bool_schools_closed,telework_reference,telework_target,max_part_weight,
                                      bool_transmission_param,age_susceptibility_text,age_infectiousness_text,
                                      filename){
  
  
  
    # initiate lists to store all matrices
    location_matrices            <- list()
    location_matrices_per_capita <- list()
    
    # set experimental design by location and reciprocity
    exp_design <- data.frame(location     = c(opt_location,'total','total'),
                             reciprocal   = c(rep(FALSE,7),TRUE)
                             )
    exp_design$name <- tolower(paste0(exp_design$location,ifelse(exp_design$reciprocal,'_reciprocal','')))
    
    i_loc <- 1
    # loop over all contact locations
    for(i_loc in 1:nrow(exp_design)){
      
      # select specific location
      sel_location <- exp_design$location[i_loc]
      if(sel_location == 'total'){
        sel_location <- opt_location
      }
      print(sel_location)
      
      # disable reciprocity if needed
      sel_matrix_features <- opt_matrix_features
      if(!exp_design$reciprocal[i_loc]){
        sel_matrix_features <- sel_matrix_features[!grepl('reciprocal',sel_matrix_features,ignore.case = T)]
      }
      print(sel_matrix_features)
      
      suppressWarnings(
    
      # run SOCRATES-app main function
      out_all <- run_social_contact_analysis(country  = country,
                                             daytype  = daytype,
                                             touch    = touch,
                                             duration = duration,
                                             gender   = gender,
                                             cnt_location            = sel_location,
                                             cnt_matrix_features     = sel_matrix_features,
                                             age_breaks_text         = age_breaks_text,
                                             bool_schools_closed     = bool_schools_closed,
                                             telework_reference      = telework_reference,
                                             telework_target         = telework_target,
                                             max_part_weight         = max_part_weight,
                                             bool_transmission_param = bool_transmission_param,
                                             age_susceptibility_text = age_susceptibility_text,
                                             age_infectiousness_text = age_infectiousness_text)
    )
    # add matrix to list
    location_matrices[i_loc]            <- list(out_all$matrix)
    location_matrices_per_capita[i_loc] <- list(out_all$matrix_per_capita)
    
  } # end for-loop: location
  
    # use meta data, demography and participant info from last output (= total & reciprocal)
    meta_data_total_reciprocal <- out_all$meta_data
    summary_demography         <- out_all$demography
    summary_participants       <- out_all$participants
  
    # add location names and info
    names(location_matrices)            <- exp_design$name
    names(location_matrices_per_capita) <- exp_design$name
  
  # store as .RData
  save(location_matrices,location_matrices_per_capita,
       meta_data_total_reciprocal,summary_demography,
       summary_participants,file = filename)
}


