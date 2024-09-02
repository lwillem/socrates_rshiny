#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => DOWNLOAD LOCATION SPECIFIC MATRICES: AVERAGE AND PER CAPITA 
#
#  Copyright 2020, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________


download_contact_matrices <- function(country,
                                      daytype,
                                      touch,
                                      duration,
                                      gender,
                                      cnt_location,
                                      cnt_matrix_features,
                                      age_breaks_text,
                                      weight_threshold,
                                      cnt_reduction, 
                                      wave,
                                      age_susceptibility_text,
                                      age_infectiousness_text,
                                      bool_NGA_analysis,
                                      q_text,
                                      delta_p_text,
                                      nrgen_text,
                                      filename){
  
  
  
    # initiate lists to store all matrices
    location_matrices            <- list()
    location_matrices_per_capita <- list()
    
    # set experimental design by location and reciprocity
    exp_design      <- data.frame(location     = c('total',opt_location))
    exp_design$name <- tolower(paste0(exp_design$location))
    
    i_loc <- 1
    # loop over all contact locations
    for(i_loc in 1:nrow(exp_design)){
      
      # select specific location
      sel_location <- exp_design$location[i_loc]
      if(sel_location == 'total'){
        sel_location <- opt_location
      }

      suppressWarnings(
      # run SOCRATES-app main function
      out_all <- run_social_contact_analysis(country  = country,
                                             daytype  = daytype,
                                             touch    = touch,
                                             duration = duration,
                                             gender   = gender,
                                             cnt_location            = sel_location,
                                             cnt_matrix_features     = cnt_matrix_features,
                                             age_breaks_text         = age_breaks_text,
                                             weight_threshold        = weight_threshold,
                                             cnt_reduction           = cnt_reduction,
                                             wave                    = wave,
                                             age_susceptibility_text = age_susceptibility_text,
                                             age_infectiousness_text = age_infectiousness_text,
                                             bool_NGA_analysis       = bool_NGA_analysis,
                                             q_text                  = q_text,
                                             delta_p_text            = delta_p_text,
                                             nrgen_text              = nrgen_text
                                             )
    )
    # add matrix to list
    location_matrices[i_loc]            <- list(out_all$matrix)
    location_matrices_per_capita[i_loc] <- list(out_all$matrix_per_capita)
    
  } # end for-loop: location
  
    # use meta data, demography and participant info from last output (= total & reciprocal)
    meta_data_total_reciprocal <- out_all$meta_data
    summary_demography         <- out_all$demography
    summary_participants       <- out_all$participants
    
    # store NGA output, if available
    if('NGA' %in% names(out_all)){
      summary_NGA     <- out_all$NGA
    } else{
      summary_NGA     <- NULL
    }
    
    # add location names and info
    names(location_matrices)            <- exp_design$name
    names(location_matrices_per_capita) <- exp_design$name
  
  # store as .RData
  save(location_matrices,
       location_matrices_per_capita,
       meta_data_total_reciprocal,
       summary_demography,
       summary_participants,
       summary_NGA,
       file = filename)
}


