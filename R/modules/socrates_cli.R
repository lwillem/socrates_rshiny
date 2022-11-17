#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# A. Run SOCRATES functions via R(studio)
# B. Run a remote SOCRATES UI
#
#  Copyright 2020, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________

################################################################### #
# Run SOCRATES functions via R(studio) ----
################################################################### #

# note: set your work directory to the main SOCRATES repo folder to 
#       enable the relative file paths
# setwd('path/to/SOCRATES/main')

# clear workspace
rm(list=ls())

# load functions and options
source('R/socrates_main.R')
source('R/load_config_base.R') # re-load settings (without CoMix-based selection)

# use the UI defined lists
opt_country
opt_day_type
opt_touch
opt_duration
opt_gender

# aggregate the input parameters in a list
input <- list(age_breaks_num = c(0,18,60),
                country     = opt_country[1],
                daytype     = opt_day_type[1],
                touch       = opt_touch[[1]],
                duration    = opt_duration[[1]],
                gender      = opt_gender[1],
                cnt_location = opt_location,
                cnt_matrix_features   = opt_matrix_features,
                bool_transmission_param = FALSE,
                bool_reciprocal = TRUE,
                bool_suppl_professional_cnt = TRUE,
                wave = 3,
                cnt_reduction = data.frame(Transport=0,Leisure=0,Otherplace=0) # no reductions for this example
  )
  
# include age breaks as text (required for SOCRATES)
input$age_breaks_text = paste(input$age_breaks_num,sep=',')

# set age-specifific susceptibility and/or infectiouness (using the given age breaks)
input$age_susceptibility_text = paste(rep(1,length(input$age_breaks_num)),collapse=',')
input$age_infectiousness_text = input$age_susceptibility_text

# attach all atributes from the input list, so you can use their names directly
attach(input,warn.conflicts = F)

## 1. GET SURVEY OBJECT AND CREATE CONTACT MATRICES (manually) ----
################################################################### #
# get survey object  
survey_obj <- get_survey_object(country,
                              daytype,
                              touch,
                              duration,
                              gender,
                              cnt_location,
                              bool_reciprocal = "Reciprocal" %in% cnt_matrix_features,
                              bool_suppl_professional_cnt = TRUE,
                              bool_hhmatrix_selection = FALSE,
                              wave = wave)

# inspect survey object
lapply(survey_obj,dim)# head(survey$contacts)
summary(survey_obj$contacts)

# generate default contact matrix, based on the survey object
cnt_obj <- contact_matrix(survey_obj, 
                          age.limits = c(0,18,60))

# inspect contact matrix object
lapply(cnt_obj,dim)
table(is.na(cnt_obj$matrix))
sum(cnt_obj$participants$contacts_reported)

# get (default) symmetric contact matrix
matrix_out <- contact_matrix(survey_obj, 
                             age.limits = age_breaks_num,
                             symmetric  = TRUE,
                             quiet      = TRUE)
names(matrix_out)

# inspect contact matrix using internal function(s)  
plot_cnt_matrix(matrix_out$matrix)

# get (weighted) contact matrix based on previous input values
matrix_out <- contact_matrix(survey_obj, 
                             age.limits = age_breaks_num,
                             symmetric  = "Reciprocal" %in% cnt_matrix_features, #TRUE,
                             estimated.contact.age = 'sample',
                             quiet      = TRUE,
                             weigh.dayofweek = "Weigh by week/weekend" %in% cnt_matrix_features, #TRUE,
                             weigh.age       = "Weigh by age" %in% cnt_matrix_features, #TRUE,
                             weight.threshold = weight_threshold)
names(matrix_out)

# inspect contact matrix using internal function(s)  
plot_cnt_matrix(matrix_out$matrix)

## 1.5 run the NGA analysis, which is printed in the UI ----
################################################################### #

A = matrix_out$matrix # contact matrix
eigens=eigen_(A,norm = T);eigens  # compute eigenvalues

sensi=sens(eigens) # compute sensitivities
plot_cnt_matrix(sensi$sens,plot_title_extra = "- sensitivity") # plot sensitivities

# compute the NGM for the SIR age-structured model

beta=rep(0.05,3)
gamma=rep(1/7,3)
C=A
N=c(100,100,100)
S=N
NGM = NGM_SIR(beta,gamma,C,N,S)

plot_cnt_matrix(NGM,plot_title_extra = "- NGM") # plot NGM

check_matrix(NGM) # check primitivity of NGM

eigens_NGM=eigen_(NGM,norm = T);eigens_NGM # Eigenvalues and eigenvectors of the NGM

sensi=sens(eigens_NGM) # sensitivity matrix for the NGM

plot_cnt_matrix(sensi$sens,plot_title_extra = "- sensitivity") # plot sensitivity matrix

# plot lower level sensitivity
dbetas=all_dbeta(beta=beta,gamma=gamma,C=C,N=N,S=S,s=sensi)
dbetas_plot=plot_bar(dbetas %>% filter(indicator=="sen"),"beta sensititivty")


dSs=all_dS(beta=beta,gamma=gamma,C=C,N=N,S=S,s = sensi)
dSs_plot=plot_bar(dSs %>% filter(indicator=="sen"),"S sensitivity")

dgammas=all_dgamma(beta=beta,gamma=gamma,C=C,N=N,S=S,s = sensi)
dgammas_plot=plot_bar(dgammas %>% filter(indicator=="sen"),"gamma sensitivity")

dNs=all_dN(beta=beta,gamma=gamma,C=C,N=N,S=S,s = sensi)
dNs_plot=plot_bar(dNs%>% filter(indicator=="sen"),"N sensitivity")

figure <- ggarrange(dbetas_plot, dSs_plot, dgammas_plot, dNs_plot,
                    ncol = 2, nrow = 2)
figure


## 2. run the SOCRATES analysis, which is printed in the UI ----
################################################################### #

# run the principal function 
socrates_out <- run_social_contact_analysis(country,
                                            daytype,
                                            touch,
                                            duration,
                                            gender,
                                            cnt_location,
                                            cnt_matrix_features,
                                            age_breaks_text,
                                            weight_threshold,
                                            bool_transmission_param,
                                            age_susceptibility_text,
                                            age_infectiousness_text,
                                            bool_NGA_analysis=T,
                                            age_beta_text=as.character(beta),
                                            age_gamma_text=as.character(gamma),
                                            age_N_text=as.character(N),
                                            age_S_text=as.character(S),
                                            wave = wave,
                                            cnt_reduction = cnt_reduction)
# inspect socrates object
names(socrates_out)
socrates_out[1:6]
plot_cnt_matrix(socrates_out$matrix)

socrates_out$NGA$NGM
################################################################### #
# B. Run a remote SOCRATES UI  ----
################################################################### #

if(0==1){ # never executed...
  # load package
  library('shiny')
  
  # run UI via GitHub ----
  runGitHub('socrates_rshiny','lwillem')
  
  # run UI via URL (of GitHub repo) ----
  runUrl('https://github.com/lwillem/socrates_rshiny/archive/master.tar.gz')
}

