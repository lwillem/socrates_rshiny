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
print('load all packages')
# list all package names
all_packages <- c('socialmixr','npsp','countrycode')

# load package, and install if not present yet
for(package_i in all_packages){
  
  # if not present => install
  if(!package_i %in% rownames(installed.packages())){
    install.packages(package_i)
  }
  
  # load package
  library(package_i, 
          character.only=TRUE, 
          quietly = TRUE, 
          warn.conflicts = FALSE,
          verbose = FALSE)
}


#__________________________#
##  UI PANEL OPTIONS    ####
#__________________________#

# set all options
# note: the first is the default
opt_gender   <- list("All","Female","Male")
opt_day_type <- list("All contacts",
                     "Monday-Friday",
                     "Saturday-Sunday",
                     "Monday-Friday (holidays)",
                     "Monday-Friday (excl. holidays)") 

opt_touch    <- list("All contacts", "Physical contacts","Non-physical contacts")
opt_duration <- list("All contacts","Less than 5 minutes", "Less than 15 minutes","More than 15 minutes","More than 1 hour","More than 4 hours")
opt_country  <- as.list(levels(unique(polymod$participants$country)))

# make named lists
names(opt_gender)   <- unlist(opt_gender)
names(opt_day_type) <- unlist(opt_day_type)
names(opt_touch)    <- unlist(opt_touch)
names(opt_duration) <- unlist(opt_duration)
names(opt_country)  <- unlist(opt_country)



