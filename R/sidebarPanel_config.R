#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => DEFINE USER INTERFACE AND MODELLING OPTIONS 
#
#  Copyright 2020, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________


##########################
##  UI PANEL OPTIONS    ##
##########################

# set all options
# note: the first is the default
opt_gender   <- list("All","Female","Male")
opt_day_type <- list("All contacts",
                     "Monday-Friday (all)",
                     "Saturday-Sunday (all)",
                     "Monday-Friday (holidays)",
                     "Monday-Friday (excl. holidays)") 

opt_touch    <- list("All contacts", "Physical contacts","Non-physical contacts")
opt_duration <- list("All contacts","Less than 5 minutes", "Less than 15 minutes","More than 15 minutes","More than 1 hour","More than 4 hours")
opt_country  <- as.list(levels(unique(polymod$participants$country)))

# make named lists
names(opt_gender)   <- unlist(opt_gender)
names(opt_day_type) <- unlist(opt_day_type)
names(opt_period)   <- unlist(opt_period)
names(opt_day_type_period)   <- unlist(opt_day_type_period)

names(opt_touch)    <- unlist(opt_touch)
names(opt_duration) <- unlist(opt_duration)
names(opt_country)  <- unlist(opt_country)



