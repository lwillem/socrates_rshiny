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
opt_gender   <- list("all","female","male")
opt_day_type <- list("all days", "weekday","weekend")
opt_period   <- list("regular and holiday","holiday period","regular period") 
opt_touch    <- list("all contacts", "physical contacts","non-physical contacts")
opt_duration <- list("all contacts","less than 5 minutes", "less than 15 minutes","more than 15 minutes","more than 1 hour","more than 4 hours")
opt_country  <- as.list(levels(unique(polymod$participants$country)))

# make named lists
names(opt_gender)   <- unlist(opt_gender)
names(opt_day_type) <- unlist(opt_day_type)
names(opt_period)   <- unlist(opt_period)
names(opt_touch)    <- unlist(opt_touch)
names(opt_duration) <- unlist(opt_duration)
names(opt_country)  <- unlist(opt_country)



