#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => DEFINE COMIX OPTIONS 
#
#  Copyright 2020, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________

#__________________________#
##  SET CoMix BOOLEAN   ####
#__________________________#

bool_is_comix_ui <- FALSE


#__________________________#
##  UPDATE OPTIONS      ####
#__________________________#

# select comix countries
select_ref <-  opt_country_admin$reference[opt_country_admin$bool_comix == bool_is_comix_ui]

# update opt_country
country_ref <- get_reference(opt_country)
opt_country <- opt_country[country_ref %in% select_ref]

# update data table description
table_ref <- get_reference(data_table_description$Name)
data_table_description <- data_table_description[table_ref %in% select_ref,]

# UserInterface title
if(bool_is_comix_ui){
  ui_title <- "SOCRATES CoMix"
  bool_selectInput_duration <- "false"
}





