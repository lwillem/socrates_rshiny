#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => SURVEY DATA DESCRIPTION
# 
#  Copyright 2020, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________

# initialise list
data_description <- list()

# general
data_description['General'] <- 'Each data set that is marked with an * contains supplementary professional contacts (SPC) that have imputed from aggregated statistics.'

# add info for Belgium (Mossong)
data_description['Belgium (Mossong 2008)'] <- 'This data is described in Mossong et al (2008) and does not contain SPC as described in Hens et al (2009, BMC Infect Dis).'

# add info for Belgium2010
data_description[opt_country[grepl('Belgium\\* 2010',opt_country)]] <- 'The manuscript describing this survey in detail is in preparation and will be published soon. Currenlty, see Willem et al (2012, PLoS ONE) and Kifle et al (2015, PLoS ONE) for more info. This dataset contains SPC.'

# add info for France
data_description[opt_country[grepl('France\\*',opt_country)]] <- 'We selected one diary per participant and included SPC.'

# reformat to table
data_description <- data.frame('Name' = names(data_description),
                               'Description' = unlist(data_description))
# 
# # add column names
# names(data_description) <- c('Data set','Description')

data_table_description <- setDT(data_description)

data_table_description
