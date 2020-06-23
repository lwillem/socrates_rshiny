#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => SURVEY DATA DESCRIPTION
# 
#  Copyright 2020, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________

# initialise list
data_description <- list()

# add info
data_description['POLYMOD countries: Italy, Germany, Luxembourg, The Netherlands, Poland, The United Kingdom, Finland, Belgium (Mossong 2008)'] <- 'Mossong J, Hens N, Jit M, Beutels P, Auranen K, et al. (2008) Social Contacts and Mixing Patterns Relevant to the Spread of Infectious Diseases. PLOS Medicine 5(3): e74.'
data_description[opt_country[grepl('Peru',opt_country)]] <- 'Grijalva CG, Goeyvaerts N, Verastegui H, Edwards KM, Gil AI, Lanata CF, et al. (2015) A Household-Based Study of Contact Networks Relevant for the Spread of Infectious Diseases in the Highlands of Peru. PLoS One 10(3)'
data_description[opt_country[grepl('Zimbabwe',opt_country)]] <- 'Melegaro A, Del Fava E, Poletti P, Merler S, Nyamukapa C, et al. (2017) Social Contact Structures and Time Use Patterns in the Manicaland Province of Zimbabwe. PLoS One 12(1)'
data_description[opt_country[grepl('France',opt_country)]] <- 'Béraud G, Kazmercziak S, Beutels P, Levy-Bruhl D, Lenne X, Mielcarek N, et al. (2015) The French Connection: The First Large Population-Based Contact Survey in France Relevant for the Spread of Infectious Diseases. PLoS One 10(7)'
data_description[opt_country[grepl('Hong Kong',opt_country)]] <- 'Leung K, Jit M, Lau EHY, Wu JT (2017) Social contact patterns relevant to the spread of respiratory infectious diseases in Hong Kong. Sci Rep 7(1), 1–12'
data_description[opt_country[grepl('Vietnam',opt_country)]] <- '	Horby P, Thai PQ, Hens N, Yen NTT, Mai LQ, et al. (2011) Social Contact Patterns in Vietnam and Implications for the Control of Infectious Diseases. PLoS One'
data_description[opt_country[grepl('Zambia & South Africa',opt_country)]] <- 'Dodd PJ, Looker C, Plumb ID,Bond V, et al. (2016); Age- and Sex-Specific Social Contact Patterns and Incidence of Mycobacterium tuberculosisInfection.'
data_description[opt_country[grepl('Russia',opt_country)]] <- 'Litvinova M, Liu QH, Kulikov ES and Ajelli M (2019); Reactive school closure weakens the network of social interactions and reduces the spread of influenza.'

# add info for Zimbabwe
data_description[opt_country[grepl('Zimbabwe',opt_country)]] <- paste(data_description[opt_country[grepl('Zimbabwe',opt_country)]],'We selected one diary per participant.')

# add info for France
data_description[opt_country[grepl('France',opt_country)]] <- paste(data_description[opt_country[grepl('France',opt_country)]],'We selected one diary per participant.')

# add info for Belgium2010
data_description[opt_country[grepl('Belgium\\* 2010',opt_country)]] <- 'This dataset contains supplementary professional contacts (SPC). The manuscript describing this survey in detail is in preparation and will be published soon. Willem et al (2012, PLoS ONE) is the first publicaiton using this survey data.'

# reformat to table
data_description <- data.frame('Name' = names(data_description),
                               'Description' = unlist(data_description))

# sort
data_description <- data_description[order(as.character(data_description$Name)),]

# make data table
data_table_description <- setDT(data_description)

data_table_description





