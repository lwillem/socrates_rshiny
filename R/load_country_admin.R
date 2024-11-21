#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => SET COUNTRY DATA DETAILS AND FACILITATE EASY LOADING 
#
#  Copyright 2024, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________


## EXPLICIT COUNTRY DATABASE UPDATE
update_country_admin_file <- function(){
  
  # check details
  country_details_stored  <- get_country_admin()
  country_details_fresh <- get_country_admin(country_admin_file_name = NA)
  
  if(any(!is_list_equal_fp(country_details_stored,country_details_fresh))){
    warning('Caution: country admin has changed after last rebase operation')
  }
  
  # execute rebase
  get_country_admin(bool_rebase_file = TRUE)
  
}

## STORE SURVEY DATA ####
check_country_admin_data <- function(){
  
  stored_country_admin_data <- get_country_admin()
  clean_country_admin_data <- get_country_admin(country_admin_file_name = NA)

  if(!all.equal(stored_country_admin_data,clean_country_admin_data)){
  
  }
  
  get_country_admin(bool_rebase_file = TRUE)
  
}


## LOAD SURVEY DATA ####

# survey_name <- sel_dataset$name;bool_has_waves <- sel_dataset$has_waves
load_survey_data <- function(survey_dataset,
                             bool_has_waves,
                             bool_add_to_db = TRUE){
  
  # check if survey database exists, if not, create global variable
  if(!exists("db_survey_data")){
    db_survey_data <<- list() 
  }
  
  # if in db_survey_data, return
  if(survey_dataset %in% names(db_survey_data)){
    
    return(db_survey_data[[survey_dataset]])
    
  } else { # load from disk
    
    # load data
    data_survey <- readRDS(survey_dataset)
    
    # adjust location data: account for missing and multiple locations 
    if(nrow(data_survey$contacts)>0){
      # set data.table to data.frame
      data_cnt_tmp <- data.frame(data_survey$contacts)
      
      # select all location-specific columns
      cnt_location_colnames <- c(paste0('cnt_',tolower(opt_location)))
      data_cnt_tmp <- data_cnt_tmp[,cnt_location_colnames]
      # dim(data_cnt_tmp)
      
      # replace value 'NA' for a location to 'false' (=not-present)
      data_cnt_tmp[is.na(data_cnt_tmp)] <- 0
      
      # add missing location to "other"
      # note: missing could also be "other locations than specified in opt_location"
      cnt_loc_missing <- rowSums(data_cnt_tmp,na.rm=T) == 0
      data_cnt_tmp$cnt_otherplace  <- as.numeric(data_cnt_tmp$cnt_otherplace | cnt_loc_missing)
      
      # 1. calculate cumulative sum (from left to right)
      tmp_loc_cumsum <- t(apply(data_cnt_tmp,1,cumsum))
      
      # 2. set locations with cumulative sum >1 (== not unique and not the "main location") to 0
      data_cnt_tmp[tmp_loc_cumsum>1] <- 0
      
      # 3. copy adjusted location data back
      data_survey$contacts[,cnt_location_colnames] <- data_cnt_tmp
    }
    
    # add date
    data_survey$participants[, date_str := paste0(year, '-', month, '-', day)]
    data_survey$participants[, date := as.Date(date_str, format = "%Y-%m-%d")]
    
    # make sure country is all lower case
    data_survey$participants$country <- tolower(data_survey$participants$country)
    
    # optional: add wave_id
    if(bool_has_waves){
      data_survey$participants <- add_wave_id(data_survey$participants)
    }
    
    # option to store survey data in database
    if(bool_add_to_db){
      db_survey_data[[survey_dataset]] <<- data_survey      
    }
    
    # return data 
    return(data_survey)
  }
}

get_country_admin <- function(country_admin_file_name = 'data/opt_country_admin.rds',
                              bool_rebase_file = FALSE){

  # if file exists, load and return
  if(!bool_rebase_file && file.exists(as.character(country_admin_file_name))){
    return(readRDS(country_admin_file_name))
  }
      
  # else, generate from scratch
  print("Country admin variable is loaded from scratch")
  
  #__________________________#
  ##  COUNTRY OPTIONS    ####
  #__________________________#
  
  # get polymod countries
  polymod_countries <- suppressMessages(survey_countries(polymod))
  
  # add other dataset (and reference)
  opt_country       <- c(paste(polymod_countries,'(Mossong 2008)'),
                          'Peru (Grijalva 2015)',
                          'Zimbabwe (Melegaro 2013)',
                          'France* (Béraud 2015)',
                          'Hong Kong (Leung 2017)',
                          'Vietnam (Horby 2007)',
                          'United Kingdom (van Hoek 2012)',
                          'Russia (Litvinova 2019)',
                          'China (Zhang 2019)',
                          'Zambia (Dodd 2011)',
                          'South Africa (Dodd 2011)',
                          'Belgium 2010* (Van Hoang 2020)',
                          'Belgium CoMix',
                          'Austria CoMix',   
                          'Denmark CoMix',                           
                          'Spain CoMix',   
                          'France CoMix',   
                          'Italy CoMix',     
                          'Portugal CoMix',     
                          'Poland CoMix',   
                          'Finland CoMix',                                                   
                          'Greece CoMix',                                                   
                          'Lithuania CoMix',                                                   
                          'Slovenia CoMix',   
                          'Switzerland CoMix',
                          'Croatia CoMix',
                          'Estonia CoMix',
                          'Hungary CoMix',
                          'Netherlands CoMix',
                          'UK CoMix',
                          'Slovakia CoMix'
                         )
  #Austria","Denmark","Spain","France","Italy","Portugal","Poland
  # fix for Belgium polymod
  opt_country[grepl('Belgium \\(',opt_country)] <- "Belgium 2006 (Mossong 2008)"
  
  # set country admin => filenames and country names
  opt_country_admin <- data.frame(name = opt_country,
                                  dataset = c(rep("polymod2008",8),'peru2011','zimbabwe2013','france2012_spc',
                                              'hong_kong2015','vietnam2018','uk2018',
                                              'russia2019','china2017','zambia_south_africa2011','zambia_south_africa2011',
                                              'belgium2010',
                                              'belgium2020_comix_incl48',
                                              'austria2020_comix',
                                              'denmark2020_comix',
                                              'spain2020_comix',
                                              'france2020_comix',
                                              'italy2020_comix',
                                              'portugal2020_comix',
                                              'poland2020_comix',
                                              'finland2021_comix',
                                              'greece2021_comix',
                                              'lithuania2021_comix',
                                              'slovenia2021_comix',
                                              'switzerland2021_comix',
                                              'croatia2021_comix',
                                              'estonia2021_comix',
                                              'hungary2021_comix',
                                              'netherlands2020_comix',
                                              'uk2020_comix',
                                              'slovakia2021_comix'
                                               ),
                                  country =  c(polymod_countries, 'Peru','Zimbabwe','France',
                                               '','Vietnam','United Kingdom',
                                               'Russia','China','Zambia','South Africa',
                                               'Belgium',
                                               'Belgium',
                                               'Austria',
                                               'Denmark',
                                               'Spain',
                                               'France',
                                               'Italy',
                                               'Portugal',
                                               'Poland',
                                               'Finland',                                                 
                                               'Greece',                                                   
                                               'Lithuania',                                                   
                                               'Slovenia',   
                                               'Switzerland',
                                               'Croatia',
                                               'Estonia',
                                               'Hungary',
                                               'Netherlands',
                                               'United Kingdom',
                                               'Slovakia'
                                               ),
                                  stringsAsFactors = FALSE)
  
  # add with holiday boolean
  opt_country_admin$has_holiday_data <- TRUE
  opt_country_admin$has_holiday_data[opt_country_admin$country %in% c('Italy','Netherlands','Poland',
                                                                      'Russia','South Africa','Vietnam',
                                                                      'Zambia','Zimbabwe')] <- FALSE
  opt_country_admin$has_holiday_data[grepl('hong_kong',opt_country_admin$dataset)] <- FALSE
  opt_country_admin$has_holiday_data[grepl('comix',opt_country_admin$dataset)] <- FALSE
  
  # add dayofweek boolean
  opt_country_admin$has_dayofweek_data <- TRUE
  opt_country_admin$has_dayofweek_data[opt_country_admin$country %in% c('Russia')] <- FALSE
  
  # add contact duration boolean
  opt_country_admin$has_cnt_duration_data <- TRUE
  opt_country_admin$has_cnt_duration_data[opt_country_admin$country %in% c('Zimbabwe','Russia')] <- FALSE
  opt_country_admin$has_cnt_duration_data[grepl('comix',opt_country_admin$dataset)] <- FALSE
  
  # add contact intensity boolean
  opt_country_admin$has_cnt_touch_data <- TRUE
  opt_country_admin$has_cnt_touch_data[grepl('Dodd',opt_country_admin$name)] <- FALSE
  
  # add "supplementary professional contacts" boolean
  opt_country_admin$has_suppl_professional_cnt_data <- FALSE
  opt_country_admin$has_suppl_professional_cnt_data[grepl('\\*',opt_country_admin$name)] <- TRUE
  
  # add "has household member contact info" boolean
  opt_country_admin$has_hhmember_cnt_data <- FALSE
  opt_country_admin$has_hhmember_cnt_data[grepl('Belgium 2010',opt_country_admin$name)] <- TRUE
  
  # add "has wave info" boolean
  opt_country_admin$has_waves <- FALSE
  opt_country_admin$has_waves[grepl('comix',opt_country_admin$dataset)] <- TRUE
  opt_country_admin$has_waves[grepl('france',opt_country_admin$dataset)] <- TRUE
  
  # add "comix boolean"
  opt_country_admin$bool_comix <- FALSE
  opt_country_admin$bool_comix[grepl('comix',opt_country_admin$dataset)] <- TRUE
  
  # complete filenames with relative path
  opt_country_admin$dataset <- paste0('data/survey_',opt_country_admin$dataset,'.rds')
  
  # exclude some datasets (TEMP)
  opt_country_admin <- opt_country_admin[!grepl('van Hoek',opt_country_admin$name),]
  opt_country_admin <- opt_country_admin[!grepl('China',opt_country_admin$name),]
  
  # reformat sort by country name
  opt_country_admin <- opt_country_admin[order(opt_country_admin$name),]

  #__________________________#
  ##  Survey waves        ####
  #__________________________#
  
  # load data and store in list (with wave info) ####
  opt_waves <- 'All'
  opt_country_admin$opt_wave <- opt_waves
  for(i_country in 1:nrow(opt_country_admin)){
    
    # load data (and keep only the first one in the global database)
    data_survey <- load_survey_data(survey_dataset = opt_country_admin$dataset[i_country],
                                    bool_has_waves = opt_country_admin$has_waves[i_country],
                                    bool_add_to_db = (i_country == 1))
  
    if(opt_country_admin$has_waves[i_country]){
      
      # sort wave id's (default sort does not work with 1, 10, 2, ...)
      country_waves    <- unique(data_survey$participants$wave)
      country_waves_id <- unlist(lapply(country_waves,strsplit,':'))[seq(1,length(country_waves)*2,2)]
      
      # add sorted list to opt_country_admin
      opt_country_admin$opt_wave[i_country] <- list(c(opt_waves,country_waves[order(as.numeric(country_waves_id))]))  
    }
  }
  
  #__________________________#
  ##  REFERENCES          ####
  #__________________________#
  
  strsplit_reference <- function(x){
    if(length(x)>1){
      warning('get_reference() only uses the first element')
    }
    x <- as.character(x)
    x <- unlist(strsplit(x,'\\('))[2]
    x <- unlist(strsplit(x,'\\)'))[1]
    return(x) 
  }
  
  get_reference <- function(x_list){
    return(unlist(lapply(x_list,strsplit_reference)))
  }
  
  # add reference
  opt_country_admin$reference <- get_reference(opt_country_admin$name)
 
  
  #__________________________#
  ##  STORE              ####
  #__________________________#
  if(bool_rebase_file){
    saveRDS(opt_country_admin,
            file = country_admin_file_name)
    warning("Country admin data file updated!")
  }
  
  #__________________________#
  ##  RETURN              ####
  #__________________________#
  
  return(opt_country_admin) 
}



