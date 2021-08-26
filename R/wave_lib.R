#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => TO LOAD WAVE LABELS ETC
#
#  Copyright 2021, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________



add_wave_id <- function(data_part){
  
  # if(!'wave_wrt_panel' %in% names(data_part)){
  if(!'wave' %in% names(data_part)){
    return(data_part)
  }
  
  if(!'wave_wrt_panel' %in% names(data_part)){
    data_part[,wave_wrt_panel := wave]
    data_part[,panel := NA]
  }
  
  # get wave data
  db_wave <- data_part[,c('part_id','wave_wrt_panel','panel','sday_id')]
  db_wave[,sday_id := gsub('\\.','',as.character(sday_id))] 
  db_wave[,date := as.Date(sday_id,format="%Y%m%d")]  
  db_wave[,panel_wave := paste0(panel,wave_wrt_panel)]
  db_wave[,date_start := min(date),by=panel_wave]
  if(all(is.na(data_part$panel))){
    db_wave[,panel_wave := as.character(date_start)]
  } else{
    db_wave[,panel_wave := paste0(date_start,' [',panel_wave,']')]
  }

  # specify wave lib
  round_lib <- data.table(panel_wave = unique(db_wave[order(date),panel_wave]))
  round_lib[,wave_ext := paste0(1:.N,': ',panel_wave,'')]
  
  # merge wave info with original dataset
  db_wave   <- merge(db_wave[,c('part_id','panel_wave')],
                     round_lib,
                     by='panel_wave')
  data_part <- merge(data_part,
                     db_wave,
                     by='part_id')
  data_part[,wave := wave_ext]

  return(data_part)  
}