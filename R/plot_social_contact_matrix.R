#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => LOAD AND SELECT SOCIAL CONTACT SURVEY DATA
#
#  Copyright 2020, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________


#mij <- contact_matrix(polymod, countries = "United Kingdom", age.limits = c(0, 1, 5, 15))$matrix
#mij <- matrix_out$matrix
plot_cnt_matrix <- function(mij){
  if(all(is.na(mij))){
    return(NA)
  }
  redc <- rev(heat.colors(100))
  par(mar=c(5, 6, 2, 2),mgp=c(3,0.5,0))
  p <- simage(s = mij, 
             xlab="Age of participant (year)",
             ylab="Age of contact (year)", 
             legend.width=1,
             slim=c(min(mij,na.rm=T), max(mij,na.rm=T)), 
             cex.lab=1.2,
             cex.main=1.2, 
             las=0.1,
             col=redc, 
             main="Average number of contacts / day", 
             xaxt="n", 
             yaxt="n")
  # set axis 
  plt_ticks <- seq(0,1,length=nrow(mij))
  axis(2, at=plt_ticks, labels = c(colnames(mij)),cex.axis=0.9,tick = FALSE,las=1)
  axis(1, at=plt_ticks, labels = c(colnames(mij)),cex.axis=0.9,tick = FALSE)
  
  # get grid centers and add value
  e_grid <- expand.grid(plt_ticks,plt_ticks)
  text(e_grid, labels = format(round(mij, digits = format_num_digits)))
}


