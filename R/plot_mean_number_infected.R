#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => PLOT THE AVERAGE NUMBER OF INFECTED BY AGE AGROUP
#
#  Copyright 2020, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________



plot_mean_number_infected <- function(list,title,scale_max=10){
  R = max(abs(list$NGA$eigen$values))
  mij=list$NGA$NGM
  if(all(is.na(mij))){
    return(NA)
  }
  
  # make sure the age-groups are added to the figure
  rownames(mij) <- colnames(mij)
  
  avg_infections_age <- t(c(R,colSums(mij)))
  colnames(avg_infections_age)=c("R",colnames(mij))
  
  bplt <- barplot(avg_infections_age,
                  main=title,
                  xlab="",
                  ylab="Mean number of infections per generation",
                  ylim=range(pretty(avg_infections_age*1.1),scale_max,0,na.rm=T),
                  cex.names =  0.8,
                  col=rgb(0.2,0.4,0.6,0.6), 
                  width=c(1,rep(0.5,nrow(mij))))
  text(x = bplt,
       y = avg_infections_age,
       labels = round(avg_infections_age,digits=2),
       pos=3)
}

