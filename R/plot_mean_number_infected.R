#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => PLOT THE AVERAGE NUMBER OF INFECTED BY AGE AGROUP
#
#  Copyright 2020, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________



plot_mean_number_infected <- function(mij,scale_max=10){
  if(all(is.na(mij))){
    return(NA)
  }
  
  avg_infections_age <- colSums(mij)
  bplt <- barplot(avg_infections_age,
                  xlab="Age-group",
                  ylab="Mean number of infections per generation",
                  ylim=range(pretty(avg_infections_age*1.1),scale_max,0,na.rm=T),
                  cex.names =  0.8)
  text(x = bplt,
       y = avg_infections_age,
       labels = round(avg_infections_age,digits=2),
       pos=3)
}

# test

#mij <- contact_matrix(polymod, countries = "United Kingdom", age.limits = c(0, 1, 5, 15))$matrix
#plot_mean_number_infected(mij)
