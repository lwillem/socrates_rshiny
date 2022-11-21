#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => PLOT STABLE DISTRIBUTION
#
#  Copyright 2020, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________



plot_stable_distribution <- function(list,scale_max=1){
  mij=t(as.matrix(list$NGA$eigen$w[,"dominant"]))
  if(all(is.na(mij))){
    return(NA)
  }
  
  # make sure the age-groups are added to the figure
  colnames(mij) <- colnames(list$NGA$NGM)
  
  avg_infections_age <- mij
  bplt <- barplot(avg_infections_age,
                  xlab="Age-group",
                  ylab="Stable distribution",
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
