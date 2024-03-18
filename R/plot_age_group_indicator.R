#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => PLOT age group indicator
#
#  Copyright 2020, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________



plot_age_group_indicator <- function(type,type_name="",title="",scale_max=1){
  mij=type
  if(all(is.na(mij))){
    return(NA)
  }
  
  avg_infections_age <- mij
  bplt <- barplot(avg_infections_age,
                  main=title,
                  xlab="Age-group",
                  ylab=paste0("Age-group R ",type_name),
                  ylim=range(pretty(avg_infections_age*1.1),scale_max,0,na.rm=T),
                  cex.names =  0.8,
                  col=rgb(0.2,0.4,0.6,0.6))
  text(x = bplt,
       y = avg_infections_age,
       labels = round(avg_infections_age,digits=2),
       pos=3)
}

# test

#mij <- contact_matrix(polymod, countries = "United Kingdom", age.limits = c(0, 1, 5, 15))$matrix
#plot_mean_number_infected(mij)