#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => plot lower-level sens and elasticity
#
#  Copyright 2020, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________



plot_lower_level <- function(df,scale_max=1){
  mij=df
  if(all(is.na(mij))){
    return(NA)
  }
  
  avg_infections_age <- mij
  bplt <- barplot(avg_infections_age,
                  xlab="Age-group",
                  ylab=paste0("lower-level R "),
                  ylim=range(pretty(avg_infections_age*1.1),scale_max,0,na.rm=T),
                  cex.names =  0.8,
                  col=rgb(0.2,0.4,0.6,0.6))
  text(x = bplt,
       y = avg_infections_age,
       labels = round(avg_infections_age,digits=2),
       pos=3)
}
