#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => PLOT THE AVERAGE NUMBER OF SOCIAL CONTACTS BY AGE
#
#  Copyright 2022, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________


# mij <- contact_matrix(polymod, countries = "United Kingdom", age.limits = c(0, 1, 5, 15))$matrix;scale_max=10
#mij <- matrix_out$matrix
plot_mean_number_contacts <- function(mij,scale_max=10){
  if(all(is.na(mij))){
    return(NA)
  }
  
  # make sure the age-groups are added to the figure
  rownames(mij) <- colnames(mij)
  
  avg_contacts_age <- rowSums(mij)
  bplt <- barplot(avg_contacts_age,
          xlab="Age of participant (year)",
          ylab="Mean number of contacts per day",
          ylim=range(pretty(avg_contacts_age*1.1),scale_max,0,na.rm=T),
          cex.names =  0.8)
  text(x = bplt,
       y = avg_contacts_age,
       labels = round(avg_contacts_age,digits=2),
       pos=3)
}


