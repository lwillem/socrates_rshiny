# Calculate elasticities -------------------------------------------------

elasti = function(sens){
  # calculates elasticities to entries of the matrix
  #INPUT: sens list from sens function
  #OUTPUT: elasticity matrix
  
  Rt = max(abs(sens$eigens$values))
  A = sens$A
  sens_M = sens$sens
  
  E = (1/Rt)*sens_M*A
  
  return(E)
}
