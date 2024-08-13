# Calculate sensitivities -------------------------------------------------

sens = function(list,tol=1e-07){
  
  # Computes sensitivity of eigenvalues with respect to entries of matrix A (see caswell)
  
  # INPUT: list of the type "eigens" output of function eigen_ and tolerance (tol) of the approximation of w^Tv to the Identity matrix
  # OUTPUT: list of type sens with original matrix, eigenvalues and sensitivity matrix
  
  # check if <v_i,w_i> = 1
  
  lower=t(list$eigens$w[,"dominant"]) %*% list$eigens$v[,"dominant"]-tol
  top=t(list$eigens$w[,"dominant"]) %*% list$eigens$v[,"dominant"]+tol
  bol_=between(1,lower,top)
  
  
  if (bol_!=T) {
    stop("dominant eigenvectors have dot product different than 1")
  }
  
  sens=list$eigens$v[,"dominant"] %*% t(list$eigens$w[,"dominant"]) # vw^T
  colnames(sens) <- colnames(list$A)
  rownames(sens) <- rownames(list$A)
  sens_l = list(A=list$A,eigens=list$eigens,sens=sens)
  
  return(sens_l)
}


# example

# A=matrix(c(7,9,1,5),nrow=2,ncol=2)
# 
# eigen_example=eigen_(A)
# 
# sensi=sens(eigen_example)

