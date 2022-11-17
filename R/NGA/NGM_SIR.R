# Build NGM ---------------------------------------------------------------

NGM_SIR = function(beta,gamma,C,N,S){
  # Builds the next generation matrix for the simple age-structured SIR model
  # INPUT: vectors age-stratified transmission parameter (beta), recovery rate (gamma), contact structure (C) and population (N)
  # OUTPUT: next generation matrix (see Diekmann and Britton 2013 chapter 7)
  
  n=length(N)
  if (n!=length(beta) || n!=length(gamma) || n!=nrow(C) || n!=ncol(C) || n!=length(S)) {
    stop("parameter vector size do not agree")
  }
  beta_ = diag(beta,nrow=n,ncol=n)
  gamma_ = diag(1/gamma,nrow=n,ncol=n)
  S_ = diag(S,nrow=n,ncol=n)
  N_inv= diag(1/N,nrow=n,ncol=n)
  
  NGM = beta_ %*% gamma_ %*% S_ %*% C %*% N_inv
  
  return(NGM)
  
  
}

# example

# beta=c(0.05,0.05,0.05)
# gamma=c(1/5,1/5,1/5)
# N=c(1,2,3)
# C=matrix(c(1,1,1,1,1,1,1,1,1),nrow=3,ncol=3)
# S=c(1,2,4)
# 
# NGM=NGM_SIR(beta,gamma,C,N,S)
# NGM
