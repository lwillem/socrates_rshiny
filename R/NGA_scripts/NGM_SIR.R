# Build NGM as per FRANCO 2022 ---------------------------------------------------------------

NGM_SIR = function(q,a,M,h){
  # Builds the next generation matrix for the simple age-structured SIR model
  # INPUT: vectors age-stratified q-susceptibility (a), q-infectivity (h) and social contact matrix (M) and proportionality factor q
  # OUTPUT: next generation matrix (see Diekmann and Britton 2013 chapter 7)
  names=colnames(M)
  n=nrow(M)
  if (n!=length(a) || n!=length(h)){
    stop("parameter vector size do not agree")
  }
  A_ = diag(a,nrow=n,ncol=n)
  H_ = diag(h,nrow=n,ncol=n)
  
  NGM = A_ %*% t(M) %*% H_
  NGM = q*NGM
  rownames(NGM)=gsub("contact_", "infected_", colnames(M))
  colnames(NGM)=gsub("contact_", "infective_", colnames(M))
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
