# Calculate eigen values and eigen vectors--------------------------------------------------

eigen_ = function(A,norm=T){
  # Calculates eigen values and eigen vectors for both A and A^T, constrains left (v) and right (w) eigen vector
  # such that <v_i,w_i> = 1 and <v_i,w_j> = 0, i!=j.
  
  #INPUT: nonnegative square matrix A
  #OUTPUT: eigenvalues of A and eigen vectors for both A and A^T
  
  R = eigen(A)
  colnames(R$vectors)=c("dominant",as.character(seq(2,ncol(R$vectors))))
  
  if(all(R$vectors[,1]<0)){
    R$vectors[,1]=R$vectors[,1]*-1 # make sure the dominant right eigenvalue is positive
  }
  
  # divide each entry of the right dominant eigenvector w by the sum of the entries in order to be
  # interpreted as the stable population
  
  
  if(norm==T){
    R$vectors[,1]=R$vectors[,1]/sum(R$vectors[,1]) # normalize such that ||w|| = 1
  }
  
  
  # calculate the left eigenvectors constrained on <v_i,w_i> = 1 and <v_i,w_j> = 0, for j!=i
  
  L=eigen(t(A))
  colnames(L$vectors)=c("dominant",as.character(seq(2,ncol(L$vectors))))
  
  for (i in seq(1,ncol(L$vectors))) {
    L$vectors[,i]=L$vectors[,i]/as.numeric((t(L$vectors[,i])%*%R$vectors[,i]))
    
  }
  
  
  eigens=list(values=R$values,w=R$vectors,v=L$vectors)
  
  eigen=list(eigens=eigens,A=A)
  
  return(eigen)
}


# C=matrix(c(4,2,1,3),nrow=2,ncol=2)
# 
# eigen_SIR=eigen_(C,norm=T)
# 
# # checking the the constrained worked
# t(eigen_SIR$eigens$w[,"dominant"]) %*% eigen_SIR$eigens$v[,"dominant"] # <v_i,w_i> = 1
# 
# t(eigen_SIR$eigens$w) %*% eigen_SIR$eigens$v # <v_i,w_j> = 0, for j!=i
