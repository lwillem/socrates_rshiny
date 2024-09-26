dv_=function(e){
  #calculates the sensitivity of the left eigenvector v on each matrix entry kij
  #INPUT: eigen value object from eigen_ function (e)
  #OUTPUT: list with sensitivity vector corresponding to changes to each matrix entry kij
  
  R=e$eigen$values[1]        # pick dominant eigenvalue
  lambda=e$eigen$values      # pick eigenvalues
  v=e$eigens$v               # left eigenvector
  w=e$eigens$w               # right eigenvector
  res=list()
  name=c()
  out=list()
  
  for (i in seq(1,ncol(v))) {
    for (j in seq(1,ncol(v))) {
      aux=0
      for (s in seq(2,ncol(v))) {
        aux=aux+(w[j,s]/(R-lambda[s]))*v[,s]  # compute sum
      }
      res[[paste0(i,j)]]=v[i,"dominant"]*aux  # compute sensitivity of w towards kij
    }
  }
  return(res)
}

# # Test
# M=matrix(c(1,2,3,4),nrow=2,ncol=2)
# a=c(0.5,1)
# h=c(1,1.2)
# q=1
# pos=2
# NGM=NGM_SIR(q,a,M,h)
# e=eigen_(NGM)
# dv_(e)
# e$eigens$v[,"dominant"]
# 
# #test
# # changing k12
# expected_change = e$eigens$v[,"dominant"]+(1*c(-0.04978317,  0.02976578))
# 
# M=matrix(c(1,2,3,4),nrow=2,ncol=2)
# a=c(0.5,1)
# h=c(1,1.2)
# q=1
# pos=2
# NGM=NGM_SIR(q,a,M,h)
# NGM[1,2]=NGM[1,2]+1
# e_test=eigen_(NGM)
# e_test$eigens$v[,"dominant"]
# 
# # checks out


dv_a=function(dv_,q,a,M,h,pos){
  
  Mt=t(M)
  n_row=nrow(Mt)                              # get nr rows and columns
  n_col=ncol(Mt)
  da_aux = matrix(0,nrow = n_row,ncol=n_col)  # build auxiliary matrix (0s everyone except on line pos)
  da_aux[pos,]=1
  
  h_= diag(h,nrow=n_row,ncol=n_col)           # build h parameter vector as a diagonal matrix
  
  da = q*(Mt %*% h_)                          # compute the derivative of "a" with respect to kij
  dK_da = da_aux*da                           # compute the matrix of these derivative
  
  filter_dv_=dv_[substr(names(dv_),1,1)==pos]
  
  aux=list()
  for (i in seq(n_col)) {
    aux[[names(filter_dv_[i])]]=filter_dv_[[i]]*dK_da[pos,i]
  }
  out=Reduce('+', aux)
}

# # example
# M=matrix(c(1,2,3,4),nrow=2,ncol=2)
# a=c(0.5,1)
# h=c(1,1.2)
# q=1
# pos=1
# NGM=NGM_SIR(q,a,M,h)
# e=eigen_(NGM)
# save_e=e$eigens$v[,"dominant"]
# dv=dv_(e)
# 
# dv_a=dv_a(dv,q,a,M,h,pos)
# 
# # test
# # changing a1
# expected_change = save_e+(0.05*dv_a);expected_change
# 
# M=matrix(c(1,2,3,4),nrow=2,ncol=2)
# a=c(0.5+0.05,1)
# h=c(1,1.2)
# q=1
# pos=1
# NGM=NGM_SIR(q,a,M,h)
# e=eigen_(NGM)
# e$eigens$v[,"dominant"]
# 
# expected_change-e$eigens$v[,"dominant"]

# checks out

dv_h=function(dv_,q,a,M,h,pos){
  
  Mt=t(M)
  n_row=nrow(Mt)                              # get nr rows and columns
  n_col=ncol(Mt)
  dh_aux = matrix(0,nrow = n_row,ncol=n_col)  # build auxiliary matrix (0s everyone except on line pos)
  dh_aux[pos,]=1
  
  a_= diag(a,nrow=n_row,ncol=n_col)           # build h parameter vector as a diagonal matrix
  
  dh = q*(a_%*%Mt)                            # compute the derivative of "h" with respect to kij
  dK_dh = dh_aux*dh                           # compute the matrix of these derivative
  
  filter_dv_=dv_[substr(names(dv_),2,2)==pos]
  
  aux=list()
  for (i in seq(n_col)) {
    aux[[names(filter_dv_[i])]]=filter_dv_[[i]]*dK_dh[i,pos]
  }
  out=Reduce('+', aux)
}

# # example
# M=matrix(c(1,2,3,4),nrow=2,ncol=2)
# a=c(0.5,1)
# h=c(3,1.5)
# q=1
# pos=2
# NGM=NGM_SIR(q,a,M,h)
# e=eigen_(NGM)
# save_e=e$eigens$v[,"dominant"]
# dv=dv_(e)
# 
# dvh=dv_h(dv,q,a,M,h,pos)
# 
# # test
# # changing h1
# expected_change = save_e+(0.3*dvh);expected_change
# 
# M=matrix(c(1,2,3,4),nrow=2,ncol=2)
# a=c(0.5,1)
# h=c(3,1.5+0.15)
# q=1
# NGM=NGM_SIR(q,a,M,h)
# e=eigen_(NGM)
# e$eigens$v[,"dominant"]
# 
# expected_change-e$eigens$v[,"dominant"]
# 
# #checks out
