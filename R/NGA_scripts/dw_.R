dw_=function(e){
  #calculates the sensitivity of the right eigenvector w on each matrix entry kij
  #INPUT: eigen value object from eigen_ function (e)
  #OUTPUT: list with sensitivity vector corresponding to changes to each matrix entry kij
  
  R=e$eigen$values[1]        # pick dominant eigenvalue
  lambda=e$eigen$values      # pick eigenvalues
  v=e$eigens$v               # left eigenvector
  w=e$eigens$w               # right eigenvector
  res=list()
  name=c()
  out=list()
  
  for (i in seq(1,ncol(w))) {
    for (j in seq(1,ncol(w))) {
      aux=0
      for (s in seq(2,ncol(w))) {
        aux=aux+(v[i,s]/(R-lambda[s]))*w[,s]  # compute sum
      }
      res[[paste0(i,j)]]=w[j,"dominant"]*aux  # compute sensitivity of w towards kij
    }
  }
  return(res)
}


dw_n=function(e){
  #calculates the sensitivity of the normalized right eigenvector w on each matrix entry kij
  #INPUT: eigen value object from eigen_ function (e)
  #OUTPUT: list with sensitivity vector corresponding to changes to each matrix entry kij
  
  w=e$eigens$w[,"dominant"]
  dw=dw_(e)
  
  out=map(dw,function(x){x-w*sum(x)}) 
  return(out)
}









# # Test
# M=matrix(c(1,2,3,4),nrow=2,ncol=2)
# a=c(0.5,1)
# h=c(1,1.2)
# q=1
# pos=2
# NGM=NGM_SIR(q,a,M,h)
# e=eigen_(NGM)
# dw_n(e)
# e$eigens$w[,"dominant"]
# 
# #test
# # changing k12
# expected_change = e$eigens$w[,"dominant"]+(0.01*c(0.1135562, -0.1135562))
# 
# M=matrix(c(1,2,3,4),nrow=2,ncol=2)
# a=c(0.5,1)
# h=c(1,1.2)
# q=1
# pos=2
# NGM=NGM_SIR(q,a,M,h)
# NGM[1,2]=NGM[1,2]+0.01
# e_test=eigen_(NGM)
# e_test$eigens$w[,"dominant"]
# 
# # checks out


dw_a=function(dw_n,q,a,M,h,pos){
  
  Mt=t(M)
  n_row=nrow(Mt)                              # get nr rows and columns
  n_col=ncol(Mt)
  da_aux = matrix(0,nrow = n_row,ncol=n_col)  # build auxiliary matrix (0s everyone except on line pos)
  da_aux[pos,]=1
  
  h_= diag(h,nrow=n_row,ncol=n_col)           # build h parameter vector as a diagonal matrix
  
  da = q*(Mt %*% h_)                          # compute the derivative of "a" with respect to kij
  dK_da = da_aux*da                           # compute the matrix of these derivative
  
  filter_dw_n=dw_n[substr(names(dw_n),1,1)==pos]
  
  aux=list()
  for (i in seq(n_col)) {
    aux[[names(filter_dw_n[i])]]=filter_dw_n[[i]]*dK_da[pos,i]
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
# save_e=e$eigens$w[,"dominant"]
# dwn=dw_n(e)
# 
# dwa=dw_a(dwn,q,a,M,h,pos)
# 
# # test
# # changing a1
# expected_change = save_e+(0.05*c(0.2996934, -0.2996934));expected_change
# 
# M=matrix(c(1,2,3,4),nrow=2,ncol=2)
# a=c(0.5+0.05,1)
# h=c(1,1.2)
# q=1
# pos=1
# NGM=NGM_SIR(q,a,M,h)
# e=eigen_(NGM)
# e$eigens$w[,"dominant"]
# 
# expected_change-e$eigens$w[,"dominant"]

# checks out

dw_a_all=function(dw_n,q,a,M,h,agegroup){
  pos_=seq(1,nrow(M))
  
  da_aux=function(x){
    aux=dw_a(dw_n = dw_n,q=q,a=a,M=M,h=h,pos=x)
    return(aux)
  }
  
  dwa=map(pos_,da_aux)
  names(dwa)=c(paste0("a_",as.character(pos_)))
  
  dwa=as.data.frame(dwa)
  dwa$agegroup=agegroup
  return(dwa)
}

####################

dw_h=function(dw_n,q,a,M,h,pos){
  
  Mt=t(M)
  n_row=nrow(Mt)                              # get nr rows and columns
  n_col=ncol(Mt)
  dh_aux = matrix(0,nrow = n_row,ncol=n_col)  # build auxiliary matrix (0s everyone except on line pos)
  dh_aux[,pos]=1
  
  a_= diag(a,nrow=n_row,ncol=n_col)           # build h parameter vector as a diagonal matrix
  
  dh = q*(a_%*%Mt)                            # compute the derivative of "h" with respect to kij
  dK_dh = dh_aux*dh                           # compute the matrix of these derivative
  
  filter_dw_n=dw_n[substr(names(dw_n),2,2)==pos]
  
  aux=list()
  for (i in seq(n_col)) {
    aux[[names(filter_dw_n[i])]]=filter_dw_n[[i]]*dK_dh[i,pos]
  }
  out=Reduce('+', aux)
}

# # example
# M=matrix(c(1,2,3,4),nrow=2,ncol=2)
# a=c(0.5,1)
# h=c(3,1.5)
# q=1
# pos=1
# NGM=NGM_SIR(q,a,M,h)
# e=eigen_(NGM)
# save_e=e$eigens$w[,"dominant"]
# dwn=dw_n(e)
# 
# dwh=dw_h(dwn,q,a,M,h,pos)
# 
# # test
# # changing h1
# expected_change = save_e+(3*c(-0.00327253,0.00327253));expected_change
# 
# M=matrix(c(1,2,3,4),nrow=2,ncol=2)
# a=c(0.5,1)
# h=c(3+3,1.5)
# q=1
# pos=1
# NGM=NGM_SIR(q,a,M,h)
# e=eigen_(NGM)
# e$eigens$w[,"dominant"]
# 
# expected_change-e$eigens$w[,"dominant"]

dw_h_all = function(dw_n,q,a,M,h,agegroup){
  pos_=seq(1,nrow(M))
  dh_aux=function(x){
    aux=dw_h(dw_n = dw_n,q=q,a=a,M=M,h=h,pos=x)
    return(aux)
  }
  
  
  dwh=map(pos_,dh_aux)
  names(dwh)=c(paste0("h_",as.character(pos_)))
  
  dwh=as.data.frame(dwh)
  dwh$agegroup=agegroup
  
  return(dwh)
}
