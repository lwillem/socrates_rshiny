Rs=function(q,a,M,h){
  NGM=NGM_SIR(q,a,M,h)              # compute NGM
  Rs=colSums(NGM)                   # calculate age-specific reproduction number
  Rr=rowSums(NGM)
  
  Mt=t(M)
  n_row=nrow(Mt)                    # get nr rows and columns
  n_col=ncol(Mt)
  h_=diag(h,nrow=n_row,ncol=n_col) 
  a_=diag(a,nrow=n_row,ncol=n_col) 
  
  
  dR_a=q*(Mt%*%h_)                  # compute the derivative of Rs with respect to each a
  colnames(dR_a)=paste0("R",seq(1,n_row))
  rownames(dR_a)=paste0("a",seq(1,n_row))
  
  
  dR_h=colSums(q*(a_%*%Mt))
  dR_h=diag(dR_h,nrow=n_row,ncol=n_col)
  colnames(dR_h)=paste0("R",seq(1,n_row))
  rownames(dR_h)=paste0("h",seq(1,n_row))
  
  
  return(list(Rs=Rs,Rr=Rr,dR_a=dR_a,dR_h=dR_h,NGM=NGM))
}

# # Test
# M=matrix(c(1,2,3,4),nrow=2,ncol=2)
# a=c(0.5,1)
# h=c(1,1.2)
# q=1
# Rs(q,a,M,h)
# Rs(q,a,M,c(1,2.2))

