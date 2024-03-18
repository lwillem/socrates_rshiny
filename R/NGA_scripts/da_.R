da_=function(q,a,M,h,s,pos){
  #calculates the sensitivity and elasticity of Rt on lower-level parameter a_i
  #INPUT: parameters q,a,M,h, sensitivity of the NGM and index i (a_1 then pos=1)
  #OUTPUT:total sensitivity and elasticity of a_i towards R
  
  Rt = max(abs(s$eigens$values))   # get dominant eigenvalue
  s = s$sens                       # get sensitivity matrix
  Mt=t(M)
  n_row=nrow(s)                    # get nr rows and columns
  n_col=ncol(s)
  
  da_aux = matrix(0,nrow = n_row,ncol=n_col)  # build auxiliary matrix (0s everyone except on line pos)
  da_aux[pos,]=1
  
  h_= diag(h,nrow=n_row,ncol=n_col)           # build h parameter vector as a diagonal matrix
  
  da = q*(Mt %*% h_)                               # compute the derivative of "a" with respect to kij
  
  dK_da = da_aux*da                           # compute the matrix of these derivative
  
  aux=s * dK_da                          # compute each summing term of the sensitivity
  
  sensitivity= sum(aux)                       # compute sensitivity
  elasticity = (a[pos]/Rt)*sensitivity        # compute elasticity
  
  return(list(sen=sensitivity,
              ela=elasticity))
}

all_da=function(q,a,M,h,s){
  #calculates the sensitivity and elasticity of Rt for all lower-level parameters a_i
  #INPUT: parameters q (proportionality factor), a (q-susceptibility), M (contact rates matrix), h (infectivity), s (sensitivity object)
  #OUTPUT: Rt and total sensitivity and elasticity of all a_i
  pos=seq(1,nrow(s$sens))
  
  da_aux=function(x){
    aux=da_(q=q,a=a,M=M,h=h,s=s,pos=x)
    return(aux)
  }
  
  out=map(pos,da_aux)
  names(out)=c(paste0("a_",as.character(pos)))
  
  out=as.data.frame(out)
  out %>%
    pivot_longer(everything()) %>%
    separate(name,c("parameter","indicator"),sep=4)->out
  return(out)
}



# # Test
# M=matrix(c(1,2,3,4),nrow=2,ncol=2)
# a=c(0.5,1)
# h=c(1,1.2)
# q=1
# pos=2
# NGM=NGM_SIR(q,a,M,h)
# eigens=eigen_(NGM)
# sensi=sens(eigens)
# da_(q,a,M,h,s=sensi,pos=pos)
# 
# # test
# test=sum(eigens$eigens$v[,"dominant"][pos]*eigens$eigens$w[,"dominant"]*(M[pos,]*h));test # checks out
# test*(a[pos]/(max(eigens$eigens$values))) # checks out
# 
# # compute all sensitivities and elasticities for a
# 
# all_da(q = q,a = a,M = M,h = h,s = sensi)
# 
# # test if sensitivitis make sense
# a_aux=a
# a_aux[1]=a_aux[1]+0.1
# 
# # Rt should increase by 0.1*1.38
# expected_increase=eigens$eigens$values[1]+(0.1*1.38);expected_increase
# 
# # confirmation
# NGM_aux=NGM_SIR(q=q,a=a_aux,M=M,h=h)
# eigen_(NGM_aux)$eigens$values
# 
# # checks out