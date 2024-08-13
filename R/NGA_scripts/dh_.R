dh_=function(q,a,M,h,s,pos){
  #calculates the sensitivity and elasticity of Rt on lower-level parameter h_i
  #INPUT: parameters q,a,M,h, sensitivity of the NGM and index i (h_1 then pos=1)
  #OUTPUT:total sensitivity and elasticity of h_i towards R
  Rt = max(abs(s$eigens$values))   # get dominant eigenvalue
  s = s$sens                       # get sensitivity matrix
  Mt=t(M)
  n_row=nrow(s)                    # get nr rows and columns
  n_col=ncol(s)
  
  dh_aux = matrix(0,nrow = n_row,ncol=n_col)  # build auxiliary matrix (0s everyone except on line pos)
  dh_aux[,pos]=1
  
  a_= diag(a,nrow=n_row,ncol=n_col)           # build h parameter vector as a diagonal matrix
  
  dh = q*(a_ %*% Mt)                             # compute the derivative of "h" with respect to kij
  
  dK_dh = dh_aux*dh                           # compute the matrix of these derivative
  
  aux=s * dK_dh                          # compute each summing term of the sensitivity
  
  sensitivity= sum(aux)                       # compute sensitivity
  elasticity = (h[pos]/Rt)*sensitivity        # compute elasticity
  
  return(list(sen=sensitivity,
              ela=elasticity))
}

all_dh=function(q,a,M,h,s){
  #calculates the sensitivity and elasticity of Rt for all lower-level parameters h_i
  #INPUT: parameters q (proportionality factor), a (q-susceptibility), M (contact rates matrix), h (infectivity), s (sensitivity object)
  #OUTPUT: Rt and total sensitivity and elasticity of all h_i
  pos=seq(1,nrow(s$sens))
  
  dh_aux=function(x){
    aux=dh_(q=q,a=a,M=M,h=h,s=s,pos=x)
    return(aux)
  }
  
  out=map(pos,dh_aux)
  names(out)=c(paste0("h_",as.character(pos)))
  
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
# 
# dh_(q,a,M,h,s=sensi,pos=pos)
# 
# # test
# Mt=t(M)
# test=sum(eigens$eigens$v[,"dominant"]*eigens$eigens$w[,"dominant"][pos]*(q*a*Mt[,pos]));test # checks out
# test*(h[pos]/(max(eigens$eigens$values))) # checks out
# 
# # compute all sensitivities and elasticities for a
# 
# all_dh(q = q,a = a,M = M,h = h,s = sensi)
# 
# # test if sensitivitis make sense
# h_aux=h
# h_aux[1]=h_aux[1]+0.1
# 
# # Rt should increase by 0.1*1.38
# expected_increase=eigens$eigens$values[1]+(0.1*0.690);expected_increase
# 
# # confirmation
# NGM_aux=NGM_SIR(q=q,a=a,M=M,h=h_aux)
# eigen_(NGM_aux)$eigens$values
# 
# # checks out