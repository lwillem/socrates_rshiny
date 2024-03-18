# hellinger calibrations

# build optimization function

hellinger_NGA_a = function(a,h,q,M,data){
  a=exp(a)
  NGM = NGM_SIR(q=q,a=a,M=M,h=h)
  eigen=eigen_(NGM)
  model=eigen$eigens$w[,"dominant"]
  
  out=(1/sqrt(2))*sqrt(sum((sqrt(model)-sqrt(data))^2)) #hellinger distance
  return(out)
}

hellinger_NGA_h = function(h,a,q,M,data){
  h=exp(h)
  NGM = NGM_SIR(q=q,a=a,M=M,h=h)
  eigen=eigen_(NGM)
  model=eigen$eigens$w[,"dominant"]
  
  out=(1/sqrt(2))*sqrt(sum((sqrt(model)-sqrt(data))^2)) #hellinger distance
  return(out)
}

hellinger_NGA_both = function(par,q,M,data){
  a=exp(par[1:nrow(M)])
  h=exp(par[(nrow(M)+1):(2*nrow(M))])
  NGM = NGM_SIR(q=q,a=a,M=M,h=h)
  eigen=eigen_(NGM)
  model=eigen$eigens$w[,"dominant"]
  
  out=(1/sqrt(2))*sqrt(sum((sqrt(model)-sqrt(data))^2)) #hellinger distance
  return(out)
}
