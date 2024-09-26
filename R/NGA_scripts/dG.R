# dG_x=function(dx,dw_x,dv_x,n,e,pos){
#   #calculates the sensitivity of the of the product lambda_1 * w_1 w.r.t a given vector
#   #INPUT: sensitivity of lambda and w w.r.t x (filtered)
#   #OUTPUT: sensitivity of lambda_1 * w_1
#   
#   dGdx = dx$value[pos]*e$eigens$w[,"dominant"]+e$eigens$values[pos]*dw_x[,pos]
#   return(dGdx)
# }

G_ratio.f = function(delta,n,dx,dw_x,e,pos){
  w=e$eigens$w[,"dominant"]
  lambda=e$eigen$values[1]
  
  ratio=1+(delta/w)*dw_x+n*(delta/lambda)*dx#+delta*sum(dv_x*w)
  
  return(ratio)
}

G.ratio_da=function(pos,delta_a1,l,da,eigens,q,a,M,h,dwn){
  
  dwa=dw_a(dw_n = dwn,q=q,a=a,M=M,h=h,pos=pos)
  
  da %>% filter(indicator=="sen") ->da_X
  da_X=da_X$value[pos]
  delta=delta_a1
  
  G_ratio=G_ratio.f(delta = delta,
                    dx = da_X,
                    n = l,
                    dw_x = dwa,
                    e = eigens)
  return(G_ratio)
}

all_G_ratio_da=function(delta_a1,l,da,eigens,agegroup,q,a,M,h,dwn){
  
  
  pos=seq(1,nrow(eigens$eigens$w))
  
  G_aux=function(x,y){
    aux=G.ratio_da(pos=x,delta_a1=y,l,da,eigens,q=q,a=a,M=M,h=h,dwn=dwn)
    return(aux)
  }
  
  G.ratio.da=map2(1:nrow(eigens$eigens$w),delta_a1,G_aux)
  names(G.ratio.da)=c(paste0("a_",as.character(pos)))
  # 
  G.ratio.da=as.data.frame(G.ratio.da)
  G.ratio.da$agegroup=agegroup
  # 
  G.ratio.da %>%
    pivot_longer(-agegroup)->G.ratio.da
  
  return(G.ratio.da)
}

G.ratio_dh=function(pos,delta_h1,l,dh,eigens,q,a,M,h,dwn){
  
  dwh=dw_h(dw_n = dwn,q=q,a=a,M=M,h=h,pos=pos)
  
  dh %>% filter(indicator=="sen") ->dh_X
  dh_X=dh_X$value[pos]
  delta=delta_h1
  
  G_ratio=G_ratio.f(delta = delta,
                    dx = dh_X,
                    n = l,
                    dw_x = dwh,
                    e = eigens)
  return(G_ratio)
}

all_G_ratio_dh=function(delta_h1,l,dh,eigens,agegroup,q,a,M,h,dwn){
  
  
  pos=seq(1,nrow(eigens$eigens$w))
  
  G_aux=function(x,y){
    aux=G.ratio_dh(pos=x,delta_h1=y,l,dh,eigens,q=q,a=a,M=M,h=h,dwn=dwn)
    return(aux)
  }
  
  G.ratio.dh=map2(1:nrow(eigens$eigens$w),delta_h1,G_aux)
  names(G.ratio.dh)=c(paste0("h_",as.character(pos)))
  # 
  G.ratio.dh=as.data.frame(G.ratio.dh)
  G.ratio.dh$agegroup=agegroup
  # 
  G.ratio.dh %>%
    pivot_longer(-agegroup)->G.ratio.dh
  
  return(G.ratio.dh)
}
