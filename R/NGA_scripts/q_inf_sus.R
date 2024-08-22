# Calculate q-susceptibility and q-infectivity -------------------------------------------------


q_susc=function(lambda,q,M,h,data,ref_class){
  #calculates q-susceptibility (relative to ref_class) given that a*Mt*h*w=lambda*w
  #INPUT: dominant eigenvalue lambda, contact matrix M, q-infectivity h, relative incidence (data), ref class
  #OUTPUT: q-susceptibility vector (a)
  if (ref_class>0 & ref_class <=nrow(M)) {
    a=c()
    Mt=t(M)
    ref.class=(lambda/q)*(data[ref_class]/(sum(Mt[ref_class,]*h*data)))
    for (i in seq(nrow(M))) {
      a[i]=(lambda/q)*(data[i]/(sum(Mt[i,]*h*data)))
    }
    return(list(a=a,a_n=a/ref.class,ref.class=ref.class)) 
  }else{
    stop("ref_class does not match age_structure")
  }
}

q_inf=function(lambda,q,M,a,data,ref_class){
  #calculates q-infectivity given that a*Mt*h*w=lambda*w
  #INPUT: dominant eigenvalue (lambda), contact matrix M, q-susceptibility a, relative incidence (data), ref class
  #OUTPUT: q-infectivity vector (h)
  Mt=t(M)
  coef=matrix(t(M)%*%diag(data,nrow=nrow(M),ncol=ncol(M)),nrow=nrow(M),ncol=ncol(M))
  res=matrix((lambda/q)*(data/a),nrow = nrow(M),ncol = 1)
  h=as.numeric(solve(coef,res))
  ref.class=h[ref_class]
  h_n=h/ref.class
  
  return(list(h=h,h_n=h_n,ref.class=ref.class))
  
}



# test

# source("config.R")
# source(paste0(pish,"/scripts/NGM_SIR.R"))
# source(paste0(pish,"/scripts/eigen.R"))
# source(paste0(pish,"/scripts/sens.R"))
# source(paste0(pish,"/scripts/elasti.R"))
# 
# 
# M=read.csv(paste0(pish_I,"/20221128141454_social_contact_matrix.csv"),sep = ",",check.names = F)
# #M=matrix(c(1,2,7,4,8,6,13,8,9,2,11,1,13,3,15,9),nrow=4,ncol=4)
# data=c(0.0404616, 0.2547371, 0.3574583, 0.3473429)
# Ro=2.5
# q=1
# ref_class=4
# 
# # susceptibility
# h=c(0.5,0.5,0.7,0.9)
# 
# 
# q.susc=q_susc(lambda=Ro,M=M,h=h,data=data,ref_class=ref_class) # calculate q-susc
# a=q.susc$a 
# ref.class=q.susc$ref.class
# 
# 
# NGM=NGM_SIR(q=1,a,M,h)                        # compute the NGM
# Rs=colSums(NGM);Rs
# #NGM_1=NGM # copy to compare
# eigens=eigen_(NGM);eigens$eigens$values         # compute eigenvalues and eigenvectors
# #eigen_copy_1=max(eigens$eigens$values)
# sensi=sens(eigens)                              # compute R sensitivities towards Kij
# 
# sens_long<-melt(sensi$sens)                     # Plot sensitivities
# 
# ggplot(sens_long, aes(x = Var2, y = Var1)) +
#   geom_raster(aes(fill=value)) +
#   scale_fill_gradient(low="grey90", high="red") +
#   labs(title="Sensitivity matrix of R") +
#   theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
#                      axis.text.y=element_text(size=9),
#                      plot.title=element_text(size=11))
# 
# elas=elasti(sensi)                              # compute R elasticities towards Kij
# 
# elas_long<-melt(elas)                           # Plot elasticities
# 
# ggplot(elas_long, aes(x = Var2, y = Var1)) +
#   geom_raster(aes(fill=value)) +
#   scale_fill_gradient(low="grey90", high="red") +
#   labs(title="Elasticity matrix of R") +
#   theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
#                      axis.text.y=element_text(size=9),
#                      plot.title=element_text(size=11))
# 
# # infectivity
# 
# a=c(0.1021745, 0.6555298, 0.9311263, 1.1448274)
# #a=rep(1,4)
# lambda=2.5
# q=1
# 
# q.inf=q_inf(lambda = Ro,q = q,M = M,a = a,data = data,ref_class = ref_class)
# h=q.inf$h
# 
# NGM=NGM_SIR(q=1,a,M,h)                        # compute the NGM
# Rs=colSums(NGM);Rs
# #NGM_1=NGM # copy to compare
# eigens=eigen_(NGM);eigens$eigens$values         # compute eigenvalues and eigenvectors
# #eigen_copy_1=max(eigens$eigens$values)
# sensi=sens(eigens)                              # compute R sensitivities towards Kij
# 
# sens_long<-melt(sensi$sens)                     # Plot sensitivities
# 
# ggplot(sens_long, aes(x = Var2, y = Var1)) +
#   geom_raster(aes(fill=value)) +
#   scale_fill_gradient(low="grey90", high="red") +
#   labs(title="Sensitivity matrix of R") +
#   theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
#                      axis.text.y=element_text(size=9),
#                      plot.title=element_text(size=11))
# 
# elas=elasti(sensi)                              # compute R elasticities towards Kij
# 
# elas_long<-melt(elas)                           # Plot elasticities
# 
# ggplot(elas_long, aes(x = Var2, y = Var1)) +
#   geom_raster(aes(fill=value)) +
#   scale_fill_gradient(low="grey90", high="red") +
#   labs(title="Elasticity matrix of R") +
#   theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
#                      axis.text.y=element_text(size=9),
#                      plot.title=element_text(size=11))
