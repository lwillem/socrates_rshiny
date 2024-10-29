#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => TO CONDUCT THE NEXT-GENERATION APPROACH ANALYSIS
#
# Copyright 2024, Caetano, Willem et al.
#___________________________________________________________________________
# ref: Diekmann and Britton 2013 chapter 7
# ref: Franco et al, Plos Comput Biol, 2022

# help function to standardize the relative incidence
standardize_RI <- function(vec) {
  return(vec/sum(vec))
}

# Run the next generation approach based on an age-structured SIR model
#
# INPUTS:
#   M   social contact matrix
#   a   age-stratified q-susceptibility, 
#   h   age-stratified q-infectivity  
#   q   proportionality factor
#   p
#   nr_gen
#
# OUTPUT: next generation matrix (see Diekmann and Britton 2013 chapter 7)
run_NGA <- function(M,a,h,q,p,nr_gen){

  agegroups = gsub("infective_","", colnames(M))
  
  
  if(length(a)!=nrow(M) | length(h)!=nrow(M)){
    return(NA)
  }
  
  # the output of SOCRATES is not congruent the analysis (participant j contacts individuals of group i) 
  # this needs to be changed to the transpose i.e., individual of group i can has mij average contacts 
  # with group j
  M=t(M) 
  
  NGM=NGM_SIR(q=q,a=a,M=M,h=h)                        # compute the NGM
  eigens=eigen_(NGM)                                  # compute its eigenvalues
  bool_complex=is.complex(eigens$eigens$values)
  
  sensi=sens(eigens)                            # compute R sensitivities towards Kij
  elas=elasti(sensi)                            # compute R elasticities towards Kij
  
  Rs_=Rs(q=q,a=a,M=M,h=h)                       # sum of lines and columns of the NGM
  names(Rs_$Rs)=agegroups 
  Rs_$elas_kj=colSums(elas)
  
  age_analysis.df=data.frame(agegroups,
                             as.numeric(Rs_$Rs),
                             as.numeric(Rs_$Rr),
                             as.numeric(Rs_$elas_kj))
  colnames(age_analysis.df)=c("agegroup","k.j","ki.","elasticity")
  age_analysis.df <- age_analysis.df %>% 
                      select(c("agegroup","k.j","ki.","elasticity"))
  
  if (bool_complex==FALSE) {  # compute only sensitivities to w and RI if the eigenvalues are all real
    da=all_da(q = q,M = M,a = a,h = h,s = sensi)  # sensitivity and elasticity of Ro w.r.t a
    dh=all_dh(q = q,M = M,a = a,h = h,s = sensi)  # sensitivity and elasticity of Ro w.r.t h
    
    dwn=dw_n(eigens)                                # sensitivity of the right eigenvector (w) toward kij
    dwa=dw_a_all(dw_n = dwn,q=q,a=a,M=M,h=h,agegroup=agegroups) # sensitivity of w towards a
    dwh=dw_h_all(dw_n = dwn,q=q,a=a,M=M,h=h,agegroup=agegroups) # sensitivity of w towards h
    
    all.Gda=all_G_ratio_da(delta_a1=a*p,                      # RI for proportional perturbations on all entries of a
                           l=nr_gen,
                           da=da,
                           eigens=eigens,
                           agegroups=agegroups,
                           q=q,
                           a=a,
                           M=M,
                           h=h,
                           dwn=dwn)
    
    all.Gdh=all_G_ratio_dh(delta_h1=h*p,                      # RI for proportional perturbations on all entries of h
                           l=nr_gen,
                           dh=dh,
                           eigens=eigens,
                           agegroups=agegroups,
                           q=q,
                           a=a,
                           M=M,
                           h=h,
                           dwn=dwn)
    
    R_t = max(eigens$eigens$values)
  
  } else {
    R_t = NA
    all.Gda = NA
    all.Gdh = NA
  } # end if-else clause on complex eigen values
  
  # compile output list
  NGA=list(next_gen_matrix = NGM,
           elasticity_tbl = age_analysis.df,
           R_t = R_t,
           bool_complex=bool_complex,
           RI_a = all.Gda,
           RI_h = all.Gdh) 
  
  # return
  return(NGA)
}

# Builds the next generation matrix for the simple age-structured SIR model
# INPUT: vectors age-stratified q-susceptibility (a), q-infectivity (h) and social contact matrix (M) and proportionality factor q
# OUTPUT: next generation matrix (see Diekmann and Britton 2013 chapter 7)
NGM_SIR = function(q,a,M,h){

  # TODO: issue with age-group names  
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

# Calculates eigen values and eigen vectors for both A and A^T, constrains left (v) and right (w) eigen vector
# such that <v_i,w_i> = 1 and <v_i,w_j> = 0, i!=j.
#INPUT: nonnegative square matrix A
#OUTPUT: eigenvalues of A and eigen vectors for both A and A^T
eigen_ = function(A,norm=T){
  
  R = eigen(A)
  colnames(R$vectors)=c("dominant",as.character(seq(2,ncol(R$vectors))))

  # if the eigenvalue is NOT complex
  if(!is.complex(R$vectors)){
    
    # make sure the dominant right eigenvalue is positive
    if(all(R$vectors[,1]<0)){
      R$vectors[,1]=R$vectors[,1]*-1 
    }
    
    # divide each entry of the right dominant eigenvector w by the sum of the entries in order to be
    # interpreted as the stable population
    if(norm==T){
      R$vectors[,1]=R$vectors[,1]/sum(R$vectors[,1]) # normalize such that ||w|| = 1
    }
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


# calculates elasticities to entries of the matrix
#INPUT: sens list from sens function
#OUTPUT: elasticity matrix
elasti = function(sens){

  Rt = max(abs(sens$eigens$values))
  A = sens$A
  sens_M = sens$sens

  E = (1/Rt)*sens_M*A
  
  return(E)
}

# Computes sensitivity of eigenvalues with respect to entries of matrix A (see caswell)
# INPUT: list of the type "eigens" output of function eigen_ and tolerance (tol) of the approximation of w^Tv to the Identity matrix
# OUTPUT: list of type sens with original matrix, eigenvalues and sensitivity matrix
# check if <v_i,w_i> = 1
sens = function(list,tol=1e-07){
  
  lower=t(list$eigens$w[,"dominant"]) %*% list$eigens$v[,"dominant"]-tol
  top=t(list$eigens$w[,"dominant"]) %*% list$eigens$v[,"dominant"]+tol
  bol_=between(1,lower,top)
  
  
  if (bol_!=T) {
    stop("dominant eigenvectors have dot product different than 1")
  }
  
  sens=list$eigens$v[,"dominant"] %*% t(list$eigens$w[,"dominant"]) # vw^T
  colnames(sens) <- colnames(list$A)
  rownames(sens) <- rownames(list$A)
  sens_l = list(A=list$A,eigens=list$eigens,sens=sens)
  
  return(sens_l)
}

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

#calculates the sensitivity and elasticity of Rt for all lower-level parameters a_i
#INPUT: parameters q (proportionality factor), a (q-susceptibility), M (contact rates matrix), h (infectivity), s (sensitivity object)
#OUTPUT: Rt and total sensitivity and elasticity of all a_i
all_da=function(q,a,M,h,s){
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

#calculates the sensitivity and elasticity of Rt on lower-level parameter a_i
#INPUT: parameters q,a,M,h, sensitivity of the NGM and index i (a_1 then pos=1)
#OUTPUT:total sensitivity and elasticity of a_i towards R
da_=function(q,a,M,h,s,pos){

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

#calculates the sensitivity and elasticity of Rt for all lower-level parameters h_i
#INPUT: parameters q (proportionality factor), a (q-susceptibility), M (contact rates matrix), h (infectivity), s (sensitivity object)
#OUTPUT: Rt and total sensitivity and elasticity of all h_i
all_dh=function(q,a,M,h,s){
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

#calculates the sensitivity and elasticity of Rt on lower-level parameter h_i
#INPUT: parameters q,a,M,h, sensitivity of the NGM and index i (h_1 then pos=1)
#OUTPUT:total sensitivity and elasticity of h_i towards R
dh_=function(q,a,M,h,s,pos){
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

#calculates the sensitivity of the right eigenvector w on each matrix entry kij
#INPUT: eigen value object from eigen_ function (e)
#OUTPUT: list with sensitivity vector corresponding to changes to each matrix entry kij
dw_=function(e){
  
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

#calculates the sensitivity of the normalized right eigenvector w on each matrix entry kij
#INPUT: eigen value object from eigen_ function (e)
#OUTPUT: list with sensitivity vector corresponding to changes to each matrix entry kij
dw_n=function(e){
  
  w=e$eigens$w[,"dominant"]
  dw=dw_(e)
  
  out=map(dw,function(x){x-w*sum(x)}) 
  return(out)
}

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

all_G_ratio_da=function(delta_a1,l,da,eigens,agegroups,q,a,M,h,dwn){
  
  # define age groups
  pos=seq(1,nrow(eigens$eigens$w))
  
  # help function
  G_aux=function(x,y){
    aux=G.ratio_da(pos=x,delta_a1=y,l,da,eigens,q=q,a=a,M=M,h=h,dwn=dwn)
    return(aux)
  }
  
  # get results
  G.ratio.da=map2(1:nrow(eigens$eigens$w),delta_a1,G_aux)
  names(G.ratio.da)=c(paste0("a_",as.character(pos)))
   
  # reformat and add age of infectee
  G.ratio.da=as.data.frame(G.ratio.da)
  G.ratio.da$age.infectee=agegroups
   
  # reshape into long table
  G.ratio.da %>%
    pivot_longer(-age.infectee)->G.ratio.da
  
  # add age.infector
  G.ratio.da <- G.ratio.da %>%
                  mutate(name = agegroups[as.numeric(sub('a_','',name))]) %>%
                  rename(age.infector =  name)
                  
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

all_G_ratio_dh=function(delta_h1,l,dh,eigens,agegroups,q,a,M,h,dwn){
  
  
  # define age groups
  pos=seq(1,nrow(eigens$eigens$w))
  
  # help function
  G_aux=function(x,y){
    aux=G.ratio_dh(pos=x,delta_h1=y,l,dh,eigens,q=q,a=a,M=M,h=h,dwn=dwn)
    return(aux)
  }
  
  # get results
  G.ratio.dh=map2(1:nrow(eigens$eigens$w),delta_h1,G_aux)
  names(G.ratio.dh)=c(paste0("h_",as.character(pos)))
  
  # reformat and add age of infectee 
  G.ratio.dh=as.data.frame(G.ratio.dh)
  G.ratio.dh$age.infectee=agegroups

  # reshape into long table
  G.ratio.dh %>%
    pivot_longer(-age.infectee)->G.ratio.dh

  # add age.infector
  G.ratio.dh <- G.ratio.dh %>%
    mutate(name = agegroups[as.numeric(sub('h_','',name))]) %>%
    rename(age.infector =  name)
  
  return(G.ratio.dh)
}