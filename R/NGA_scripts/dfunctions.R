dbeta_=function(beta,gamma,C,N,S,s,pos){
  #calculates the sensitivity and elasticity of Rt on lower-level parameter beta_i
  #INPUT: parameters gamma,C,N,s, sensitivity and elasticity of the NGM and index i (beta_1 then pos=1)
  #OUTPUT: sensitivity of Rt and total sensitivity and elasticity of beta_i
  
  Rt = max(abs(s$eigens$values))
  s = s$sens
  
  n_row=nrow(s)
  n_col=ncol(s)
  
  dbeta_M = matrix(0,nrow = n_row,ncol=n_col)
  dbeta_M[pos,]=1
  
  gamma_ = diag(1/gamma,nrow=n_row,ncol=n_col)
  S_ = diag(S,nrow=n_row,ncol=n_col)
  N_inv= diag(1/N,nrow=n_row,ncol=n_col)
  
  dbeta = gamma_ %*% S_ %*% C %*% N_inv
  
  dK_dbeta = dbeta_M*dbeta
  
  aux=s * dK_dbeta
  
  sensitivity= sum(aux)
  elasticity = (beta[pos]/Rt)*sensitivity
  
  return(list(sen=sensitivity,
              ela=elasticity))
  
}

all_dbeta=function(beta,gamma,C,N,S,s){
  #calculates the sensitivity and elasticity of Rt for all lower-level parameters beta_i
  #INPUT: parameters gamma,C,N,s, sensitivity and elasticity of the NGM
  #OUTPUT: Rt and total sensitivity and elasticity of all beta_i
  pos=seq(1,nrow(s$sens))
  
  dbeta_aux=function(x){
    aux=dbeta_(beta=beta,gamma=gamma,C=C,N=N,S=S,s=s,pos=x)
    return(aux)
  }
  
  out=map(pos,dbeta_aux)
  names(out)=c(paste0("beta_",as.character(pos)))
  
  out=as.data.frame(out)
  out %>%
    pivot_longer(everything()) %>%
    separate(name,c("parameter","indicator"),sep=7)->out
  return(out)
}

dS_=function(beta,gamma,C,N,S,s,pos){
  #calculates the sensitivity and elasticity of Rt on lower-level parameter S_i
  #INPUT: parameters gamma,C,N,s, sensitivity and elasticity of the NGM and index i (S_1 then pos=1)
  #OUTPUT: sensitivity of Rt and total sensitivity and elasticity of S_i
  
  Rt = max(abs(s$eigens$values))
  s = s$sens
  
  n_row=nrow(s)
  n_col=ncol(s)
  
  dS_M = matrix(0,nrow = n_row,ncol=n_col)
  dS_M[pos,]=1
  
  beta_ = diag(beta,nrow=n_row,ncol=n_col)
  gamma_ = diag(1/gamma,nrow=n_row,ncol=n_col)
  N_inv= diag(1/N,nrow=n_row,ncol=n_col)
  
  dS = beta_ %*% gamma_ %*% C %*% N_inv
  
  dK_dS = dS_M * dS
  
  aux = s * dK_dS
  
  sensitivity= sum(aux)
  elasticity = (S[pos]/Rt)*sensitivity
  
  return(list(sen=sensitivity,
              ela=elasticity))
  
}

all_dS=function(beta,gamma,C,N,S,s){
  #calculates the sensitivity and elasticity of Rt for all lower-level parameters S_i
  #INPUT: parameters gamma,C,N,s, sensitivity and elasticity of the NGM
  #OUTPUT: Rt and total sensitivity and elasticity of all S_i
  pos=seq(1,nrow(s$sens))
  
  dS_aux=function(x){
    aux=dS_(beta=beta,gamma=gamma,C=C,N=N,S=S,s=s,pos=x)
    return(aux)
  }
  
  out=map(pos,dS_aux)
  names(out)=c(paste0("S_",as.character(pos)))
  out=as.data.frame(out)
  out %>%
    pivot_longer(everything()) %>%
    separate(name,c("parameter","indicator"),sep=4)->out
  return(out)
}


dgamma_=function(beta,gamma,C,N,S,s,pos){
  #calculates the sensitivity and elasticity of Rt on lower-level parameter gamma_i
  #INPUT: parameters gamma,C,N,s, sensitivity and elasticity of the NGM and index i (gamma_1 then pos=1)
  #OUTPUT: sensitivity and elasticity of Rt and total sensitivity of gamma_i
  
  Rt = max(abs(s$eigens$values))
  s = s$sens
  
  n_row=nrow(s)
  n_col=ncol(s)
  
  dgamma_M = matrix(0,nrow = n_row,ncol=n_col)
  dgamma_M[pos,]=1
  
  beta_ = diag(beta,nrow=n_row,ncol=n_col)
  gamma_ = diag(-1/(gamma^2),nrow=n_row,ncol=n_col)
  N_inv= diag(1/N,nrow=n_row,ncol=n_col)
  S_ = diag(S,nrow=n_row,ncol=n_col)
  
  dgamma = beta_ %*% gamma_ %*% S_ %*% C %*% N_inv
  
  dK_dgamma = dgamma_M * dgamma
  
  aux = s * dK_dgamma
  
  sensitivity= sum(aux)
  elasticity = (gamma[pos]/Rt)*sensitivity
  
  return(list(sen=sensitivity,
              ela=elasticity))
  
}

all_dgamma=function(beta,gamma,C,N,S,s){
  #calculates the sensitivity and elasticity of Rt for all lower-level parameters gamma_i
  #INPUT: parameters gamma,C,N,s, sensitivity and elasticity of the NGM
  #OUTPUT: Rt and total sensitivity and elasticity of all gamma_i
  pos=seq(1,nrow(s$sens))
  
  dgamma_aux=function(x){
    aux=dgamma_(beta=beta,gamma=gamma,C=C,N=N,S=S,s=s,pos=x)
    return(aux)
  }
  
  out=map(pos,dgamma_aux)
  names(out)=c(paste0("gamma_",as.character(pos)))
  
  out=as.data.frame(out)
  out %>%
    pivot_longer(everything()) %>%
    separate(name,c("parameter","indicator"),sep=8)->out
  
  return(out)
}


dN_=function(beta,gamma,C,N,S,s,pos){
  #calculates the sensitivity and elasticity of Rt on lower-level parameter N_j
  #INPUT: parameters gamma,C,N,s, sensitivity and elasticity of the NGM and index j (N_1 then pos=1)
  #OUTPUT: sensitivity and elasticity of Rt and total sensitivity of N_j
  
  Rt = max(abs(s$eigens$values))
  s = s$sens
  
  n_row=nrow(s)
  n_col=ncol(s)
  
  dN_M = matrix(0,nrow = n_row,ncol=n_col)
  dN_M[,pos]=1
  
  beta_ = diag(beta,nrow=n_row,ncol=n_col)
  gamma_ = diag(1/gamma,nrow=n_row,ncol=n_col)
  N_inv= diag(-1/(N^2),nrow=n_row,ncol=n_col)
  S_ = diag(S,nrow=n_row,ncol=n_col)
  
  dN = beta_ %*% gamma_ %*% S_ %*% C %*% N_inv
  
  dK_dN = dN_M * dN
  
  aux = s * dK_dN
  
  sensitivity= sum(aux)
  elasticity = (N[pos]/Rt)*sensitivity
  
  return(list(sen=sensitivity,
              ela=elasticity))
  
  return(dRt_dN)
  
}

all_dN=function(beta,gamma,C,N,S,s){
  #calculates the sensitivity and elasticity of Rt for all lower-level parameters N_j
  #INPUT: parameters gamma,C,N,s, sensitivity and elasticity of the NGM
  #OUTPUT: Rt and total sensitivity and elasticity of all N_j
  pos=seq(1,nrow(s$sens))
  
  dN_aux=function(x){
    aux=dN_(beta=beta,gamma=gamma,C=C,N=N,S=S,s=s,pos=x)
    return(aux)
  }
  
  out=map(pos,dN_aux)
  names(out)=c(paste0("N_",as.character(pos)))
  
  out=as.data.frame(out)
  out %>%
    pivot_longer(everything()) %>%
    separate(name,c("parameter","indicator"),sep=4)->out
  
  return(out)
}
