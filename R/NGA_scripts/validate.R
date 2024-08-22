# validate RI

num_val_a = function(delta_a1,nr_gen,I0,NGM,pos,da,eigens){
  a2=a
  a2[pos]=a[pos]+delta_a1
  NGM2=NGM_SIR(q=q,a2,M,h)                        # compute the NGM
  eigens2=eigen_(NGM2)
  
  I_before=matrix.power(NGM,nr_gen)%*%I0
  
  I_after=matrix.power(NGM2,nr_gen)%*%I_before
  I_contrafactual=matrix.power(NGM,nr_gen)%*%I_before
  exact=as.vector(I_after/I_contrafactual)
  #print(exact)
  approx=G.ratio_da(pos=pos,delta_a1=delta_a1,l=nr_gen,da=da,eigens=eigens)
  # print("pos")
  # print(pos)
  # print("delta_a1")
  # print(delta_a1)
  # print("nr_gen")
  # print(nr_gen)
  # print("da")
  # print(da)
  # print("approx")
  # print(approx)
  
  error=(1/length(approx))*sum(abs(exact-approx))
  return(error)
}

all_num_val_a = function(delta_a1_max,delta_a1_min,delta_a1_step,nr_gen_min,nr_gen_max,I0,NGM,pos,da,eigens){
  
  delta_a1=a[pos]*seq(delta_a1_min,delta_a1_max,delta_a1_step)
  p=delta_a1/a[pos]
  nr_gen=seq(nr_gen_min,nr_gen_max,1)
  
  
  
  error=c()
  delta.a1=c()
  nr.gen=c()
  cont=0
  
  for (i in nr_gen) {
    for (j in delta_a1) {
      cont=cont+1
      delta.a1[cont]=j
      nr.gen[cont]=i
      error[cont]=num_val_a(delta_a1=j,nr_gen=i,I0 = I0,NGM = NGM,pos = pos,da = da,eigens = eigens)
    }
    
  }
  val.df=data.frame(delta.a1,p,nr.gen,error)
  return(val.df)
}

num_val_h = function(delta_h1,nr_gen,I0,NGM,pos,dh,eigens){
  h2=h
  h2[pos]=h[pos]+delta_h1
  NGM2=NGM_SIR(q=q,a,M,h2)                        # compute the NGM
  eigens2=eigen_(NGM2)
  
  I_before=matrix.power(NGM,nr_gen)%*%I0
  
  I_after=matrix.power(NGM2,nr_gen)%*%I_before
  I_contrafactual=matrix.power(NGM,nr_gen)%*%I_before
  exact=as.vector(I_after/I_contrafactual)
  approx=G.ratio_dh(pos=pos,delta_h1=delta_h1,l=nr_gen,dh=dh,eigens=eigens)

  
  error=(1/length(approx))*sum(abs(exact-approx))
  return(error)
}

all_num_val_h = function(delta_h1_max,delta_h1_min,delta_h1_step,nr_gen_min,nr_gen_max,I0,NGM,pos,dh,eigens){
  
  delta_h1=h[pos]*seq(delta_h1_min,delta_h1_max,delta_h1_step)
  p=delta_h1/h[pos]
  nr_gen=seq(nr_gen_min,nr_gen_max,1)
  
  
  
  error=c()
  delta.h1=c()
  nr.gen=c()
  cont=0
  
  for (i in nr_gen) {
    for (j in delta_h1) {
      cont=cont+1
      delta.h1[cont]=j
      nr.gen[cont]=i
      error[cont]=num_val_h(delta_h1=j,nr_gen=i,I0 = I0,NGM = NGM,pos = pos,dh = dh,eigens = eigens)
    }
    
  }
  val.df=data.frame(delta.h1,p,nr.gen,error)
  return(val.df)
}
