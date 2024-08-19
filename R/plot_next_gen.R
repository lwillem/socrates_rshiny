#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => PLOT SOCIAL CONTACT MATRICES
#
#  Copyright 2024, SIMID
#___________________________________________________________________________

plot_NGM = function(NGM){
  NGM_long<-reshape2::melt(NGM)                     # Plot NGM
  
  p=ggplot(NGM_long, aes(x = Var2, y = Var1)) +
    geom_raster(aes(fill=value)) +
    geom_text(aes(label=round(value,2)))+
    scale_fill_gradient(low="grey90", high="red") +
    labs(title="NGM",x="Infector",y="Infectee",fill="Infections") +
    theme_bw(base_size = 20) + theme(axis.text.x=element_text(angle=45, vjust=0.6),
                                     axis.text.y=element_text(),
                                     plot.title=element_text(),legend.position = "right")
  
  return(p)
}


plot_elas = function(Rs_,eigens,agegroups){
  Ro=max(eigens$eigens$values)
  
  age_analysis.df=data.frame(agegroups,
                             as.numeric(Rs_$Rs),
                             as.numeric(Rs_$Rr),
                             as.numeric(Rs_$elas_kj))
  colnames(age_analysis.df)=c("agegroup","k.j","ki.","elasticity")
  age_analysis.df %>% select(c("agegroup","k.j","ki.","elasticity"))-> age_analysis.df
  
  aux=max(Ro+1,max(Rs_$Rs+1),max(Rs_$Rr+1))
  
  age_analysis.df$elasticity=age_analysis.df$elasticity*aux #rescale to fit the figure
  
  age_analysis.df %>% pivot_longer(-agegroup)->age_analysis.df
  age_analysis.df$name <- factor(age_analysis.df$name, levels=c("k.j","ki.","elasticity"))
  
  p=ggplot()+
    geom_bar(data=age_analysis.df,aes(x=agegroup,y=value,fill=name),stat = "identity",position = "dodge")+
    geom_hline(yintercept=Ro,linetype="dashed")+
    annotate("text", x = age_analysis.df$agegroup[1], y = Ro+Ro*0.2, label = "R",size=7)+
    labs(x="",fill="",y="")+
    scale_y_continuous(name=paste0("Transmission parameters"),breaks=scales::pretty_breaks(n=8),expand=c(0,0),limits=c(0,aux),
                       sec.axis = sec_axis( trans=~./(aux), name="Elasticity"))+
    scale_fill_economist()+
    theme_bw(base_size = 20) + theme(axis.text.x=element_text(angle=45, vjust=0.6,face = "bold"),
                                     axis.text.y=element_text(),
                                     plot.title=element_text(),legend.position = "top")
  
  return(p)
}


plot_G_a = function(G.ratio.da,bool){
  if (bool==FALSE) {
  p=ggplot(G.ratio.da, aes(x = name, y = agegroup)) +
    geom_tile(aes(fill=value)) +
    geom_text(aes(label=round(value,3))) +
    scale_fill_gradient2(limits=c(min(G.ratio.da$value),max(G.ratio.da$value)),midpoint = 1, low = "blue", mid = "white",
                         high = "red", space = "Lab") +
    labs(title="RI",x="susceptibility",y="age-group",fill="RI") +
    theme_bw(base_size=16) + theme(axis.text.x=element_text(size=16, angle=0, vjust=0.3),
                                   axis.text.y=element_text(size=16),
                                   plot.title=element_text(size=16))
  }
  if (bool==TRUE) {
    p="Error: complex eigenvalues found. The calculation of the RI does not yet account for complex eigenvalues."
  }
  return(p)
}

plot_G_h = function(G.ratio_dh_est,bool){
  if (bool==FALSE) {
    p=  
      ggplot(G.ratio_dh_est, aes(x = name, y = agegroup)) +
      geom_tile(aes(fill=value)) +
      geom_text(aes(label=round(value,3))) +
      scale_fill_gradient2(limits=c(min(G.ratio_dh_est$value),max(G.ratio_dh_est$value)),midpoint = 1, low = "blue", mid = "white",
                           high = "red", space = "Lab") +
      labs(title="RI",x="infectivity",y="age-group",fill="RI") +
      theme_bw(base_size=16) + theme(axis.text.x=element_text(size=16, angle=0, vjust=0.3),
                                     axis.text.y=element_text(size=16),
                                     plot.title=element_text(size=16)) 
  }
  if (bool==TRUE) {
    p="Error: complex eigenvalues found. The calculation of the RI does not yet account for complex eigenvalues."
  }
  return(p)
}


