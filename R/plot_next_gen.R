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
    labs(title="",x="Infector",y="Infectee",fill="Infections") +
    theme_bw(base_size = 20) + theme(axis.text.x=element_text(angle=45, vjust=0.6),
                                     axis.text.y=element_text(),
                                     plot.title=element_text(),legend.position = "right")
  
  return(p)
}


plot_NGA_elas = function(Rs_,eigens,agegroups){
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
    annotate("text", x = 0.5, y = Ro+Ro*0.15, label = "R",size=7)+
    labs(title="",x="Age group",fill="",y="")+
    scale_y_continuous(name=paste0("k.j, ki. and R"),breaks=scales::pretty_breaks(n=8),expand=c(0,0),limits=c(0,aux),
                       sec.axis = sec_axis( trans=~./(aux), name="Elasticity"))+
    scale_fill_economist()+
    theme_bw(base_size = 20) + theme(axis.text.x=element_text(angle=45, vjust=0.6),
                                     axis.text.y=element_text(),
                                     axis.title.x = element_text(margin = margin(t = 15)),  # Increase space between x-label and axis
                                     plot.title=element_text(),
                                     legend.position = "right") 
    # coord_fixed(ratio = 0.4)  # Stretch the plot horizontally
  #p
  
  return(p)
}

plot_NGA_RI = function(NGA,bool_susceptibility=TRUE){
# plot_G_a = function(G.ratio.da,bool_complex){
  if (NGA$bool_complex==FALSE) {
    
    if(bool_susceptibility){
      output_tag <- 'Susceptibility'
      output_matrix <- NGA$RI_a
    } else{
      output_tag <- 'Infectivity'
      output_matrix <- NGA$RI_h
    }
    
    # remove tag from name to make it uniform (a_1 and h_1 will be 1)
    output_matrix$name <- gsub('._','',output_matrix$name)
    
    # add age group to x-axis labels
    db_agegroup <- data.frame(name_agegroup = NGA$agegroups)
    db_agegroup$name <- paste0(1:nrow(db_agegroup))
    G.ratio.da <- merge(db_agegroup,output_matrix,by='name')
    
    # plot title
    plot_title <- paste0('Given a ',abs(round(NGA$p*100)),'% ',ifelse(NGA$p<0,'reduction','increase'), ' to q-',tolower(output_tag),' (m=',NGA$nr_gen,')')

    # plot
    p=ggplot(G.ratio.da, aes(x = name_agegroup, y = agegroup)) +
      geom_tile(aes(fill=value)) +
      geom_text(aes(label=round(value,3))) +
      scale_fill_gradient2(limits=c(min(G.ratio.da$value),max(G.ratio.da$value)),midpoint = 1, low = "blue", mid = "white",
                           high = "red", space = "Lab") +
      # labs(title=plot_title,x=output_tag,y="Age group",fill="RI") +
      labs(title=plot_title,x="Infector",y="Infectee",fill="Relative\nImpact") +
      theme_bw(base_size=16) + theme(axis.text.x=element_text(size=16, angle=45, vjust=0.6),
                                     axis.text.y=element_text(size=16),
                                     plot.title=element_text(size=16))
  
    }
  if (NGA$bool_complex==TRUE) {
    p="Error: complex eigenvalues found. The calculation of the RI does not yet account for complex eigenvalues."
  }
  return(p)
}


