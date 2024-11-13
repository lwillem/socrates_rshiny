#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => PLOT SOCIAL CONTACT MATRICES
#
#  Copyright 2024, Caetano, Willem et al.
#___________________________________________________________________________

plot_next_gen_matrix = function(next_gen_matrix){
  next_gen_matrix_long<-reshape2::melt(next_gen_matrix)                     # Plot next_gen_matrix
  
  p=ggplot(next_gen_matrix_long, aes(x = Var2, y = Var1)) +
    geom_raster(aes(fill=value)) +
    geom_text(aes(label=round(value,2)))+
    scale_fill_gradient(low="grey90", high="red") +
    labs(title="",x="Age infector (year)",y="Age infectee  (year)",fill="Infections") +
    theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=45, vjust=0.6),
                                     axis.text.y=element_text(),
                                     plot.title=element_text(),legend.position = "right")
  
  return(p)
}


plot_NGA_elas = function(R_t,elasticity_tbl){
  
  # obtain rescaling factor
  aux=max(R_t,unlist(elasticity_tbl[,-1])) + 1
  
  elasticity_tbl$elasticity=elasticity_tbl$elasticity*aux #rescale to fit the figure
  
  elasticity_tbl %>% pivot_longer(-agegroup)->elasticity_tbl
  elasticity_tbl$name <- factor(elasticity_tbl$name, levels=c("k.j","ki.","elasticity"))
  
  p=ggplot()+
    geom_bar(data=elasticity_tbl,aes(x=agegroup,y=value,fill=name),stat = "identity",position = "dodge")+
    geom_hline(yintercept=R_t,linetype="dashed")+
    annotate("text", x = 0.75, y = R_t+R_t*0.1, label = "R",size=7)+
    labs(title="",x="Age group (year)",fill="",y="")+
    scale_y_continuous(name=paste0("k.j, ki. and R"),breaks=scales::pretty_breaks(n=8),expand=c(0,0),limits=c(0,aux),
                       sec.axis = sec_axis( trans=~./(aux), name="Elasticity"))+
    scale_fill_economist()+
    theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=45, vjust=0.6),
                                     axis.text.y=element_text(),
                                     axis.title.x = element_text(margin = margin(t = 15)),  # Increase space between x-label and axis
                                     plot.title=element_text(),
                                     legend.position = "right") 

  return(p)
}

plot_NGA_RI = function(NGA, delta_p, rn_gen, bool_susceptibility=TRUE, round_digits = 3){

  if (!NGA$bool_complex) {
    
    if(bool_susceptibility){
      output_tag <- 'Susceptibility'
      G.ratio.da <- NGA$RI_a
    } else{
      output_tag <- 'Infectivity'
      G.ratio.da <- NGA$RI_h
    }
    
    # make sure delta_p is numeric
    delta_p <- as.numeric(delta_p)
    
    # plot title
    plot_title <- paste0('Given a ',abs(round(delta_p*100)),'% ',ifelse(delta_p<0,'reduction','increase'), ' to q-',tolower(output_tag),' (m=',rn_gen,')')
    
    # plot
    p=ggplot(G.ratio.da, aes(x = age.infector, y = age.infectee)) +
      geom_tile(aes(fill=value)) +
      geom_text(aes(label=round(value,round_digits))) +
      scale_fill_gradient2(limits=c(min(G.ratio.da$value),max(G.ratio.da$value)),midpoint = 1, low = "blue", mid = "white",
                           high = "red", space = "Lab") +
      labs(title=plot_title,x="Age infector (year)",y="Age infectee (year)",fill="Relative\nImpact") +
      theme_bw(base_size=16) + theme(axis.text.x=element_text(size=16, angle=45, vjust=0.6),
                                     axis.text.y=element_text(size=16),
                                     plot.title=element_text(size=16))
  } else {
    p = "Error: complex eigenvalues found. The calculation of the RI does not yet account for complex eigenvalues."
  }
  return(p)
}
