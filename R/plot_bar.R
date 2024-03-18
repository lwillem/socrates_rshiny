plot_bar = function(df,tit){
  out = ggplot(data=df)+
    facet_wrap(~indicator,scales="free")+
    geom_bar(aes(x=parameter,y=value),fill="grey68",stat = "identity")+
    labs(title=tit)+
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) 
    
  return(out)
}
