plot_bar <- function(vector,x_lab="",y_lab="",scale_max=10){

  
  bplt <- barplot(vector,
                  xlab=x_lab,
                  ylab=y_lab,
                  ylim=range(pretty(vector*1.1),scale_max,0,na.rm=T),
                  col=rgb(0.2,0.4,0.6,0.6),
                  cex.names =  0.8,
                  width=c(1,rep(0.5,length(vector)-1)),main="Reproduction numbers")
  text(x = bplt,
       y = vector,
       labels = round(vector,digits=2),
       pos=3)
}
plot_bar2 = function(df,tit){
  out = ggplot(data=df)+
    facet_wrap(~indicator)+
    geom_bar(aes(x=parameter,y=value),fill="royalblue",stat = "identity")+
    geom_text(aes(x=parameter,y=value,label=round(value,2)), position=position_dodge(width=0.9), vjust=-0.25)+
    labs(title=tit)+
    scale_y_continuous(breaks=scales::pretty_breaks(n=10))+
    theme_bw(base_size = 12) +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) 
  
  return(out)
}
