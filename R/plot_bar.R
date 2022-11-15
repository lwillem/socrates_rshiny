plot_bar = function(df,tit){
  out = ggplot(data=df)+
    geom_bar(aes(x=parameter,y=value),fill="royalblue",stat = "identity")+
    scale_y_continuous(limits = c(min(0,min(df$value)),max(0,max(df$value))),
                       breaks=df$value)+
    labs(title=tit)+
    theme_gdocs()
  return(out)
}