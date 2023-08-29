

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}


graph_sector_iam<-function(dt, countryxwalk, modelxwalk, my_country, my_scenario ,my_metric,x_lab,graphpath,beamersave=0){
  dt.iam.small<-dt[ country==my_country& scenario.short==my_scenario]
  dt.iam.smallav<-dt.iam.small[,.(meanrank=mean(get(my_metric),na.rm=T)), .(country, scenario.short, sector)]
  dt.iam.smallav<-dt.iam.smallav[order(meanrank)]
  dt.iam.smallav[,rank:=1:.N]
  
  top5_tab<-dt.iam.smallav[rank<=5,.(rank,sector)]
  bottom5_tab<-dt.iam.smallav[rank>=N-5,.(rank,sector)]
  
  toptab<-dt.iam.smallav[,.(rank,sector)]
  
  dt.iam.small<-merge(dt.iam.small, dt.iam.smallav[,.(sector, country, rank)], by=c("sector","country"))
  dt.iam.small<-dt.iam.small[order(rank)]
  
  
  my_country_name<-unique(countryxwalk[country.exiobase==my_country,country])
  my_scenario_name<-unique(modelxwalk[scenario.short==my_scenario,Scenario])
  
  plot1<-ggplot(dt.iam.small) +theme_minimal()+
    theme(plot.title = element_text(face="bold"))+
    xlab("Industry")+scale_fill_discrete(name="IAM source")+ggtitle(paste0(my_country_name, "\n", my_scenario_name))+
    ylab(x_lab)+xlab("Sector")
  

  if (beamersave == 0){
    plot1<-plot1+geom_bar(stat="identity", position = "dodge", aes(x=reorder(sector, -rank), y=get(my_metric), fill=model.short))
    
    plot1<- plot1  +coord_flip()+theme(legend.direction='vertical')
    
    ggsave(paste0(graphpath,my_country,"_",my_scenario,"_",my_metric,".png"),plot1,width=8,height=11)
    print(plot1)
  }  else {
    plot1<-plot1+geom_bar(stat="identity", position = "dodge", aes(x=reorder(rank, rank), y=get(my_metric), fill=model.short))
    plot1<-plot1+theme(legend.direction='horizontal')
    #plot1<-plot1+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=.8))
    

    
    mylegend<-g_legend(plot1)
    tt3 <- ttheme_minimal(base_size=8,    core = list(padding=unit(c(1, 1), "mm")))
    
    legend2<-tableGrob(toptab,theme=tt3, cols = c("",paste0("")),rows=NULL)
    
    
    plot2<-grid.arrange(plot1+ theme(legend.position="none"), mylegend,legend2,ncol = 2,layout_matrix = cbind(c(1,1,1,1,1,1,1), c(3,3,3,3,3,3,2)))
    
    
   # ggsave(paste0(graphpath,my_country,"_",my_scenario,"_",my_metric,"_beamer.png"),plot2,width=126*2,height=96*2, units="mm")
    ggsave(paste0(graphpath,my_country,"_",my_scenario,"_",my_metric,"_beamer.png"),plot2,width=126*2,height=60*2, units="mm")
    
    
    print(plot2)
  }
  
  
}



graph_sector_iam.2<-function(dt, countryxwalk, modelxwalk, my_country, scenario_label ,my_metric,x_lab,graphpath,beamersave=0){
  #use this version after 9/23/23. Making it so we run the summary stats function and then display this
  # so scenario is instead already taken into account. 
   dt.iam.small<-dt[ country==my_country]
  dt.iam.smallav<-dt.iam.small[,.(meanrank=mean(get(my_metric),na.rm=T)),
                               .(country, sector)]
  dt.iam.smallav<-dt.iam.smallav[order(meanrank)]
  dt.iam.smallav[,rank:=1:.N]
  
  top5_tab<-dt.iam.smallav[rank<=5,.(rank,sector)]
  bottom5_tab<-dt.iam.smallav[rank>=N-5,.(rank,sector)]
  
  toptab<-dt.iam.smallav[,.(rank,sector)]
  
  dt.iam.small<-merge(dt.iam.small, dt.iam.smallav[,.(sector, country, rank)], by=c("sector","country"))
  dt.iam.small<-dt.iam.small[order(rank)]
  
  
  my_country_name<-unique(countryxwalk[country.exiobase==my_country,country])
 #my_scenario_name<-unique(modelxwalk[scenario.short==my_scenario,Scenario])
  
  plot1<-ggplot(dt.iam.small) +theme_minimal()+
    theme(plot.title = element_text(face="bold"))+
    xlab("Industry")+scale_fill_discrete(name="IAM source")+ggtitle(paste0(my_country_name, "\n", scenario_label))+
    ylab(x_lab)+xlab("Sector")
  
  
  if (beamersave == 0){
    plot1<-plot1+geom_bar(stat="identity", position = "dodge", aes(x=reorder(sector, -rank), y=get(my_metric), fill=model.short))
    
    plot1<- plot1  +coord_flip()+theme(legend.direction='vertical')
    
    ggsave(paste0(graphpath,my_country,"_",scenario_label,"_",my_metric,".png"),plot1,width=8,height=11)
    print(plot1)
  }  else {
    plot1<-plot1+geom_bar(stat="identity", position = "dodge", aes(x=reorder(rank, rank), y=get(my_metric), fill=model.short))
    plot1<-plot1+theme(legend.direction='horizontal')
    #plot1<-plot1+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=.8))
    
    
    
    mylegend<-g_legend(plot1)
    tt3 <- ttheme_minimal(base_size=8,    core = list(padding=unit(c(1, 1), "mm")))
    
    legend2<-tableGrob(toptab,theme=tt3, cols = c("",paste0("")),rows=NULL)
    
    
    plot2<-grid.arrange(plot1+ theme(legend.position="none"), mylegend,legend2,ncol = 2,layout_matrix = cbind(c(1,1,1,1,1,1,1), c(3,3,3,3,3,3,2)))
    
    
    # ggsave(paste0(graphpath,my_country,"_",my_scenario,"_",my_metric,"_beamer.png"),plot2,width=126*2,height=96*2, units="mm")
    ggsave(paste0(graphpath,my_country,"_",scenario_label,"_",my_metric,"_beamer.png"),plot2,width=126*2,height=60*2, units="mm")
    
    
    print(plot2)
  }
  
  
}





graph_sector_iam_beamer<-function(dt, countryxwalk, modelxwalk, my_country, my_scenario ,my_metric,x_lab,graphpath,beamersave=0){
  dt.iam.small<-dt[ country==my_country& scenario.short==my_scenario]
  dt.iam.smallav<-dt.iam.small[,.(meanrank=mean(get(my_metric),na.rm=T)), .(country, scenario.short, sector)]
  dt.iam.smallav<-dt.iam.smallav[order(meanrank)]
  dt.iam.smallav[,rank:=1:.N]
  
  top5_tab<-dt.iam.smallav[rank<=5,.(rank,sector)]
  bottom5_tab<-dt.iam.smallav[rank>=N-5,.(rank,sector)]
  
  
  dt.iam.small<-merge(dt.iam.small, dt.iam.smallav[,.(sector, country, rank)], by=c("sector","country"))
  dt.iam.small<-dt.iam.small[order(rank)]
  
  
  my_country_name<-unique(countryxwalk[country.exiobase==my_country,country])
  my_scenario_name<-unique(modelxwalk[scenario.short==my_scenario,Scenario])
  
  plot1<-ggplot(dt.iam.small, aes(x=reorder(sector, -rank), y=get(my_metric), fill=model.short)) +
    geom_bar(stat="identity", position = "dodge")+theme_minimal()+
    theme(plot.title = element_text(face="bold"))+
    xlab("Industry")+scale_fill_discrete(name="IAM source")+theme(legend.direction='vertical')+ggtitle(paste0(my_country_name, "\n", my_scenario_name))+
    ylab(x_lab)+xlab("Sector")+ theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))
  
  if (beamersave == 0){
    ggsave(paste0(graphpath,my_country,"_",my_scenario,"_",my_metric,".png"),plot1,width=8,height=11)
  }  else {
    ggsave(paste0(graphpath,my_country,"_",my_scenario,"_",my_metric,".png"),plot1,width=126*2,height=96*2, units="mm")
  }
  
  print(plot1)
  
}

# 
# graph_sector_iam<-function(dt, countryxwalk, modelxwalk, my_country, my_scenario ,my_metric,x_lab,graphpath){
#   dt.iam.small<-dt[ country==my_country& scenario.short==my_scenario]
#   dt.iam.smallav<-dt.iam.small[,.(meanrank=mean(get(my_metric),na.rm=T)), .(country, scenario.short, sector)]
#   dt.iam.smallav<-dt.iam.smallav[order(meanrank)]
#   dt.iam.smallav[,rank:=1:.N]
#   
#   top5_tab<-dt.iam.smallav[rank<=5,.(rank,sector)]
#   bottom5_tab<-dt.iam.smallav[rank>=N-5,.(rank,sector)]
#   
#   
#   dt.iam.small<-merge(dt.iam.small, dt.iam.smallav[,.(sector, country, rank)], by=c("sector","country"))
#   dt.iam.small<-dt.iam.small[order(rank)]
#   
#   
#   my_country_name<-unique(countryxwalk[country.exiobase==my_country,country])
#   my_scenario_name<-unique(modelxwalk[scenario.short==my_scenario,Scenario])
#   
#   plot1<-ggplot(dt.iam.small, aes(x=reorder(sector, -rank), y=get(my_metric), fill=model.short)) +
#     geom_bar(stat="identity", position = "dodge")+theme_minimal()+
#     theme(plot.title = element_text(face="bold"))+
#     xlab("Industry")+scale_fill_discrete(name="IAM source")+
#     coord_flip()+theme(legend.direction='vertical')+ggtitle(paste0(my_country_name, "\n", my_scenario_name))+
#     ylab(x_lab)+xlab("Sector")
#   
#   print(plot1)
#   
#   ggsave(paste0(graphpath,my_country,"_",my_scenario,"_",my_metric,".png"),plot1,width=8,height=11)
#   
#   
#   
# }
# 

graph_sector_alpha<-function(dt, countryxwalk, modelxwalk, my_country, my_scenario ,my_model,my_metric,x_lab,graphpath,beamersave=0){
  dt.iam.small<-dt[ country==my_country& scenario.short==my_scenario& model.short==my_model]
  dt.iam.smallav<-dt.iam.small[,.(meanrank=mean(get(my_metric),na.rm=T)), .(country, scenario.short, sector)]
  dt.iam.smallav<-dt.iam.smallav[order(meanrank)]
  dt.iam.smallav[,rank:=1:.N]
  
  top5_tab<-dt.iam.smallav[rank<=5,.(rank,sector)]
  bottom5_tab<-dt.iam.smallav[rank>=N-5,.(rank,sector)]
  
  toptab<-dt.iam.smallav[,.(rank,sector)]
  
  dt.iam.small<-merge(dt.iam.small, dt.iam.smallav[,.(sector, country, rank)], by=c("sector","country"))
  dt.iam.small<-dt.iam.small[order(rank)]
  
  
  my_country_name<-unique(countryxwalk[country.exiobase==my_country,country])
  my_scenario_name<-unique(modelxwalk[scenario.short==my_scenario,Scenario])
  
  plot1<-ggplot(dt.iam.small, ) +theme_minimal()+
    theme(plot.title = element_text(face="bold"))+
    xlab("Industry")+scale_fill_discrete(name="alpha")+ggtitle(paste0(my_country_name, "\n", my_scenario_name))+
    ylab(x_lab)+xlab("Sector")
  
  

  
  if (beamersave == 0){
    plot1<-plot1+geom_bar(stat="identity", position = "dodge", aes(x=reorder(sector, -rank), y=get(my_metric), fill=as.factor(num)))
    
    plot1<- plot1  +coord_flip()+theme(legend.direction='vertical')
    
    
    ggsave(paste0(graphpath,my_country,"_",my_scenario,"_",my_metric,".png"),plot1,width=8,height=11)
  }  else {
    
    plot1<-plot1+geom_bar(stat="identity", position = "dodge", aes(x=reorder(rank, rank), y=get(my_metric),  fill=as.factor(num)))
    plot1<-plot1+theme(legend.direction='horizontal')
    #plot1<-plot1+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=.8))
    
    
    
    mylegend<-g_legend(plot1)
    tt3 <- ttheme_minimal(base_size=8,    core = list(padding=unit(c(1, 1), "mm")))
    
    legend2<-tableGrob(toptab,theme=tt3, cols = c("",paste0("")),rows=NULL)
    
    
    plot2<-grid.arrange(plot1+ theme(legend.position="none"), mylegend,legend2,ncol = 2,layout_matrix = cbind(c(1,1,1,1,1,1,1), c(3,3,3,3,3,3,2)))
    print(head(toptab))
    print(plot2)
    
    ggsave(paste0(graphpath,my_country,"_",my_scenario,"_",my_metric,"_",my_model,"_","alpha.png"),plot2,width=126*2,height=60*2, units="mm")
  }
  
}
