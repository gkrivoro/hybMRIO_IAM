

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



graph_sector_iam.2<-function(dt, countryxwalk, modelxwalk, my_country,scenario_label,save_label ,my_metric,x_lab,graphpath,my.N,my.iosectors,beamersave=0){
  

  highlight = function(x, pat, color="black", family="") {
    ifelse(grepl(pat, x), glue("<b style='font-family:{family}; color:{color}'>{x}</b>"), x)
  }
  
  if (beamersave ==0){
    my.iosectors<-gsub("\\|","\\\\|",set_sectors.IO)
  }
  
  #use this version after 9/23/23. Making it so we run the summary stats function and then display this
  # so scenario is instead already taken into account. 
   dt.iam.small<-dt[ country==my_country]
  dt.iam.smallav<-dt.iam.small[,.(meanrank=mean(get(my_metric),na.rm=T)),
                               .(country, sector)]
  dt.iam.smallav<-dt.iam.smallav[order(meanrank)]
  dt.iam.smallav[,rank:=1:.N]
  
  top5_tab<-dt.iam.smallav[rank<=5,.(rank,sector)]
  bottom5_tab<-dt.iam.smallav[rank>=my.N-5,.(rank,sector)]
  
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
    plot1<-plot1+  scale_x_discrete(labels= function(x) highlight(x, paste0(my.iosectors,collapse="|"), "black")) +theme(axis.text.y=element_markdown())

    plot1<- plot1  +coord_flip()+theme(legend.direction='vertical')
    
    ggsave(paste0(graphpath,my_country,"_",save_label,".png"),plot1,width=8,height=11)
    print(plot1)
  }  else {
    plot1<-plot1+geom_bar(stat="identity", position = "dodge", aes(x=reorder(rank, rank), y=get(my_metric), fill=model.short))
    print(toptab[sector%in%my.iosectors,rank])
    plot1<-plot1+  scale_x_discrete(labels= function(x) highlight(x, paste0(toptab[sector%in%my.iosectors,rank],collapse="|"), "black")) +theme(axis.text.x=element_markdown())
    plot1<-plot1+theme(legend.direction='horizontal')
    #plot1<-plot1+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=.8))
    
    
    
    mylegend<-g_legend(plot1)
    tt3 <- ttheme_minimal(base_size=8,    core = list(padding=unit(c(1, 1), "mm")))
    
    
    
    
    legend2<-tableGrob(toptab,theme=tt3, cols = c("",paste0("")),rows=NULL)
    
    find_cells <- function(table, row, col, name="core-fg"){
      l <- table$layout
      unlist(Map(function(r, c) which(((l$t-1) == r) & ((l$l-1) == c) & (l$name == name)), row, col))
    }
    
    modify_cells <- function(g, ids, gp=gpar()){
      for(id in ids) g$grobs[id][[1]][["gp"]] <- gp
      return(g)
    }
    
    ids <- find_cells(legend2, toptab[sector%in%my.iosectors,rank],rep(1,length(toptab[sector%in%my.iosectors,rank])), "core-fg")
    legend2 <- modify_cells(legend2, ids, gpar(fontsize=7,fontface="bold"))
    
    plot2<-grid.arrange(plot1+ theme(legend.position="none"), mylegend,legend2,ncol = 3,layout_matrix = cbind(c(1,1,1,1,1,1,1), c(1,1,1,1,1,1,1), c(3,3,3,3,3,3,2)))
    
    
    # ggsave(paste0(graphpath,my_country,"_",my_scenario,"_",my_metric,"_beamer.png"),plot2,width=126*2,height=96*2, units="mm")
    ggsave(paste0(graphpath,my_country,"_",save_label,"_beamer.png"),plot2,width=126*2.5,height=60*2.5, units="mm")
    
    
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


########################################################################
#previously the ones after this point were in parse_iam.R
########################################################################

graphseries.nigem<-function(dt.nigem,mymodel,myseries,mycountry,mypath){
  
  
  plot1<-ggplot(data=dt.nigem[model.short==mymodel&sector.nigem.out==myseries&country.nigem.out==mycountry])+geom_line(aes(x=year, y = series.wghted, color =Scenario),size=1.5)+
    theme_minimal(base_size=20)+ggtitle(paste0(unique(dt.nigem[model.short==mymodel&sector.nigem.out==myseries&country.nigem.out==mycountry, Model]),": ",mycountry,", ",myseries))+
    ylab(paste0("TJ/",unique(dt.nigem[model.short==mymodel&sector.nigem.out==myseries&country.nigem.out==mycountry, units.nigem.out])))
  print(plot1)
  #ggsave(paste0(path_graphs,"nigem_",mymodel,"_",myseries,"_",mycountry,".png"),plot1)
  ggsave(paste0(mypath,"nigem_",mymodel,"_",gsub("[[:punct:]]", "", myseries),"_",mycountry,".png"),plot1,width=30,height=20,units="cm")
}

graphseries.nigem.2<-function(dt.nigem,mymodel,myseries,mycountry,myvar,mypath,mylabel="TJ - 2020 index"){
  
  
  plot1<-ggplot(data=dt.nigem[model.short==mymodel&sector==myseries&country==mycountry])+geom_line(aes(x=year, y = get(myvar), color =Scenario),size=1.5)+
    theme_minimal(base_size=12)+ggtitle(paste0(unique(dt.nigem[model.short==mymodel&sector==myseries&country==mycountry, Model]),": ",mycountry,", ",myseries))+
    ylab(mylabel)
  print(plot1)
  #ggsave(paste0(path_graphs,"nigem_",mymodel,"_",myseries,"_",mycountry,".png"),plot1)
  ggsave(paste0(mypath,"nigem_",mymodel,"_",gsub("[[:punct:]]", "", myseries),"_",mycountry,".png"),plot1,width=128,height=96,units="mm")
}

graphseries.nigem.3<-function(dt.nigem,dt.decomp1,mymodel,myseries,mycountry,myvar,mypath,iprint=1){
  #adding in series decomposition
  
  plot1<-ggplot(data=dt.nigem[model.short==mymodel&sector==myseries&country==mycountry])+geom_line(aes(x=year, y = get(myvar), color =Scenario),size=1.5)+
    theme_minimal(base_size=20)+ggtitle(paste0(unique(dt.nigem[model.short==mymodel&sector==myseries&country==mycountry, Model]),": ",mycountry,", ",myseries))+
    ylab(paste0("TJ - 2020 index"))+theme(legend.position="bottom")
  
  
  
  plot_x1<-ggplot(dt.decomp1[country.out==mycountry&model.short==mymodel& sector.out ==myseries &scenario.short =="current"], 
                  aes(x="", y=series.coef.sum.pc, fill=sector.in.top5)) +
    geom_bar(stat="identity", width=1) +xlab("")+
    coord_polar("y", start=0)+theme_minimal(base_size=8)+ylab("")+ggtitle("Current")+ scale_fill_discrete(name = "Input Sector")
  
  plot_y1<-ggplot(dt.decomp1[country.out==mycountry&model.short==mymodel& sector.out ==myseries &scenario.short =="delayed"], 
                  aes(x="", y=series.coef.sum.pc, fill=sector.in.top5)) +
    geom_bar(stat="identity", width=1)+xlab("") +
    coord_polar("y", start=0)+theme_minimal(base_size=8)+ylab("")+ggtitle("Delayed")+ scale_fill_discrete(name = "Input Sector")
  
  plot_z1<-ggplot(dt.decomp1[country.out==mycountry&model.short==mymodel& sector.out ==myseries &scenario.short =="netzero"], 
                  aes(x="", y=series.coef.sum.pc, fill=sector.in.top5)) +
    geom_bar(stat="identity", width=1)+xlab("") +
    coord_polar("y", start=0)+theme_minimal(base_size=8)+ylab("")+ggtitle("Net Zero")+ scale_fill_discrete(name = "Input Sector")
  
  plot_xyz<-grid.arrange(plot1,plot_x1, plot_y1, plot_z1 ,layout_matrix=rbind(c(1,1,2),c(1,1,3),c(1,1,4)))
  
  #grid.arrange(tableGrob(tab1,theme=tt2),tableGrob(tab3,theme=tt2),tableGrob(tab3,theme=tt2) ,nrow=1)
  if (iprint==1){
    print(plot_xyz)
  }
  
  
  
  #ggsave(paste0(path_graphs,"nigem_",mymodel,"_",myseries,"_",mycountry,".png"),plot1)
  ggsave(paste0(mypath,"nigem_",mymodel,"_",gsub("[[:punct:]]", "", myseries),"_",mycountry,".png"),plot_xyz,width=128*2,height=96*2,units="mm")
}


graphseries.nigem.other<-function(dt.nigem,dt.b,mymodel,myseries,sec.standalone,mycountry,myvar,mypath,I_energy=0){
  
  
  plot1<-ggplot(data=dt.nigem[model.short==mymodel&sector==myseries&country==mycountry])+geom_line(aes(x=year, y = get(myvar), color =Scenario),size=1.5)+
    theme_minimal(base_size=20)+ggtitle(paste0(unique(dt.nigem[model.short==mymodel&sector==myseries&country==mycountry, Model]),": ",myseries))+
    ylab(paste0("TJ - 2020 index"))
  print(plot1)
  #ggsave(paste0(path_graphs,"nigem_",mymodel,"_",myseries,"_",mycountry,".png"),plot1)
  ggsave(paste0(mypath,"nigem_",mymodel,"_",gsub("[[:punct:]]", "", myseries),"_",mycountry,".png"),plot1,width=30,height=20,units="cm")
  
  if (I_energy ==0){
    tab1<-dt.b[sector.out==myseries& country.out==mycountry][order(-value),.(country.in, sector.in, value)][1:5]
    tab2<-dt.b[sector.out==myseries& country.out==mycountry&sector.in%in%sec.standalone][order(-value),.(country.in, sector.in, value)][1:5]
  } else {
    dt.b[,energyalpha:=TJperMeuro.in*value]
    tab1<-dt.b[sector.out==myseries& country.out==mycountry][order(-energyalpha),.(country.in, sector.in, value,TJperMeuro.in, energyalpha )][1:5]
    tab2<-dt.b[sector.out==myseries& country.out==mycountry&sector.in%in%sec.standalone][order(-energyalpha),.(country.in, sector.in, value, TJperMeuro.in, energyalpha)][1:5]
  }
  print(tab1)
  print(tab2)
}




#modify to only display 2. 
graph.IO.decompose<-function(my.dt.decomp, mycountry,mysector, mymodel, my.model_xwalk,my.path_graphs){
  ################################
  # This graphs the decomposition of the IO series. First you need to create the decomposed series using decompose_country() 
  ################################
  ylim1<-c(0,max(my.dt.decomp[model.short==mymodel&sector.out==mysector,sum(series.coef.sum),.(year,scenario.short)][,V1],na.rm=T)*1.1)
  list_plot<-list()
  for (myscenario in unique(my.model_xwalk[scenario.short!="divnetzero",scenario.short])){
    list_plot[[myscenario]]<-ggplot(data = my.dt.decomp[year>=2020&sector.out==mysector&model.short==mymodel&scenario.short==myscenario], aes(x = year, y = series.coef.sum, fill=sector.in.top5))+
      geom_area()+theme_minimal(base_size=12)+ylab("TJ")+
      ggtitle(paste0(my.model_xwalk[model.short==mymodel,Model][1],"\n", mycountry, ", ",mysector, "\n",my.model_xwalk[scenario.short==myscenario,Scenario][1]))+
      guides(fill=guide_legend(title="Top input sectors"))+ylim(ylim1)
  }
  
  ###plot2<-grid.arrange(list_plot[[1]],list_plot[[2]],list_plot[[3]])
  plot2<-grid.arrange(list_plot[[1]],list_plot[[3]])
  
  # so they fit on the beamer slides!
  #ggsave(paste0(my.path_graphs,gsub("[[:punct:]]", "", mysector),"_",mycountry,"_",mymodel,".png"),plot2,width=24,height=18,units="cm")
  ggsave(paste0(my.path_graphs,gsub("[[:punct:]]", "", mysector),"_",mycountry,"_",mymodel,".png"),
         plot2,width=24,height=12,units="cm")
  
}
