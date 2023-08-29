


loadiam<-function(path,my_fill=F){
  
  dt.wide<-fread(path,header=T,fill=my_fill)
  dt.wide<-dt.wide[Model!="?? NGFS Scenario Explorer https://data.ece.iiasa.ac.at/ngfs"]
  
  dt<-melt(dt.wide,id.vars=c("Model","Scenario","Region","Variable","Unit"))
  
  dt[,variable_n:=as.numeric(as.character(variable))]
  dt[,variable:=NULL]
  
  setnames(dt,c("Variable","variable_n"),c("series","year"))
  
  
  dt[Model=="GCAM 5.3+ NGFS", model.short:="gcam"]
  dt[Model=="REMIND-MAgPIE 3.0-4.4", model.short:="remind"]
  dt[Model=="MESSAGEix-GLOBIOM 1.1-M-R12", model.short:="message"]
  dt[Scenario=="Current Policies", scenario.short:="current"]
  dt[Scenario=="Delayed transition", scenario.short:="delayed"]
  dt[Scenario=="Nationally Determined Contributions (NDCs)", scenario.short:="ndc"]
  dt[Scenario=="Net Zero 2050", scenario.short:="netzero"]
  dt[Scenario=="Divergent Net Zero", scenario.short:="divnetzero"]
  
  
  dt[,Region_new:=gsub(".*\\|\\s*","",Region)]
  dt[,Region:=NULL]
  setnames(dt,"Region_new","Region")
  
  dt<-dt[Model!="© NGFS Scenario Explorer https://data.ece.iiasa.ac.at/ngfs"]
  
}


addseries<-function(dt.iam,seriesgroup,newname){
  
  dt.iam.new<-dt.iam[series %in%seriesgroup][,sum(value),by=c("Model","Scenario","Region","year","model.short","scenario.short","Unit")]
  dt.iam.new[,series:=newname]
  setnames(dt.iam.new,"V1","value")
  
  dt.iam.new
  
  #if there is an existing series of the same name, delete it
  dt.iam.n1<-dt.iam[series!=newname,]
  dt.iam.n2<-rbind(dt.iam.n1,dt.iam.new)
  dt.iam.n2[order(Model, Scenario, Region, series, year)]
}

subtractseries<-function(dt.iam,seriesgroup1,seriesgroup2,newname){
  
  dt.iam.new1<-dt.iam[series %in%seriesgroup1][,.(value1=sum(value)),by=c("Model","Scenario","Region","year","model.short","scenario.short","Unit")]
  dt.iam.new2<-dt.iam[series %in%seriesgroup2][,.(value2=sum(value)),by=c("Model","Scenario","Region","year","model.short","scenario.short","Unit")]
  dt.iam.new<-merge(dt.iam.new1,dt.iam.new2,by=c("Model","Scenario","Region","year","model.short","scenario.short","Unit"))
  
  dt.iam.new[,value:=value1-value2]
  dt.iam.new[,series:=newname]
  
  dt.iam.new[,value1:=NULL]
  dt.iam.new[,value2:=NULL]
  
  #setnames(dt.iam.new,"V1","value")
  dt.iam.new
  
  #if there is an existing series of the same name, delete it
  dt.iam.n1<-dt.iam[series!=newname,]
  dt.iam.n2<-rbind(dt.iam.n1,dt.iam.new)
  dt.iam.n2[order(Model, Scenario, Region, series, year)]
}


graphseries.iam<-function(dt.iam,mymodel,myseries,myregion,mypath){
  
  
  plot1<-ggplot(data=dt.iam[model.short==mymodel&series==myseries&Region==myregion])+geom_line(aes(x=year, y = value, color =Scenario),size=1.5)+
    theme_minimal(base_size=14)+ggtitle(paste0(unique(dt.iam[model.short==mymodel, Model]),": ", myregion,", ",myseries))+ylab(unique(dt.iam[model.short==mymodel, Unit]))+
    geom_vline(xintercept=2020,linetype=3)
  
  
  print(plot1)
  
  ggsave(paste0(mypath,mymodel,"_",gsub("[[:punct:]]", "", myseries),"_",myregion,".png"),plot1,width=126*2,height=60*2, units="mm")
  
}

graphseries.nigem<-function(dt.nigem,mymodel,myseries,mycountry,mypath){
  
  
  plot1<-ggplot(data=dt.nigem[model.short==mymodel&sector.nigem.out==myseries&country.nigem.out==mycountry])+geom_line(aes(x=year, y = series.wghted, color =Scenario),size=1.5)+
    theme_minimal(base_size=20)+ggtitle(paste0(unique(dt.nigem[model.short==mymodel&sector.nigem.out==myseries&country.nigem.out==mycountry, Model]),": ",mycountry,", ",myseries))+
    ylab(paste0("TJ/",unique(dt.nigem[model.short==mymodel&sector.nigem.out==myseries&country.nigem.out==mycountry, units.nigem.out])))
  print(plot1)
  #ggsave(paste0(path_graphs,"nigem_",mymodel,"_",myseries,"_",mycountry,".png"),plot1)
  ggsave(paste0(mypath,"nigem_",mymodel,"_",gsub("[[:punct:]]", "", myseries),"_",mycountry,".png"),plot1,width=30,height=20,units="cm")
}

graphseries.nigem.2<-function(dt.nigem,mymodel,myseries,mycountry,myvar,mypath){
  
  
  plot1<-ggplot(data=dt.nigem[model.short==mymodel&sector==myseries&country==mycountry])+geom_line(aes(x=year, y = get(myvar), color =Scenario),size=1.5)+
    theme_minimal(base_size=12)+ggtitle(paste0(unique(dt.nigem[model.short==mymodel&sector==myseries&country==mycountry, Model]),": ",mycountry,", ",myseries))+
    ylab(paste0("TJ - 2020 index"))
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
