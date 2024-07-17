


loadiam<-function(path,my_fill=F){
  ##########################################
  #Modified to work with the Phase IV IIASA scenario explorer
  ##########################################
  
  dt.wide<-fread(path,header=T,fill=my_fill)
  dt.wide<-dt.wide[Model!="?? NGFS Scenario Explorer https://data.ece.iiasa.ac.at/ngfs"]
  
  dt<-melt(dt.wide,id.vars=c("Model","Scenario","Region","Variable","Unit"))
  
  dt[,variable_n:=as.numeric(as.character(variable))]
  dt[,variable:=NULL]
  
  setnames(dt,c("Variable","variable_n"),c("series","year"))
  
  
  dt[Model=="GCAM 6.0 NGFS", model.short:="gcam"]
  dt[Model=="REMIND-MAgPIE 3.3-4.8", model.short:="remind"]
  dt[Model=="MESSAGEix-GLOBIOM 2.0-M-R12-NGFS", model.short:="message"]
  dt[Scenario=="Current Policies", scenario.short:="current"]
  dt[Scenario=="Delayed transition", scenario.short:="delayed"]
  dt[Scenario=="Nationally Determined Contributions (NDCs)", scenario.short:="ndc"]
  dt[Scenario=="Net Zero 2050", scenario.short:="netzero"]
  dt[Scenario=="Below 2°C", scenario.short:="below2"]
  dt[Scenario=="Fragmented World", scenario.short:="fragmented"]
  dt[Scenario=="Low demand", scenario.short:="lowdemand"]
  
  
  
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
