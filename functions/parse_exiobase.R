

parse_exiobase_F<-function(raw_dat_path, n_colvars, rowvar,which_rowvar=NULL,list_industry_tabs ){
  #####first colvar must be the region!!!
  #### This function processes the industry with multiple countries. 
  
  
  industrytable<-list_industry_tabs$industry.tab
  industrycountrytable<-list_industry_tabs$industry.cntry.tab
  
  
  raw_dat<-fread(raw_dat_path,header=F)
  
  #first create index for that
  xwalk.names<-as.data.table(cbind(t(raw_dat[1:n_colvars,]), colnames(raw_dat)))[-(1:n_colvars),]
  #change v3 to variable so that the merge is more convenient later
  setnames(xwalk.names,c("V1","V2","V3"),c("region","sector","variable"))
  setnames(raw_dat,c("V1"),rowvar,skip_absent=T)
  raw_dat<- raw_dat[-(1:(n_colvars+1)),]
  dt.long<-melt(raw_dat, id.vars = rowvar) 
  dt.long<-merge(dt.long, xwalk.names[,.(variable , region.out = region , sector.old.out = sector)], by = "variable")
  dt.long[,value_n:=as.numeric(value)]
  dt.long[,value:=NULL]
  setnames(dt.long, "value_n","value")
  
  # dt.long[,country_id.out := 2]
  # dt.long[region.out == "US",country_id.out := 1]
  # 
  
  setnames(dt.long, "region.out","country.out")
  
  
  
  dt.long<-merge(dt.long,industrytable[,.(sector.old.out=sector.old, sector_id.out=sector_id)],by = c("sector.old.out") )
  dt.long<-merge(dt.long,industrycountrytable[,.(sector_id.out=sector_id, country_id.out = country_id, country.out = country, id.out=id)],by = c("sector_id.out","country.out") )
  #dt.long<-merge(dt.long,raw_output[,.(region.out=region, sector.old.out=sector.old, indout)],by = c("sector.old.out","region.out") )
  
  
  if (is.null(which_rowvar)){
    dt.long.small<-dt.long[,.(value=sum(value)),by=c("sector_id.out", "country_id.out","id.out", rowvar)]
  } else{
    dt.long.small<-dt.long[get(rowvar)%in%which_rowvar][,.(value=sum(value)),by=c("sector_id.out", "country_id.out","id.out", rowvar)]
  }
  
  dt.long.small<-dt.long.small[order(id.out)]
}





parse_exiobase_F_hh<-function(raw_dat_path, n_colvars, rowvar,which_rowvar=NULL,list_industry_tabs ){
  #####first colvar must be the region!!!
  
  
  
  industrytable<-list_industry_tabs$industry.tab
  industrycountrytable<-list_industry_tabs$industry.cntry.tab
  
  raw_dat<-fread(raw_dat_path,header=F)
  
  #first create index for that
  xwalk.names<-as.data.table(cbind(t(raw_dat[1:n_colvars,]), colnames(raw_dat)))[-(1:n_colvars),]
  #change v3 to variable so that the merge is more convenient later
  setnames(xwalk.names,c("V1","V2","V3"),c("region","category","variable"))
  setnames(raw_dat,c("V1"),rowvar,skip_absent=T)
  raw_dat<- raw_dat[-(1:(n_colvars+1)),]
  dt.long<-melt(raw_dat, id.vars = rowvar)
  dt.long<-merge(dt.long, xwalk.names[,.(variable , region.out = region , category)], by = "variable")
  dt.long[,value_n:=as.numeric(value)]
  dt.long[,value:=NULL]
  setnames(dt.long, "value_n","value")
  
  dt.long[,country_id.out := 2]
  dt.long[region.out == "US",country_id.out := 1]
  
  
  
  if (is.null(which_rowvar)){
    dt.long.small<-dt.long[,.(value=sum(value)),by=c("category", "country_id.out", rowvar)]
  } else{
    dt.long.small<-dt.long[get(rowvar)%in%which_rowvar][,.(value=sum(value)),by=c("category", "country_id.out", rowvar)]
  }
  
  dt.long.small<-dt.long.small[order(country_id.out, category)]
}


parse_exiobase_Y<-function(raw_dat_path, n_colvars, which_rowvar=NULL,list_industry_tabs ){
  #####first colvar must be the region!!!
  raw_dat<-fread(raw_dat_path,header=F)
  
  
  industrytable<-list_industry_tabs$industry.tab
  industrycountrytable<-list_industry_tabs$industry.cntry.tab
  
  #first create index for that
  xwalk.names<-as.data.table(cbind(t(raw_dat[1:n_colvars,]), colnames(raw_dat)))[-(1:n_colvars),]
  #change v3 to variable so that the merge is more convenient later
  setnames(xwalk.names,c("V1","V2","V3"),c("region","category","variable"))
  setnames(raw_dat,names(raw_dat)[1:2],c( "region.in","sector.old.in"),skip_absent=T)
  raw_dat<- raw_dat[-(1:(n_colvars+1)),]
  dt.long<-melt(raw_dat, id.vars = c( "region.in","sector.old.in"))
  dt.long<-merge(dt.long, xwalk.names[,.(variable , region.out = region , category)], by = "variable")
  dt.long[,value_n:=as.numeric(value)]
  dt.long[,value:=NULL]
  setnames(dt.long, "value_n","value")
  
  
  
  # 
  # 
  # dt.long[,country_id.out := 2]
  # dt.long[region.out == "US",country_id.out := 1]
  # 
  # dt.long[,country_id.in := 2]
  # dt.long[region.in == "US",country_id.in := 1]
  # 
  # 
  setnames(dt.long, "region.out","country.out")
  setnames(dt.long, "region.in","country.in")
  
  dt.long<-merge(dt.long,industrytable[,.(sector.old.in=sector.old, sector_id.in=sector_id)],by = c("sector.old.in") )
  dt.long<-merge(dt.long,unique(industrycountrytable[,.(country.out=country, country_id.out = country_id)]),by = c("country.out") )
  
  dt.long<-merge(dt.long,industrycountrytable[,.(sector_id.in=sector_id, country.in = country,country_id.in = country_id, id.in=id)],by = c("sector_id.in","country.in") )
  
  
  
  if (is.null(which_rowvar)){
    dt.long.small<-dt.long[,.(value=sum(value)),by=c("category", "country_id.out", "sector_id.in","country_id.in","id.in")]
  } else{
    dt.long.small<-dt.long[get(rowvar)%in%which_rowvar][,.(value=sum(value)),by=c("category", "country_id.out",  "sector_id.in","country_id.in","id.in")]
  }
  
  dt.long.small<-dt.long.small[order(country_id.out, category)]
}



get_prod_ind_hyb<-function(raw_y_path){
  rawy<-as.data.table(t(fread(raw_y_path)),keep.rownames=T)
  
  setnames(rawy,c("rn","V1","V4","V7" ,"V8"),c("country","industry","product","units","value"))
  
  
  
  unique(rawy[,.(industry, product, units)])
}





parse_exiobase_HIOT_y<-function(raw_y_path,list_industry_tabs){
  
  rawy<-as.data.table(t(fread(raw_y_path)),keep.rownames=T)
  setnames(rawy,c("rn","V1","V4","V7" ,"V8"),c("country","industry","product","units","value"))
  unique(rawy[,.(industry, product, units)])
  
  
  rawy_sec<-merge(rawy[,.(country, sector = industry, value)], list_industry_tabs$industry.tab[,.( sector=sector.old,sector_id= sector_id )] ,by=c("sector"))
  rawy_sec<-merge(rawy_sec, list_industry_tabs$industry.cntry.tab[,.(country,sector_id=sector_id, country_id=country_id, id=id)], by=c("sector_id","country"))
  rawy_sec[,value_n:=as.numeric(value)]
  rawy_sec[,.(gross_output=sum(value_n)),by=c("id","country_id","sector_id")][order(id, country_id, sector_id)]
  
  
  
}

parse_exiobase_HIOT<-function(raw_dat_path, n_colvars,which_rowvar=NULL,list_industry_tabs ){
  #####first colvar must be the region!!!
  
  industrytable<-list_industry_tabs$industry.tab
  industrycountrytable<-list_industry_tabs$industry.cntry.tab
  
  raw_dat<-fread(raw_dat_path,header=F)
  
  #first create index for that
  xwalk.names<-as.data.table(cbind(t(raw_dat[1:n_colvars,]), colnames(raw_dat)))[-(1:(n_colvars+1)),]
  #change v3 to variable so that the merge is more convenient later
  setnames(xwalk.names,c("V1","V2","V5"),c("region","sector","variable"))
  setnames(raw_dat,c("V1","V2","V5"),c("region.in","product.in","units"),skip_absent=T)
  
  raw_dat<- raw_dat[-(1:(n_colvars)),]
  raw_dat[,V3:=NULL]
  raw_dat[,V4:=NULL]
  
  dt.long<-melt(raw_dat, id.vars = c("region.in","product.in","units"))
  
  
  #convert products to industries.
  dt.prod_ind<-data.table( product = unique(dt.long[,product.in]) , industry = unique(xwalk.names[,sector]))
  dt.long<-merge(dt.long, dt.prod_ind[,.(product.in = product, sector.old.in=industry)],by="product.in")
  

  
  dt.long<-merge(dt.long, xwalk.names[,.(variable , country.out = region , sector.old.out = sector)], by = "variable")
  dt.long[,value_n:=as.numeric(value)]
  dt.long[,value:=NULL]
  setnames(dt.long, "value_n","value")
  setnames(dt.long, "region.in","country.in")
  
  #setnames(setnames,"region.out","country")
  
  # dt.long[,country_id.out := 2]
  # dt.long[region.out == "US",country_id.out := 1]
  # dt.long[,country_id.in := 2]
  # dt.long[region.in == "US",country_id.in := 1]
  # 
  dt.long<-merge(dt.long,industrytable[,.(sector.old.out=sector.old, sector_id.out=sector_id)],by = c("sector.old.out") )
  dt.long<-merge(dt.long,industrycountrytable[,.(sector_id.out=sector_id, country.out = country,country_id.out = country_id, id.out=id)],by = c("sector_id.out","country.out") )
  
  dt.long<-merge(dt.long,industrytable[,.(sector.old.in=sector.old, sector_id.in=sector_id)],by = c("sector.old.in") )
  dt.long<-merge(dt.long,industrycountrytable[,.(sector_id.in=sector_id,  country.in = country,country_id.in = country_id,id.in=id)],by = c("sector_id.in","country.in") )
  #dt.long<-merge(dt.long,raw_output[,.(region.out=region, sector.old.out=sector.old, indout)],by = c("sector.old.out","region.out") )
  
  
  if (is.null(which_rowvar)){
    dt.long.small<-dt.long[,.(value=sum(value)),by=c("sector_id.out", "country_id.out","id.out","sector_id.in", "country_id.in","id.in" )]
  } else{
    dt.long.small<-dt.long[get(rowvar)%in%which_rowvar][,.(value=sum(value)),by=c("sector_id.out", "country_id.out","id.out","sector_id.in", "country_id.in","id.in")]
  }
  
  dt.long.small<-dt.long.small[order(id.out)]

}



parse_exiobase_Z<-function(raw_dat_path, n_colvars,which_rowvar=NULL,list_industry_tabs ){
  #####first colvar must be the region!!!
  
  industrytable<-list_industry_tabs$industry.tab
  industrycountrytable<-list_industry_tabs$industry.cntry.tab
  
  raw_dat<-fread(raw_dat_path,header=F)
  
  #first create index for that
  xwalk.names<-as.data.table(cbind(t(raw_dat[1:n_colvars,]), colnames(raw_dat)))[-(1:n_colvars),]
  #change v3 to variable so that the merge is more convenient later
  setnames(xwalk.names,c("V1","V2","V3"),c("region","sector","variable"))
  setnames(raw_dat,c("V1","V2"),c("region.in","sector.old.in"),skip_absent=T)
  raw_dat<- raw_dat[-(1:(n_colvars+1)),]
  dt.long<-melt(raw_dat, id.vars = c("region.in","sector.old.in"))
  dt.long<-merge(dt.long, xwalk.names[,.(variable , country.out = region , sector.old.out = sector)], by = "variable")
  dt.long[,value_n:=as.numeric(value)]
  dt.long[,value:=NULL]
  setnames(dt.long, "value_n","value")
  setnames(dt.long, "region.in","country.in")
  
  #setnames(setnames,"region.out","country")
  
  # dt.long[,country_id.out := 2]
  # dt.long[region.out == "US",country_id.out := 1]
  # dt.long[,country_id.in := 2]
  # dt.long[region.in == "US",country_id.in := 1]
  # 
  dt.long<-merge(dt.long,industrytable[,.(sector.old.out=sector.old, sector_id.out=sector_id)],by = c("sector.old.out") )
  dt.long<-merge(dt.long,industrycountrytable[,.(sector_id.out=sector_id, country.out = country,country_id.out = country_id, id.out=id)],by = c("sector_id.out","country.out") )
  
  dt.long<-merge(dt.long,industrytable[,.(sector.old.in=sector.old, sector_id.in=sector_id)],by = c("sector.old.in") )
  dt.long<-merge(dt.long,industrycountrytable[,.(sector_id.in=sector_id,  country.in = country,country_id.in = country_id,id.in=id)],by = c("sector_id.in","country.in") )
  #dt.long<-merge(dt.long,raw_output[,.(region.out=region, sector.old.out=sector.old, indout)],by = c("sector.old.out","region.out") )
  
  
  if (is.null(which_rowvar)){
    dt.long.small<-dt.long[,.(value=sum(value)),by=c("sector_id.out", "country_id.out","id.out","sector_id.in", "country_id.in","id.in" )]
  } else{
    dt.long.small<-dt.long[get(rowvar)%in%which_rowvar][,.(value=sum(value)),by=c("sector_id.out", "country_id.out","id.out","sector_id.in", "country_id.in","id.in")]
  }
  
  dt.long.small<-dt.long.small[order(id.out)]
}






parse_exiobase_x<-function(raw_dat_path,list_industry_tabs ){
  raw_dat<-fread(raw_dat_path)
  industrytable<-list_industry_tabs$industry.tab
  industrycountrytable<-list_industry_tabs$industry.cntry.tab
  
  setnames(raw_dat,"sector","sector.old")
  
  x1.new<-merge(raw_dat , industrytable[,.(sector.old, sector, sector_id)], by="sector.old")
  #x1.new[,country_id:=2]
  #x1.new[region=="US",country_id:=1]
  setnames(x1.new,"region","country")
  x1.new<-merge(x1.new,industrycountrytable[,.(sector_id, country, country_id, id)],by = c("sector_id","country") )
  
 
  x1.small<-x1.new[,.(gross_output = sum(indout)),by=c("country","country_id","sector","id","sector_id")]
  if(length(unique(x1.small[gross_output<=0,sector]))>0){
    
    print(x1.small[gross_output<=0,.(country,sector)])
    
    warning("Some sectors have 0 gross output")
  }
  x1.small
}




gen_alpha_mu<-function(Z.long, x, F.long, other_input_ind){

  
  
  Z.long<-merge(Z.long, x[,.(country_id,sector_id,gross_output)],by.x =c("country_id.out","sector_id.out"), by.y = c("country_id","sector_id" ) )
  Z.long[,alpha_ij :=value/gross_output]
  F.other<-F.long[stressor%in%other_input_ind][,.(other_input=sum(value)),  by=c("sector_id.out","country_id.out")]
  Z.long.other<-merge(Z.long,F.other ,by=c("country_id.out","sector_id.out"))
  Z.long.other[,other_input_pc := other_input/gross_output]

  Z.long.other<-Z.long.other[order(id.in, id.out)]
  
  
  outp<-list(  dt.alpha_ij= Z.long.other[,.(country_id.in, sector_id.in, id.in,country_id.out, sector_id.out, id.out, alpha_ij)],
                                       dt.mu_i=Z.long.other[,mean(other_input_pc),by="id.out"])                        

  
  
  
}


gen_ghosh<-function(Z.long, x ,industrymapping ){
  
  
  
  Z.long<-merge(Z.long, x[,.(country_id,sector_id,gross_output)],by.x =c("country_id.in","sector_id.in"), by.y = c("country_id","sector_id" ) )
  Z.long[,coef :=ifelse(gross_output!=0,value/gross_output,0)]
  
  Z.long<-Z.long[order(id.in, id.out)]
  
  
  dt.ghosh_ij= Z.long[,.(country_id.in, sector_id.in, id.in,country_id.out, sector_id.out, id.out, coef)]
  
  
  
  
  dt.ghosh_ij<-merge(dt.ghosh_ij, industrymapping$industry.cntry.tab[,.(id.in=id,sector.in=sector, country.in=country)],by="id.in")
  dt.ghosh_ij<-merge(dt.ghosh_ij, industrymapping$industry.cntry.tab[,.(id.out=id,sector.out=sector, country.out=country)],by="id.out")
  
  
  
}



gen_alpha<-function(Z.long, x, industrymapping){
  
  
  
  Z.long<-merge(Z.long, x[,.(country_id,sector_id,gross_output)],by.x =c("country_id.out","sector_id.out"), by.y = c("country_id","sector_id" ) )

  Z.long[,coef :=value/gross_output]
  

  dt.alpha_ij<- Z.long[,.(country_id.in, sector_id.in, id.in,country_id.out, sector_id.out, id.out, coef)]
  
  
  dt.alpha_ij<-merge(dt.alpha_ij, industrymapping$industry.cntry.tab[,.(id.in=id,sector.in=sector, country.in=country)],by="id.in")
  dt.alpha_ij<-merge(dt.alpha_ij, industrymapping$industry.cntry.tab[,.(id.out=id,sector.out=sector, country.out=country)],by="id.out")
  
  
  
  
}


gen_mu<-function(F.long,x,  other_input_ind, impact=0){
  
  if (impact ==1){
    F.other<-F.long[impact%in%other_input_ind][,.(other_input=sum(value)),  by=c("id.out","sector_id.out","country_id.out")]
    
  } else {
    F.other<-F.long[stressor%in%other_input_ind][,.(other_input=sum(value)),  by=c("id.out","sector_id.out","country_id.out")]
    
  }
  F.other<-merge(F.other, x[,.(country_id.out=country_id,sector_id.out=sector_id,gross_output)],by =c("country_id.out","sector_id.out") )
  F.other[,other_input_pc := other_input/gross_output]
  F.other<-F.other[order( id.out)]
  
  
  dt.mu_i<-F.other[,.(coef=mean(other_input_pc)),by="id.out"]                     
}



gen_inv.zerod<-function(dt.direct, zerod.ind,industrymapping){
  #SWITCHED IN AND OUT
  
  direct.mat<-as.matrix(dcast(dt.direct[,.(id.in, id.out, coef)], id.out ~ id.in, value.var = "coef")) [,-1]
  
  
  N_dim<-max(industrymapping$industry.cntry.tab[,id])
  
  
  
  Om<-diag(N_dim)
  
  Om[,zerod.ind]<-0
  
  #this zeroed out the columns
  direct.zerod.mat<- Om %*% direct.mat 
  
  
  total.mat<-solve(diag(N_dim)-direct.zerod.mat)
  
  
  dt.temptot<-as.data.table(total.mat)
  dt.temptot[,id.out:=1:.N]
  
  dt.total<-melt(as.data.table(dt.temptot),id.vars="id.out")
  
  dt.total[,id.in:=as.numeric(gsub("V","",variable))]
  
  
  dt.total<-merge(dt.total, industrymapping$industry.cntry.tab[,.(id.in=id,sector.in=sector, country.in=country)],by="id.in")
  dt.total<-merge(dt.total, industrymapping$industry.cntry.tab[,.(id.out=id,sector.out=sector, country.out=country)],by="id.out")
  
  
  
  
}

gen_invdt<-function(dt.direct, industrymapping){
  #SWITCHED IN AND OUT
  
  
  direct.mat<-as.matrix(dcast(dt.direct[,.(id.in, id.out, coef)], id.out ~ id.in, value.var = "coef")) [,-1]
  

  N_dim<-length(unique(industrymapping$industry.cntry.tab[,id]))
  
  total.mat<-solve(diag(N_dim)-direct.mat)
  
  
  
  
  
  dt.temptot<-as.data.table(total.mat,keep.rownames = T)
  dt.temptot[,id.out:=as.integer(rn)]
  dt.temptot[,rn:=NULL]
  #print(dt.temptot[,id.out])
  #creat xwalk for subset
  xwalk.temp<-data.table(id=dt.temptot[,id.out])
  xwalk.temp[,colnum:=1:.N]
  dt.total<-melt(as.data.table(dt.temptot),id.vars="id.out")
  
  dt.total[,colnum:=as.integer(gsub("V","",variable))]
  dt.total<-merge(dt.total,xwalk.temp[,.(colnum,id.in = id)], by="colnum")
  

  
  dt.total<-merge(dt.total, industrymapping$industry.cntry.tab[,.(id.in=id,sector.in=sector, country.in=country)],by="id.in")
  dt.total<-merge(dt.total, industrymapping$industry.cntry.tab[,.(id.out=id,sector.out=sector, country.out=country)],by="id.out")
  
  
  
  
}


gen_industry_tables<-function(path_industry_xwalk, energy_sectors, countries){
  
  
  dt.ctry<-data.table(country=countries)
  dt.ctry[,country_id:=1:.N]
  
  #Note, this has a small difference over the original in sectoralmodel folder in that I make the country table here and have countries as an input. 
  
  industry.xwalk.raw<-fread(path_industry_xwalk)
  dt.energy_sectors=data.table( energy_names=energy_sectors)

  industry.xwalk.raw[,Number.average:=mean(as.double(Number.old)), by="sector"]
  dt.energy_sectors[,sector_id:=1:.N]
  
  sector_n<-data.table( unique(industry.xwalk.raw[,.(sector,Number.average)]))
  sector_n[order(Number.average)]
  sector_n[,sector_id_temp:=1:.N]
  sector_n<-merge(sector_n,dt.energy_sectors, all.x=T, by.x="sector", by.y="energy_names")
  sector_n[order(sector_id,sector_id_temp)]
  sector_n[is.na(sector_id), sector_id:=nrow(dt.energy_sectors)+(1:.N)]
  industry.xwalk.raw<-merge(industry.xwalk.raw, sector_n[,.(sector, sector_id)], by=c("sector"),all.x=T)
  
  
  industry.xwalk.raw<-industry.xwalk.raw[order(sector_id)]
  
  
  dt.industry.country1<-CJ(country_id=unique(dt.ctry[order(country_id),country_id]), sector_id=unique(industry.xwalk.raw[,sector_id]))
  dt.industry.country1<-merge(dt.industry.country1,unique(dt.ctry[,.(country_id, country)]) , by= "country_id")
  dt.industry.country1<-dt.industry.country1[order(country_id, sector_id)]
  dt.industry.country1[,id:=1:.N]
  
  dt.industry.country2<-merge(dt.industry.country1,unique(industry.xwalk.raw[,.(sector_id,sector)]) , by="sector_id")
  dt.industry.country2<-dt.industry.country2[order(id)]
  
  myout=list(industry.tab = industry.xwalk.raw, industry.cntry.tab = dt.industry.country2)
}




gen_industry_tables_hyb<-function(path_industry_xwalk,path_hybrid_prod,countries){
  
  
  dt.ctry<-data.table(country=countries)
  dt.ctry[,country_id:=1:.N]
  
  #Note, this has a small difference over the original in sectoralmodel folder in that I make the country table here and have countries as an input. 
  
  industry.xwalk.raw<-fread(path_industry_xwalk)

  rawy<-as.data.table(t(fread(path_hybrid_prod)),keep.rownames=T)
  
  setnames(rawy,c("rn","V1","V4","V7" ,"V8"),c("country","industry","product","units","value"))
  
  
  
  prod_ind<-unique(rawy[,.(industry, product, units)])
  
  
  industry.xwalk.raw.product<-merge(industry.xwalk.raw,prod_ind[,.(sector.old = industry, product.old = product, units)],by="sector.old")
  
  #there is a problem in that there are many sectors with subsectors that have units in tonnes and units in tonnes (service). This doesn't seem meaningful in our context. 
  #     Tonnes (service) mainly seems to refer to re-processing or recycling services. 
  #the direct burning of fossil fuels for energy does not seem to be taken into account here. One could only calculate the exposure to a joules-related sector. 
  
  industry.xwalk.raw.product[units=="tonnes (service)", units :="tonnes"]

  #now just create a new sector scheme sector and unit together
  
  industry.xwalk.raw.product[,sector_units:=paste0(sector,"|",units)]
  
  #set the energy sectors as those with Joules. 
  energy_sectors<-unique(industry.xwalk.raw.product[gsub(".*\\|","",sector_units)=="TJ",sector_units])
  
  
  setnames(industry.xwalk.raw.product,"sector","industry.CPRS")
  
  industry.xwalk.raw.product[,sector:=sector_units]
  industry.xwalk.raw.product[,sector_units:=NULL]
  
  dt.energy_sectors=data.table( energy_names=energy_sectors)
  
  industry.xwalk.raw.product[,Number.average:=mean(as.double(Number.old)), by="sector"]
  dt.energy_sectors[,sector_id:=1:.N]
  
  sector_n<-data.table( unique(industry.xwalk.raw.product[,.(sector,Number.average)]))
  sector_n[order(Number.average)]
  sector_n[,sector_id_temp:=1:.N]
  sector_n<-merge(sector_n,dt.energy_sectors, all.x=T, by.x="sector", by.y="energy_names")
  sector_n[order(sector_id,sector_id_temp)]
  sector_n[is.na(sector_id), sector_id:=nrow(dt.energy_sectors)+(1:.N)]
  industry.xwalk.raw.product<-merge(industry.xwalk.raw.product, sector_n[,.(sector, sector_id)], by=c("sector"),all.x=T)
  
  
  industry.xwalk.raw.product<-industry.xwalk.raw.product[order(sector_id)]
  
  
  dt.industry.country1<-CJ(country_id=unique(dt.ctry[order(country_id),country_id]), sector_id=unique(industry.xwalk.raw.product[,sector_id]))
  dt.industry.country1<-merge(dt.industry.country1,unique(dt.ctry[,.(country_id, country)]) , by= "country_id")
  dt.industry.country1<-dt.industry.country1[order(country_id, sector_id)]
  dt.industry.country1[,id:=1:.N]
  
  dt.industry.country2<-merge(dt.industry.country1,unique(industry.xwalk.raw.product[,.(sector_id,sector)]) , by="sector_id")
  dt.industry.country2<-dt.industry.country2[order(id)]
  
  myout=list(industry.tab = industry.xwalk.raw.product, industry.cntry.tab = dt.industry.country2)
}



gen_emissions.sat<-function(F.sat.long.my, x1.my){
  
  form1<-as.formula(paste0("sector_id.out+country_id.out+id.out~stressor"))
  F.sat.wide<-dcast(F.sat.long.my,form1 ,value.var ="value")
  setnames(F.sat.wide, ghg_cat , c("CO2","CH4","N2O"))
  F.sat.wide<-merge(F.sat.wide, x1.my[,.(id.out=id,gross_output)], by=c("id.out"))
  #merging in gross output so we can get emissions per dollar output
  F.sat.wide<-F.sat.wide[order(id.out)]
  #formula accordin to ghgprotocol.org/sites/default/files/ghgp/Global-Warming-Potential-Values%20%28Feb%2016%202016%29_1.pdf
  F.sat.wide[, GWP100:= CO2+28*CH4+265*N2O]
  F.sat.wide[,Emissions_grameuro:=GWP100*1000/(gross_output*1e6)]
  
  F.sat.wide
}

gen_emissions.imp<-function(F.imp.long.my, x1.my){
  
  F.imp.long.my[impact==rowvars_impact[2],impact:="GHG_GWP100"]
  
  form1<-as.formula(paste0("sector_id.out+country_id.out+id.out~impact"))
  F.imp.wide<-dcast(F.imp.long.my,form1 ,value.var ="value")
  F.imp.wide<-merge(F.imp.wide, x1.my[,.(id.out=id,gross_output)], by=c("id.out"))
  #merging in gross output so we can get emissions per dollar output
  F.imp.wide<-F.imp.wide[order(id.out)]
  #formula accordin to ghgprotocol.org/sites/default/files/ghgp/Global-Warming-Potential-Values%20%28Feb%2016%202016%29_1.pdf
  F.imp.wide[,Emissions_grameuro:=GHG_GWP100*1000/(gross_output*1e6)]
  F.imp.wide
}



