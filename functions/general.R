
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}


lm_eqn <- function(df,formula){
  m <- lm(formula, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

summarystats<-function(my.dt, my.series, scenario1, scenario2, year1, year2){
  #my.dt[,series.year1:=get(my.series)[year==year1],.(model.short, scenario.short, id,country,sector)]
  # my.dt[,series.year1:=get(my.series)[year==year1],.(model.short, scenario.short, id,country,sector)]
  
  
  
  dt.year1.scenario1<-my.dt[year==year1&scenario.short==scenario1,
                            .SD,.SDcols=c("model.short",my.series  ,"id","country","sector")]
  dt.year2.scenario2<-my.dt[year==year2&scenario.short==scenario2,
                            .SD,.SDcols=c("model.short",my.series  ,"id","country","sector")]
  setnames(dt.year1.scenario1,my.series,"my.series.1")
  setnames(dt.year2.scenario2,my.series,"my.series.2")
  
  dt.2.over.1<-merge(dt.year1.scenario1,  dt.year2.scenario2
                     , by=c("model.short" ,"id","country","sector"))
  
  #dt.2.over.1[,my.series.2.over.1:=0.0]
  dt.2.over.1[,my.series.2.over.1:=(my.series.2/my.series.1-1.0)*100]
  dt.2.over.1[,my.series.2.over.1:=(my.series.2/my.series.1-1.0)*100]
}




sector_change<-function(dt){
  dt[,sector_short := sector]
  dt[sector=="Mining of coal and lignite; extraction of peat (10)", sector_short:="Mining of coal and lignite; extraction of peat"]
  dt[sector =="Production of electricity by fossil fuel, biomass, other; Steam supply", sector_short:= "Fossil fuel electricity; steam supply"]
  dt[sector =="Production of electricity by renewables", sector_short:= "Renewable electricity"]
  dt[sector =="Petroleum Refining, Gas Production and Distribution, Coke, Auto Fuel", sector_short:= "Petroleum refining, gas, auto fuel, coke"]
  dt[sector=="Air transport (62)", sector_short:="Air transport"]
  dt[sector=="Post and telecommunications (64)", sector_short:="Post and telecommunications"]
  dt[sector=="Hotels and restaurants (55)", sector_short:="Hotels and restaurants"]
  dt[sector=="Wholesale trade and commission trade, except of motor vehicles and motorcycles (51)", sector_short:="Wholesale trade and commission trade"]
  dt[sector=="Financial intermediation, except insurance and pension funding (65)", sector_short:="Financial intermediation"]
  dt[sector=="Real estate activities (70)", sector_short:="Real estate activities"]
  dt[sector==" \"\"Real estate activities (70)\"\"", sector_short:="Real estate activities"]
  dt[sector=="Insurance and pension funding, except compulsory social security (66)", sector_short:="Insurance and pension funding"]
  dt[sector=="Sale, maintenance, repair of motor vehicles, motor vehicles parts, motorcycles, motor cycles parts and accessoiries", sector_short:="Sale, maintenance, repair of motor vehicles"]
  dt[sector=="Retail trade, except of motor vehicles and motorcycles; repair of personal and household goods (52)", sector_short:="Retail trade"]
  
  dt
}



decompose_country<-function(mycountry,dt.wt.IO, dt.wt.IAM,dt.e1IO,dt.e3IAM ,mymodel_xwalk,myindustries.nigem){
  #########################################################################
  #         This is used in decompose.R. It could also be used in the main function. 
  #########################################################################
  
  
  dt.wt.IAM.small<-dt.wt.IAM[country.out==mycountry]
  
  dt.wt.IO.small<-dt.wt.IO[country.out==mycountry,
                           .(id.in.IO = id.in, id.out.IO = id.out, value)]
  
  #E^3_{IAM}(t) A_{IAM,IO}(1-A_{IO, IO})^-1
  dt.L_0<-merge(dt.wt.IAM.small,dt.e3IAM[,.(id,model.short, scenario.short, sector, year, Model, Scenario, country,  TJ.out.2020)], 
                by.x="id.in.iam",by.y="id", allow.cartesian = T)
  
  dt.L_1<-merge(dt.wt.IO.small,dt.e1IO[,.(id, model.short, scenario.short, sector, year, Model, Scenario, country,  TJperMeuro.direct)], 
                by.x="id.in.IO",by.y="id", allow.cartesian = T)
  
  
  
  dt.L_01<-rbind(dt.L_0[,.(id.in=id.in.iam, id.out.IO, model.short, scenario.short, year, value,TJ = TJ.out.2020)],
                 dt.L_1[,.(id.in=id.in.IO, id.out.IO, model.short, scenario.short, year, value,TJ = TJperMeuro.direct)])
  
  #E^3_{IAM}(t) A_{IAM,IO}(1-A_{IO, IO})^-1
  dt.L_01[,series:=value*TJ]
  setnames(dt.L_01,"id.out.IO", "id.out")
  
  # ##############################################################################
  # #making rank metrics
  # ##############################################################################
  # dt.L_01_topwt<-unique(dt.L_01[,.(id.in, id.out, value)])
  # dt.L_01_topwt<-merge( dt.L_01_topwt, industries.nigem$industry.cntry.tab[,.(id.out=id,sector.out=sector, country.out=country)],by="id.out")
  # dt.L_01_topwt<-merge( dt.L_01_topwt, industries.nigem$industry.cntry.tab[,.(id.in=id,sector.in=sector, country.in=country)],by="id.in")
  # 
  # dt.L_01_topwt<-dt.L_01_topwt[order(id.out, -value)]
  # 
  ##############################################################################
  #instead, we take the sum of the inputs by all years (area under the curve) and rank by that 
  dt.L_01_topwt.alt<-dt.L_01[year>=2020,.( series_sum = sum(series)),.(id.out,id.in, model.short,scenario.short)]
  dt.L_01_topwt.alt<-merge( dt.L_01_topwt.alt, myindustries.nigem$industry.cntry.tab[,.(id.out=id,sector.out=sector, country.out=country)],by="id.out")
  dt.L_01_topwt.alt<-merge( dt.L_01_topwt.alt, myindustries.nigem$industry.cntry.tab[,.(id.in=id,sector.in=sector, country.in=country)],by="id.in")
  
  dt.L_01_topwt.alt<-dt.L_01_topwt.alt[order(id.out,model.short ,scenario.short, -series_sum)]
  dt.L_01_topwt.alt[,rank:=1:.N,.(id.out,model.short ,scenario.short)]
  dt.L_01_topwt.alt[,series_sum.pc:=series_sum/sum(series_sum),.(id.out,model.short ,scenario.short) ]
  
  #Merging back to main
  ##############################################################################
  
  dt.L_01<-merge(dt.L_01,dt.L_01_topwt.alt[,.(id.out,id.in,model.short ,scenario.short,rank)],by=c("id.out","id.in","model.short" ,"scenario.short"))
  
  
  
  dt.L_01<-merge(dt.L_01, myindustries.nigem$industry.cntry.tab[,.(id.in=id,sector.in=sector, country.in=country)],by="id.in")
  dt.L_01<-merge(dt.L_01, myindustries.nigem$industry.cntry.tab[,.(id.out=id,sector.out=sector, country.out=country)],by="id.out")
  
  dt.L_01<-merge(dt.L_01, mymodel_xwalk ,by=c("model.short", "scenario.short"))
  
  
  dt.L_01[,sector.in.top5:=ifelse(rank<=5, paste0(rank,": ",country.in,",",sector.in), "all others")]
  
  dt.L_01.small<-dt.L_01[,.(series.coef.sum=sum(series), rank = mean(rank)),
                         .(id.out,sector.out,country.out, sector.in.top5,model.short ,scenario.short, Model, Scenario,year)]
  
  
}
