
######################################
# 02_exiobase_run.R
# This script parses EXIOBASE 3 I/O tables and IAM series
#  and produces downscaled industrial sector final energy series
#  Output will be saved in a time stamped run folder. This should be used when subsequently running decompositions or output calculations. 
#
#   Make sure to change paths:  path_wd, path_data, path_output appropriately. 
#   also, users need to download 2022 ixi EXIOBASE from https://zenodo.org/record/5589597#.ZFvUtHbMKUl and save to the data folder. 
###############################



rm(list=ls())
library('parallel')
detectCores()
library(ggplot2)
library(grid)
library(gridExtra)
library(data.table)

###These are for highlighting IO sectors on overview graphs. 
library(tidyverse)
library(ggtext)
library(glue)

##ALERT - MESSAGE has unexpected series for non-ferrous, and middle east coal


#Set working directory here. This would include the functions folder. 
path_wd <- "C:/Users/boldrin/Sectoral Granularity/repo_final/" 
setwd(path_wd)


path_data <-"C:/Users/boldrin/Sectoral Granularity/repo_final/data/IO/EXIOBASE/IOT_2022_ixi/"
path_input <-paste0(path_wd,"input_data/" )

path_run<-paste0(path_wd,"run/",format(Sys.time(), "%F_%H-%M"),"/")
dir.create(paste0(path_wd,"run/"))
dir.create(file.path(paste0(path_run)))
path_graphs <-paste0(path_run,"graphs/" )
dir.create(file.path(path_graphs))
path_run_out<-paste0(path_run,"out/")

dir.create(file.path(path_run_out))

tt2 <- ttheme_minimal()

I_graph_all<-0

##########################################################################
#    (1)    Define industries
##########################################################################
#This file contains the functions that parse the I/O tables from the pandas-style multirows to long data.table format. 

source("functions/parse_exiobase.R")
source("functions/graphs.R")


x1<-fread(paste0(path_data,"x.txt"))

## Make the industry crosswalks to associate CPRS sectors for NiGEM countries
# define the Energy sectors. 


EnergySectors<-c("1-fossil|coal","1-fossil|oil",
 "1-fossil|gas","2-utility|electricity|fossil|coal",
 "2-utility|electricity|fossil|oil", "2-utility|electricity|fossil|gas",
 "2-utility|electricity|renewable","2-utility|electricity|transmission") 


countries.nigem<-unique(x1[,region])
N_country<-length(countries.nigem)
industries.cprs<-gen_industry_tables(paste0(path_input, "CPRS_Xwalk_v4.csv"), EnergySectors, countries.nigem)
saveRDS(industries.cprs, paste0(path_run_out,"industries.cprs.v4.RDS"))

unique(industries.cprs$industry.tab[,.(sector_id, sector)])


##########################################################################
#   (2)     Loading IAM series and IAM-CPRS concordance
##########################################################################

# Load the IAM inputs. They're classified as being standard, upstream and downstream. 

xwalk.CPRS_IAM<-fread(paste0(path_input,"/cprs_sector_iam_v4.csv"))
country_iam_xwalk<-fread(paste0(path_input,"/country_IAM_xwalk.csv"))
industry.iam.tab<-unique(industries.cprs$industry.tab[,.(sector_id, sector)])
industry.iam.tab<-merge(industry.iam.tab, xwalk.CPRS_IAM[,.(sector,sector.iam, I_primsecenergy.iam=I_primsecenergy)] ,by="sector")
industry.iam.ctry.tab<-merge(industries.cprs$industry.cntry.tab,industry.iam.tab[,.(sector_id,sector.iam, I_primsecenergy.iam)] , by="sector_id")


industry.iam.ctry.tab<-merge(industry.iam.ctry.tab, country_iam_xwalk[,.(country=country.exiobase, country.gcam, country.remind, country.message)], by="country")
sector.other<-unique(industry.iam.ctry.tab[is.na(I_primsecenergy.iam),sector])
sector.standalone<-unique(industry.iam.ctry.tab[!is.na(I_primsecenergy.iam),sector])

##########################################################################
#   (3)   Load data,  generate Leontief inverse using nigem/cprs sectors. Also make ghosh inverse
##########################################################################

x1.small<-parse_exiobase_x(paste0(path_data, "x.txt"), industries.cprs)
Z.long<-parse_exiobase_Z(paste0(path_data, "Z.txt"),2, NULL,industries.cprs)

N<-length(unique(industries.cprs$industry.tab[,sector]))
dt.alpha_ij<-gen_alpha(Z.long, x1.small, industries.cprs)
dt.ghosh_ij<-gen_ghosh(Z.long, x1.small, industries.cprs)

rm(Z.long)

# Some sectors had 0 output and therefore requirements there are set there to 0. 
dt.alpha_ij[is.nan(coef),coef:=0]
dt.ghosh_ij[is.nan(coef),coef:=0]

###Save coefficients for the the Back of the Envelope calculation
fwrite(dt.alpha_ij,paste0(path_run_out,"dt.alpha_ij.csv"))
fwrite(dt.ghosh_ij,paste0(path_run_out,"dt.ghosh_ij.csv"))
dt.b_ij<-gen_invdt(dt.alpha_ij,industries.cprs)
dt.G_ij<-gen_invdt(dt.ghosh_ij,industries.cprs)




##########################################################################
#    (4)    Load satellite accounts with energy balances
##########################################################################

ghg_cat<-c("CO2 - combustion - air" ,"CH4 - combustion - air" ,"N2O - combustion - air")
energy_balances<-c("Energy Carrier Net Total","Energy Carrier Net NENE","Energy Carrier Net NTRA", "Energy Carrier Net TAVI",
                   "Energy Carrier Net TMAR",	"Energy Carrier Net TOTH",	"Energy Carrier Net TRAI",	"Energy Carrier Net TROA",	"Energy Carrier Net LOSS"	)


# Get energy accounts from ExioBase
F.sat.long<-parse_exiobase_F(paste0(path_data, "/satellite/F.txt"),2, "stressor",energy_balances,industries.cprs)

F.sat.long<-merge(F.sat.long, industries.cprs$industry.cntry.tab[,.(id.out=id,sector.out=sector,country.out =country)],by="id.out")
F.sat.long<-merge(F.sat.long, x1.small[,.(id.out=id,gross_output)],by="id.out")
F.sat.long[,TJperMeuro:=value/gross_output]




##########################################################################
#  (5)  Main calculations
#       Step 1)First make both input and output energy numbers.
#
#       *)  downstream exposure first using Ghosh inverse
#         intuition - measuring, per Meuro of input, the level of energy output
#         this is mostly relevant for the energy sectors
#
#       **) next exposure measure is using the Leontief inverse.
###
##########################################################################


dt.dws.energy<-merge(dt.G_ij,F.sat.long[stressor=="Energy Carrier Net Total",.(id.out, energy.out = value, 
                                                                               TJperMeuro.out = ifelse(is.nan(TJperMeuro),0,TJperMeuro))], by="id.out")

dt.dws.energy.sum<-dt.dws.energy[,.(TJperMeuro.out.sum=sum(value*TJperMeuro.out)), by=c("id.in","sector.in","country.in")]
dt.dws.energy.sum<-merge(dt.dws.energy.sum,F.sat.long[stressor=="Energy Carrier Net Total",.(id.in=id.out, 
                                                                                             TJperMeuro.direct = ifelse(is.nan(TJperMeuro),0,TJperMeuro))], by="id.in")




dt.ups.energy<-merge(dt.b_ij,F.sat.long[stressor=="Energy Carrier Net Total",.(id.in=id.out, energy.in = value, 
                                                                               TJperMeuro.in = ifelse(is.nan(TJperMeuro),0,TJperMeuro))], by="id.in")

dt.ups.energy.sum<-dt.ups.energy[,.(TJperMeuro.in.sum=sum(value*TJperMeuro.in)), by=c("id.out","sector.out","country.out")]
dt.ups.energy.sum<-merge(dt.ups.energy.sum,F.sat.long[stressor=="Energy Carrier Net Total",.(id.out, 
                                                                                             TJperMeuro.direct = ifelse(is.nan(TJperMeuro),0,TJperMeuro))], by="id.out")




dt.energy<-merge(dt.ups.energy.sum[,.(id = id.out, sector=sector.out, country= country.out, TJperMeuro.in.sum, TJperMeuro.direct)], 
                 dt.dws.energy.sum[,.(id = id.in, sector=sector.in, country= country.in, TJperMeuro.out.sum)], by=c("id","sector", "country"))

dt.energy<-merge(dt.energy, x1.small[,.(id=id,gross_output)],by="id")

dt.energy[, TJperMeuro.out.delta:=TJperMeuro.out.sum -TJperMeuro.direct ]


dt.energy1<-merge(dt.energy, industry.iam.ctry.tab[,.(id,sector.iam,I_primsecenergy.iam)],by="id")


head(dt.energy[order(-TJperMeuro.in.sum)][country=="US"],n=100L)


##########################################################################
#   (6)    loading IAM series and merging them with the weights.
##########################################################################




source("functions/parse_iam.R")

dt.iam.series<-loadiam(paste0(path_input, "IAM_data_extract_phaseV.csv"))

unique(dt.iam.series[,series])

dt.iam.series<-addseries(dt.iam.series,c("Secondary Energy|Electricity|Coal","Secondary Energy|Electricity|Gas","Secondary Energy|Electricity|Oil")
                         ,"Secondary Energy|Electricity|Fossil Fuel")

unique(dt.iam.series[,series])



dt.iam.series<-addseries(dt.iam.series,c("Secondary Energy|Electricity|Non-Biomass Renewables","Secondary Energy|Electricity|Biomass"),"Secondary Energy|Electricity|Renewable")
dt.iam.series<-addseries(dt.iam.series,c("Final Energy|Transportation|Rail","Final Energy|Transportation|Road|Freight","Final Energy|Transportation|Road|Passenger"),"Final Energy|Transportation|Rail+Road")
dt.iam.series<-subtractseries(dt.iam.series,c("Final Energy|Industry"),c("Final Energy|Industry|Steel","Final Energy|Industry|Cement"),"Final Energy|Industry")

dt.iam.new.waterfall<-rbind( dt.iam.series[series=="Final Energy|Industry"&model.short=="remind",
                                          .(Model, Scenario, Unit, year, model.short, scenario.short, Region, value,series= "Final Energy|Industry|Non-ferrous metals")],
                             dt.iam.series[series=="Final Energy|Transportation|Aviation|Passenger"&model.short=="remind",
                                           .(Model, Scenario, Unit, year, model.short, scenario.short, Region, value,series= "Final Energy|Transportation|Aviation")],
                             dt.iam.series[series=="Final Energy|Transportation"&model.short=="message",
                                           .(Model, Scenario, Unit, year, model.short, scenario.short, Region, value,series= "Final Energy|Transportation|Rail+Road")],
                             dt.iam.series[series=="Final Energy|Transportation"&model.short=="message",
                                           .(Model, Scenario, Unit, year, model.short, scenario.short, Region, value,series= "Final Energy|Transportation|Maritime")],
                             dt.iam.series[series=="Final Energy|Transportation"&model.short=="message",
                                           .(Model, Scenario, Unit, year, model.short, scenario.short, Region, value,series= "Final Energy|Transportation|Aviation")])

dt.iam.series<-rbind(dt.iam.series, dt.iam.new.waterfall)

table(dt.iam.series[,series],dt.iam.series[,model.short])

#########################################

#Downscale the output based on the year 2020
dt.iam.2020<-dt.iam.series[year==2020, .(model.short, scenario.short,Region, series,output.iam.2020=value)]
dt.iam.series<-merge(dt.iam.series, dt.iam.2020, by=c("model.short", "scenario.short","Region", "series"))
dt.iam.series<-dt.iam.series[order(Region,model.short,scenario.short,series,Region,year)]


#linearly interpolate for missing entries (i.e. those that provide forecast every 5 vs 10 years after a certain point)
dt.iam.series[,value_before := nafill(value, "locf"),.(Region,model.short,scenario.short,series,Region)]
# Bring back the next non-missing dist
dt.iam.series[,value_after := nafill(value, "nocb"),.(Region,model.short,scenario.short,series,Region)]
dt.iam.series[is.na(value),value:= (value_before+value_after)/2]

dt.iam.series[model.short=="remind"&series=="Final Energy|Transportation|Aviation"]

if (I_graph_all==1){
  for (mymod in unique(dt.iam.series[,model.short])){
    
    for (myseries in unique(dt.iam.series[model.short==mymod,series])){
      # for (mycountr in unique(industry.iam.ctry.tab[,country])){
      for (myregion in unique(dt.iam.series[model.short==mymod,Region])){
        print(mymod)
        
        print(myseries)
        print(myregion)
        
        graphseries.iam(dt.iam.series,mymod,myseries,myregion,paste0(path_graphs,"iam/"))
      }
    }
  }
}





#### Commence merge to EXIOBASE series. 

country_iam_xwalk.long<-melt(country_iam_xwalk[,.(country.exiobase, country.gcam, country.remind, country.message)], value.vars=c("country.gcam","country.remind", "country.message"), id.vars="country.exiobase")
country_iam_xwalk.long[,model.short:=gsub(".*\\.","", variable)]
country_iam_xwalk.long[,variable:=NULL]

dt.energy3<-merge(dt.energy1, country_iam_xwalk.long[,.(Region=value, model.short, country=country.exiobase)], by=c("country") ,allow.cartesian = T)

dt.energy.iam<-merge(dt.energy3,dt.iam.series,by.x=c("sector.iam","Region","model.short"),by.y=c("series","Region","model.short" ),allow.cartesian = T)
setnames(dt.energy.iam,"Region","region.iam")

# some very few series in message are 0 in 2020 which causes a bug. 
floor_out<-1e-3
dt.energy.iam[, output.iam.downscale:=ifelse(output.iam.2020!=0,value/output.iam.2020,value/floor_out)]


##########################################################################
# (7) Compute primary and secondary energy series
##########################################################################


dt.primsec.iam<-dt.energy.iam[I_primsecenergy.iam==1,.(id, sector,  country, Model,region.iam,Scenario,model.short, scenario.short, year,TJ.out.2020=output.iam.downscale*TJperMeuro.out.sum, gross_output.2020=gross_output )]



if (I_graph_all==1){
  for (mysec in unique(dt.primsec.iam[,sector])){
    # for (mycountr in unique(industry.iam.ctry.tab[,country])){
    for (mymod in unique(dt.primsec.iam[,model.short])){
      graphseries.nigem.2(dt.primsec.iam,mymod,mysec,"US","TJ.out.2020",paste0(path_graphs,"monetary/primsec/"))
    }
    # }
  }
}


##########################################################################
# (8) For series that can be matched to the IAM
#       we take the upstream energy intensity as the energy series in question.
#       We assume energy intensity remains constant but usage goes down or up according to IAM index. 
#         So you can think of this as an energy series indexed to 2020
#         This will then be fed into the downstream industries. 
#         this gives E^3_{IAM}(t)
##########################################################################


dt.standalone.iam<-dt.energy.iam[!is.na(I_primsecenergy.iam),
                                 .(id, sector,  country,region.iam, Model,Scenario,model.short, scenario.short, year,
                                   TJ.out.2020=output.iam.downscale*TJperMeuro.in.sum, gross_output.2020=gross_output )]

fwrite(dt.standalone.iam, paste0(path_run_out ,"dt.standalone.iam.csv" ))


if (I_graph_all==1){
  for (mycountry in unique(dt.standalone.iam[,country])){
    
    for (mysec in unique(dt.standalone.iam[,sector])){
      print(mycountry)
      print(mysec)
      for (mymod in unique(dt.standalone.iam[,model.short])){
        graphseries.nigem.2(dt.standalone.iam,mymod,mysec,mycountry,"TJ.out.2020",paste0(path_graphs,"monetary/standalone/"))
      }
      # }
    }
  }
}

##########################################################################
# (9) For all others, 
#
#
# E^3_{IO}(t)  = (E^1_{IO}(t) + E^3_{IAM}(t) A_{IAM,IO})(1-A_{IO, IO})^-1
##########################################################################


##########################################################################
# (9a) Derive E^1_{IO}(t)
#     Take direct sum of TJperOutput 
##########################################################################




dt.temp<- unique(dt.iam.series[order(scenario.short, model.short, year),.(scenario.short, model.short, Model, Scenario,year)])
dt.temp[,ind:=1:.N]
dt.other.iam<-merge(CJ(series=sector.other,ind=dt.temp[,ind], sorted=F),dt.temp, by="ind")
dt.other.iam<-dt.other.iam[order(series,scenario.short ,model.short, year)]
dt.other.iam.energy<-merge( dt.energy[,.(id,sector,country, TJperMeuro.direct)] ,dt.other.iam, by.x="sector",by.y="series",allow.cartesian=T)

dt.other.iam.energy<-dt.other.iam.energy[order(sector,id,country,scenario.short, model.short, year)]
dt.other.iam.energy[,output.iam.downscale:=TJperMeuro.direct]

dt.other.iam.energy<-merge(dt.other.iam.energy, 
                           country_iam_xwalk.long[,.(region.iam=value, model.short, country=country.exiobase)], by=c("country","model.short"))


fwrite(dt.other.iam.energy, paste0(path_run_out,"dt.other.iam.energy.csv" ))


model_xwalk<-unique(dt.other.iam.energy[,.(model.short, scenario.short, Model, Scenario)])

##########################################################################
# (9b) Derive (1-A_{IO, IO})^-1
##########################################################################


#create inverse block matrix for IO. 

industries.cprs.IO<-copy(industries.cprs)
industries.cprs.IO$industry.tab<-industries.cprs$industry.tab[sector%in%sector.other]
industries.cprs.IO$industry.cntry.tab<-industries.cprs$industry.cntry.tab[sector%in%sector.other]

dt.b_ij_IO<-gen_invdt(dt.alpha_ij[sector.in%in%sector.other&sector.out%in%sector.other],industries.cprs.IO)

fwrite(dt.b_ij_IO,paste0(path_run_out,"dt.b_ij_IO.csv" ))


##########################################################################
# (9c) Derive A_{IAM,IO}(1-A_{IO, IO})^-1
#this is to help with decompositions. 
##########################################################################

countries<-unique(industries.cprs.IO$industry.cntry.tab[,country])
list.L_0<-list()

for (mycountry in countries){
  print(mycountry)

  dt.alpha_IAM_IO<-dt.alpha_ij[sector.in%in%sector.standalone&sector.out%in%sector.other&country.out==mycountry] 
  dt.L_0<-merge(dt.alpha_IAM_IO[,.(id.in.iam=id.in, id.out.IO=id.out,value.alpha=coef)],dt.b_ij_IO[,.(id.in.IO=id.in, id.out.IO=id.out,value.b_ij=value)], 
                by.x="id.out.IO",by.y="id.in.IO", allow.cartesian = T)
  list.L_0[[mycountry]]<-dt.L_0[,.(value=sum(value.alpha*value.b_ij)),
                                by=c("id.in.iam","id.out.IO")] 
}




dt.alpha_IAMIO_b_IOIO<-rbindlist(list.L_0)


dt.alpha_IAMIO_b_IOIO<-merge(dt.alpha_IAMIO_b_IOIO, industries.cprs$industry.cntry.tab[,.(id.in.iam=id,sector.in=sector, country.in=country)],by="id.in.iam")
dt.alpha_IAMIO_b_IOIO<-merge(dt.alpha_IAMIO_b_IOIO, industries.cprs$industry.cntry.tab[,.(id.out.IO=id,sector.out=sector, country.out=country)],by="id.out.IO")

##This is then used in decompose.R
fwrite(dt.alpha_IAMIO_b_IOIO, paste0(path_run_out,"dt.alpha_IAMIO_b_IOIO.csv" ))


##########################################################################
# (9d) 
#  Take E^3_{IAM}(t) from step (2) and A_{IAM,IO}(1-A_{IO, IO})^-1 from (3c) calculate E^3_{IAM}(t) A_{IAM,IO}(1-A_{IO, IO})^-1
##########################################################################
countries<-unique(industries.cprs.IO$industry.cntry.tab[,country])
list.L_0<-list()
list.decomp <- list()

mycountry<-"US"

for (mycountry in countries){
  print(mycountry)
  dt.alphab_IAM_IO<-dt.alpha_IAMIO_b_IOIO[country.out==mycountry]
  
  #put in the same format as prior one
  dt.b_ij_IO_IO<-dt.b_ij_IO[country.out==mycountry,
                            .(id.in.IO = id.in, id.out.IO = id.out, value)]
  
  #E^3_{IAM}(t) A_{IAM,IO}(1-A_{IO, IO})^-1
  dt.L_0<-merge(dt.alphab_IAM_IO,dt.standalone.iam[,.(id,model.short, scenario.short, sector, year, Model, Scenario, country, TJ.out.2020)], 
                by.x="id.in.iam",by.y="id", allow.cartesian = T)
  
  #E^1_{IO}(t) (1-A_{IO, IO})^-1
  dt.L_1<-merge(dt.b_ij_IO_IO,dt.other.iam.energy[,.(id, model.short, scenario.short, sector, year, Model, Scenario, country,  TJperMeuro.direct)], 
                by.x="id.in.IO",by.y="id", allow.cartesian = T)
  
  
  
  dt.L_01<-rbind(dt.L_0[,.(id.in=id.in.iam, id.out.IO, model.short, scenario.short,year, value,TJ = TJ.out.2020)],
                 dt.L_1[,.(id.in=id.in.IO, id.out.IO, model.short, scenario.short,year, value,TJ = TJperMeuro.direct)])
  
  #E^3_{IAM}(t) A_{IAM,IO}(1-A_{IO, IO})^-1
  list.L_0[[mycountry]]<-dt.L_01[,.(series=sum(value*TJ)),
                                 by=c("id.out.IO","model.short", "scenario.short","year")]
  
  
  
  list.L_0[[mycountry]]<-merge( list.L_0[[mycountry]], industries.cprs$industry.cntry.tab[,.(id.out.IO=id,sector.out=sector, country.out=country)],by="id.out.IO")
  
  
  ##############################################################################
  #instead, we take the sum of the inputs by all years (area under the curve) and rank by that 
  dt.L_01_topwt.alt<-dt.L_01[year>=2020,.( series_sum = sum(value*TJ)),.(id.out.IO,id.in, model.short,scenario.short)]
  dt.L_01_topwt.alt<-merge( dt.L_01_topwt.alt, industries.cprs$industry.cntry.tab[,.(id.out.IO=id,sector.out=sector, country.out=country)],by="id.out.IO")
  dt.L_01_topwt.alt<-merge( dt.L_01_topwt.alt, industries.cprs$industry.cntry.tab[,.(id.in=id,sector.in=sector, country.in=country)],by="id.in")
  
  dt.L_01_topwt.alt<-dt.L_01_topwt.alt[order(id.out.IO,model.short ,scenario.short, -series_sum)]
  dt.L_01_topwt.alt[,rank:=1:.N,.(id.out.IO,model.short ,scenario.short)]
  dt.L_01_topwt.alt[,series_sum.pc:=series_sum/sum(series_sum),.(id.out.IO,model.short ,scenario.short) ]
  
  
  
  dt.L_01_topwt.alt[,sector.in.top5:=ifelse(rank<=5, paste0(rank,": ",country.in,",",sector.in), "all others")]
  
  list.decomp[[mycountry]]<-dt.L_01_topwt.alt[,
                                              .(series.coef.sum=sum(series_sum),series.coef.sum.pc=sum(series_sum.pc), rank = mean(rank),
                                                country.in =country.in[1],
                                                sector.in =sector.in[1]),
                                              .(id.out.IO,sector.out,country.out, sector.in.top5,model.short ,scenario.short)]
  
  
  ##############################################################################
  
  
}

dt.decomp<-rbindlist(list.decomp)

dt.E3_IO2<-rbindlist(list.L_0)
unique(dt.decomp[,country.out])


dt.E3_IO<-merge(dt.E3_IO2, unique(dt.other.iam.energy[,.(model.short, scenario.short, Model, Scenario)]) ,by=c("model.short", "scenario.short"))
setnames(dt.E3_IO, c("id.out.IO","country.out","sector.out"), c("id","country","sector"))


dt.E3_IO<-merge(dt.E3_IO, country_iam_xwalk.long[,.(country=country.exiobase,model.short, region.iam=value)],by=c("country","model.short"))


unique(dt.E3_IO[,sector])

graphseries.nigem.3(dt.E3_IO,dt.decomp,"message","9-other|food_processing","DE","series",paste0(path_graphs),0)

graphseries.nigem.3(dt.E3_IO,dt.decomp,"message","9-other|services|education","DE","series",paste0(path_graphs,"/series/"),0)

if (I_graph_all==1){
  for (mysec in unique(dt.E3_IO[,sector])){
    for (mycountry in unique(industry.iam.ctry.tab[,country])){
      for (mymod in unique(dt.E3_IO[,model.short])){
        print("------------------------------------------------")
        print(mycountry)
        print(mysec)
        print(mymod)
        graphseries.nigem.3(dt.E3_IO,dt.decomp,mymod,mysec,mycountry,"series",paste0(path_graphs),0)
      }
    }
  }
}

graphseries.nigem.3(dt.E3_IO,dt.decomp,"gcam","7-finance","US","series",paste0(path_graphs,"/series/"),0)


##########################################################################
# (9d) combine this with standalone series
#       save the final results
##########################################################################


dt.all<-rbind(dt.standalone.iam[,.(id, country, sector, region.iam, Model, Scenario, model.short, scenario.short, year, series = TJ.out.2020)],
              dt.E3_IO )

dt.all<-dt.all[order(model.short, scenario.short, id,year)]


dt.all[, L1.series:=shift(series), .(model.short, scenario.short, id)]
dt.all[, D1.series:=ifelse(L1.series!=0, (series-L1.series)/L1.series*100, 0), .(model.short, scenario.short, id)]


dt.all<-merge(dt.all,x1.small[,.(id, gross_output.2020=gross_output)],by="id")

fwrite(dt.all, paste0(path_run_out,"final_energy_series.csv" ))

fwrite(model_xwalk, paste0(path_run_out,"model_xwalk.v4.csv" ))



#######################################################################
#(10) summary graphs
#######################################################################

source("functions/general.R")


dt.netzero2050min2020<-summarystats(dt.all,"series","netzero","netzero", 2020,2050)

dt.netzero2050mincurr<-summarystats(dt.all,"series","current","netzero", 2050,2050)

head(dt.all[model.short=="remind"&country=="US"&sector=="1-fossil|coal",.(sector,series)],n=100L)
head(dt.all[model.short=="remind"&country=="US"&sector=="1-fossil|coal",.(sector,series)],n=100L)

head(dt.all[model.short=="remind"&country=="AU"&sector=="1-fossil|coal",.(sector,series)],n=100L)


head(dt.netzero2050mincurr[model.short=="remind"&country=="AU",.(sector,my.series.2.over.1)],n=100L)


#the CPRS xwalk marks if the series is a primary or secondary energy series, with NA if it's not standalone.
xwalk.CPRS_IAM[,I_standalone:=as.numeric(!is.na(I_primsecenergy))]

set_sectors.IO<-unique(xwalk.CPRS_IAM[I_standalone==0, sector])


for (mycountry in unique(industries.cprs$industry.cntry.tab[,country])){
  print(mycountry)
  graph_sector_iam.2(dt.netzero2050mincurr,country_iam_xwalk,model_xwalk,
                     mycountry,"Final Energy, Net Zero vs. Current Scenario, % Diff. in 2050","netzero.ov.current.energy.2050","my.series.2.over.1","% difference",
                     paste0(path_graphs,"overview/paper/" ),N,gsub("\\|","\\\\|",set_sectors.IO),0)
  
}

