####################################################################
# [Update 11/22/23] add NiGEM deltas. Using 9-other disaggregation.(hence v4 xwalks.) 
#     Phase IV forecasts, and KLEMS shares. 
# 
####################################################################

rm(list=ls())


library(parallel)
detectCores()
library(ggplot2)
library(grid)
library(gridExtra)
library(data.table)
library(readxl)
library(tidyverse)
library(ggtext)
library(glue)

#Set working directory here. This would include the functions folder. 
path_wd <- "~/workspace/climate/NGFS/hybMRIO_IAM_v2/" 
setwd(path_wd)


path_data <-"~/data/IO/EXIOBASE/IOT_2022_ixi/"
path_data_IAM <-"~/data/macro/IAM_out/"

#run_id<-"2023-11-13_15-58"
run_id<-"2023-11-22_15-05"

path_run_out <-paste0(path_wd,"run/",run_id,"/out/" )
path_input <-paste0(path_wd,"input_data/" )
path_graphs <-paste0(path_wd,"run/",run_id,"/graphs/" )
path_output<-paste0(path_wd,"output_data/")

industries.cprs<-readRDS(paste0(path_run_out,"industries.cprs.v4.RDS"))
N<-length(unique(industries.cprs$industry.tab[,sector]))
country_iam_xwalk<-fread(paste0(path_input,"/country_IAM_xwalk.csv"))


unique(industries.cprs$industry.cntry.tab[,country])

source("functions/parse_iam.R")



xwalk.CPRS_IAM<-fread(paste0(path_input,"/cprs_sector_iam_v4.csv"))
xwalk.CPRS_IAM[, I_standalone:=0]
xwalk.CPRS_IAM[sector.iam!="", I_standalone:=1]



set_sectors.IO<-unique(xwalk.CPRS_IAM[I_standalone==0, sector])
dt.all<-fread(paste0(path_run_out,"final_energy_series.csv" ))


dt.all[,series.2020:=series[year==2020],.(model.short, scenario.short, id,country,sector)]
dt.all[,series.2050:=series[year==2050],.(model.short, scenario.short, id,country,sector)]


#############################################################################
# NiGEM delta - loading and merging country and scenario crosswalks to match to EXIOBASE.
#         Note, we are using NiGEM as an *input*
#############################################################################
dt.nigem.deltas.wide<-setDT(read_xlsx(paste0(path_input,"msg1030_1102_sec-01.xlsx"),sheet=2))

dt.nigem.deltas<-melt(dt.nigem.deltas.wide, 
                      id.vars=c("Model","scenario","region","variable","unit"),
                      variable.name="year")

### make a country crosswalk with what you have right now.
#fwrite(unique(dt.nigem.deltas[,.(region)]),paste0(path_input, "xwalk.region_nigem_exiobase.csv"))

xwalk.region_nigem_exiobase<-fread(paste0(path_input,  "xwalk.region_nigem_exiobase.csv"),sep=",")
#fwrite( unique(dt.nigem.deltas[,.(scenario)]),paste0(path_input,"scenario_name_xwalk_v4.csv"))
scenario_name_xwalk<-fread(paste0(path_input,"scenario_name_xwalk_v4.csv"))

dt.nigem.deltas<-merge(dt.nigem.deltas, xwalk.region_nigem_exiobase[,.(region=region.nigem,country.exiobase)], by="region")
dt.nigem.deltas<-merge(dt.nigem.deltas, scenario_name_xwalk[,.(scenario=scenario.code,scenario.short)], by="scenario")

dt.nigem.deltas[Model=="NiGEM NGFS v1.23.2[MESSAGEix-GLOBIOM 1.1-M-R12]",model.short:="message"]


#############################################################################
# Load KLEMS shares and capital stock/labor levels For certain very small sectors in small countries, we may be missing capital and labor. 
#     For output=0 country-sectors, we're missing shares.
#############################################################################

klems_shares<-fread(paste0(path_output,"klems_shares.csv"))

#need to add id var
klems_shares<-merge(klems_shares, industries.cprs$industry.cntry.tab[,.(country, sector_id,id)],by=c("country","sector_id"))

klems_shares[,IIE_share:=II_share*energy_share_II]
klems_shares[,IINE_share:=II_share*(1-energy_share_II)]





#############################################################################
# 1) Merge KLEMS shares and do initial calculations without NiGEM deltas. 
#       also with keeping capital and labor together.
#############################################################################

dt.all.out<-merge(dt.all, klems_shares[,.(id, labor_share, cap_share, IIE_share, IINE_share, II_share)] ,by="id")


dt.all.out[,Omega.2020 := gross_output.2020^(1-II_share)*series.2020^(II_share)]
dt.all.out[,series.out1 :=Omega.2020 * (series*gross_output.2020)^II_share]

##summarize
source("functions/general.R")

dt.summary.output1<-summarystats(dt.all.out,"series.out1","current","netzero", 2050,2050)

###########
# Without intensity change
###########
source("functions/graphs.R")

dt.summary.output1[country=="US"]
mycountry<-"US"
graph_sector_iam.2(dt.summary.output1[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050","netzero.ov.current.output_noint.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/method1_" ),N,set_sectors.IO,1)





#############################################################################
# 2) Separate capital and labor and use levels from KLEMS.
#         Results, in percentage terms are exactly the same. 
#############################################################################

dt.all.out<-merge(dt.all, klems_shares[,.(id, labor_share, cap_share, IIE_share, IINE_share, II_share,Kq_GFCF ,   H_EMP)] ,by="id")

dt.all.out[,Kq_GFCF.2020:=Kq_GFCF[year==2020],.(model.short, scenario.short, id,country,sector)]
dt.all.out[,H_EMP.2020:=H_EMP[year==2020],.(model.short, scenario.short, id,country,sector)]



dt.all.out[,A.2020 := gross_output.2020/(Kq_GFCF.2020^cap_share*H_EMP.2020^labor_share*series.2020^II_share)]
dt.all.out[,series.out2 :=A.2020 * (Kq_GFCF^cap_share*H_EMP^labor_share*series^II_share)]

dt.summary.output2<-summarystats(dt.all.out,"series.out2","current","netzero", 2050,2050)
mycountry<-"US"
graph_sector_iam.2(dt.summary.output2[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/method2_" ),N,gsub("\\|","\\\\|",set_sectors.IO),0)

dt.summary.output1[country=="US"]
dt.summary.output2[country=="US"]


#############################################################################
# 3) Separate capital and labor and use levels from KLEMS.
#        Then, modify capital, labor, and energy intensity according to NiGEM deltas. 
#           First do capital and labor, then do energy intensity (series.out4)
#       these are scenario specific values and not sector specific. 
#############################################################################


#first try capital stock, employees (in employment), and volume energy use

dt.nigem.deltas[variable=="Employees (in employment) ; thousands(transition)",variable.short:="labor_delta"]
dt.nigem.deltas[variable=="Capital stock (equilibrium)(transition)",variable.short:="capital_delta"]
dt.nigem.deltas[variable=="Volume energy use as a share of GDP ; Bn US$(PPP)(transition)",variable.short:="energy_use_delta"]

dt.nigem.deltas[scenario.short=="current"]

dt.nigem.deltas.short<-dcast(dt.nigem.deltas[!is.na(variable.short)&(scenario.short!="")&country.exiobase!=""],
                             scenario.short+ model.short + country.exiobase+year~variable.short,
                             value.var = "value")
dt.nigem.deltas.short[,year_n:=as.numeric(year)+2021]
dt.nigem.deltas.short[,year:=NULL]
setnames(dt.nigem.deltas.short,"year_n","year")
#####

dt.all.out<-merge(dt.all, klems_shares[,.(id, labor_share, cap_share, IIE_share, IINE_share, II_share,Kq_GFCF ,   H_EMP)] ,by="id")
dt.all.nigem<-merge(dt.all.out[model.short=="message"&year<=2050], 
                    dt.nigem.deltas.short[,.(scenario.short,
                                             model.short,
                                             country=country.exiobase,
                                             year,
                                             capital_delta, 
                                             labor_delta,
                                             energy_use_delta)] ,by=c("scenario.short","model.short","country","year"),all.x=T)



dt.all.nigem[,Kq_GFCF.2020:=Kq_GFCF[year==2020],.(model.short, scenario.short, id,country,sector)]
dt.all.nigem[,H_EMP.2020:=H_EMP[year==2020],.(model.short, scenario.short, id,country,sector)]



#deltas for current scenario are 0 
dt.all.nigem[scenario.short=="current",capital_delta:=0]
dt.all.nigem[scenario.short=="current",labor_delta:=0]
dt.all.nigem[scenario.short=="current",energy_use_delta:=0]


dt.all.nigem[,A.2020 := gross_output.2020/((Kq_GFCF.2020)^cap_share*H_EMP.2020^labor_share*series.2020^II_share)]
dt.all.nigem[,series.out3 :=A.2020 * ((Kq_GFCF*(1+capital_delta/100))^cap_share*(H_EMP*(1+labor_delta/100))^labor_share*(series)^II_share)]



dt.summary.output3<-summarystats(dt.all.nigem,"series.out3","current","netzero", 2050,2050)
mycountry<-"US"
graph_sector_iam.2(dt.summary.output3[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/method3_" ),N,set_sectors.IO,1)


##############
# (4) Add in energy intensity
##############



dt.all.nigem[,A.2020 := gross_output.2020/((Kq_GFCF.2020)^cap_share*H_EMP.2020^labor_share*series.2020^II_share)]
dt.all.nigem[,series.out4 :=A.2020 * ((Kq_GFCF*(1+capital_delta/100))^cap_share*(H_EMP*(1+labor_delta/100))^labor_share*(series/(1+energy_use_delta/100))^II_share)]


dt.summary.output_en<-summarystats(dt.all.nigem,"series","current","netzero", 2050,2050)


dt.summary.output4<-summarystats(dt.all.nigem,"series.out4","current","netzero", 2050,2050)
mycountry<-"US"
graph_sector_iam.2(dt.summary.output4[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/method4_" ),N,set_sectors.IO,1)


#######################
#     Do a side by side graph! Also make it better for beamer slides. 
#######################

dt.summary.outputsidebyside<-rbind( dt.summary.output_en[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="en_only",id ,country,sector ,my.series.2.over.1)],
                                    dt.summary.output2[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="nodeltas",id ,country,sector ,my.series.2.over.1)],
      dt.summary.output3[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="deltas_caplab",id ,country,sector ,my.series.2.over.1)],
      dt.summary.output4[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="deltas_caplab_enint",id ,country,sector ,my.series.2.over.1)])

mycountry<-"US"
graph_sector_iam.2(dt.summary.outputsidebyside[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/sidebyside_" ),N,gsub("\\|","\\\\|",set_sectors.IO),0)



mycountry<-"FR"
graph_sector_iam.2(dt.summary.outputsidebyside[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/sidebyside_" ),N,set_sectors.IO,1)


#############################################################################
# 5) Do method 2, 
#         assume Leontief for energy and non-energy intermediates
#############################################################################


dt.all.out<-merge(dt.all, klems_shares[,.(id, labor_share, cap_share, IIE_share, IINE_share, II_share,Kq_GFCF ,   H_EMP)] ,by="id")
dt.all.nigem<-merge(dt.all.out[model.short=="message"&year<=2050], 
                    dt.nigem.deltas.short[,.(scenario.short,
                                             model.short,
                                             country=country.exiobase,
                                             year,
                                             capital_delta, 
                                             labor_delta,
                                             energy_use_delta)] ,by=c("scenario.short","model.short","country","year"),all.x=T)

dt.all.nigem[,Kq_GFCF.2020:=Kq_GFCF[year==2020],.(model.short, scenario.short, id,country,sector)]
dt.all.nigem[,H_EMP.2020:=H_EMP[year==2020],.(model.short, scenario.short, id,country,sector)]

dt.all.nigem[scenario.short=="current",capital_delta:=0]
dt.all.nigem[scenario.short=="current",labor_delta:=0]
dt.all.nigem[scenario.short=="current",energy_use_delta:=0]


dt.all.nigem[,A.2020 := gross_output.2020/(Kq_GFCF.2020^cap_share*H_EMP.2020^labor_share*(IIE_share*series.2020)^II_share)]
dt.all.nigem[,series.out5 :=A.2020 * (Kq_GFCF^cap_share*H_EMP^labor_share*(IIE_share*series)^II_share)]



dt.summary.output5<-summarystats(dt.all.nigem,"series.out5","current","netzero", 2050,2050)
mycountry<-"US"
graph_sector_iam.2(dt.summary.output5[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/method5_" ),N,gsub("\\|","\\\\|",set_sectors.IO),0)


dt.summary.outputsidebyside2<-rbind( 
                                    dt.summary.output2[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="nodeltas",id ,country,sector ,my.series.2.over.1)],
                                    dt.summary.output5[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="leontiefII",id ,country,sector ,my.series.2.over.1)])

mycountry<-"US"
graph_sector_iam.2(dt.summary.outputsidebyside2[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/sidebyside2_" ),N,set_sectors.IO,1)


#it's the same as method 2!


#############################################################################
# 6) Method 2 but modifying EOS. KL(EM) structure
#############################################################################


dt.all.out<-merge(dt.all, klems_shares[,.(id, labor_share, cap_share, IIE_share, IINE_share, II_share,Kq_GFCF ,   H_EMP)] ,by="id")
dt.all.nigem<-merge(dt.all.out[model.short=="message"&year<=2050], 
                    dt.nigem.deltas.short[,.(scenario.short,
                                             model.short,
                                             country=country.exiobase,
                                             year,
                                             capital_delta, 
                                             labor_delta,
                                             energy_use_delta)] ,by=c("scenario.short","model.short","country","year"),all.x=T)

dt.all.nigem[,Kq_GFCF.2020:=Kq_GFCF[year==2020],.(model.short, scenario.short, id,country,sector)]
dt.all.nigem[,H_EMP.2020:=H_EMP[year==2020],.(model.short, scenario.short, id,country,sector)]

dt.all.nigem[scenario.short=="current",capital_delta:=0]
dt.all.nigem[scenario.short=="current",labor_delta:=0]
dt.all.nigem[scenario.short=="current",energy_use_delta:=0]

dt.all.nigem[,sigma_KL:=.91]
dt.all.nigem[,sigma_KL_E:=.3]

dt.all.nigem[,rho_KL:=(sigma_KL-1)/(sigma_KL)]
dt.all.nigem[,rho_KL_E:=(sigma_KL_E-1)/(sigma_KL_E)]


dt.all.nigem[,KL.2020:=(cap_share/(cap_share+labor_share)*Kq_GFCF.2020^rho_KL+(labor_share/(cap_share+labor_share)*H_EMP.2020)^rho_KL)^(1/rho_KL)]

dt.all.nigem[,A.2020 := gross_output.2020/((cap_share+labor_share)*KL.2020^rho_KL_E+
                                              II_share*(IIE_share*series.2020)^rho_KL_E)^(1/rho_KL_E)]

dt.all.nigem[,KL:=(cap_share/(cap_share+labor_share)*Kq_GFCF^rho_KL+(labor_share/(cap_share+labor_share)*H_EMP)^rho_KL)^(1/rho_KL)]

dt.all.nigem[,series.out6 :=A.2020 * ((cap_share+labor_share)*KL^rho_KL_E+II_share*(IIE_share*series)^rho_KL_E)^(1/rho_KL_E)]



dt.summary.output6<-summarystats(dt.all.nigem,"series.out6","current","netzero", 2050,2050)
mycountry<-"US"
graph_sector_iam.2(dt.summary.output6[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/method6_" ),N,gsub("\\|","\\\\|",set_sectors.IO),0)


dt.summary.outputsidebyside2<-rbind( 
  dt.summary.output2[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="nodeltas",id ,country,sector ,my.series.2.over.1)],
  dt.summary.output6[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="nodeltasEOS1.01",id ,country,sector ,my.series.2.over.1)])

mycountry<-"US"
graph_sector_iam.2(dt.summary.outputsidebyside2[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/sidebyside6a_" ),N,set_sectors.IO,1)



#############################################################################
# 7) Method  (enint and caplabdeltas) but modifying EOS. KL(EM) structure. 7a only using IIE_share (lower bound on effects)
#           
#############################################################################



dt.all.out<-merge(dt.all, klems_shares[,.(id, labor_share, cap_share, IIE_share, IINE_share, II_share,Kq_GFCF ,   H_EMP)] ,by="id")
dt.all.nigem<-merge(dt.all.out[model.short=="message"&year<=2050], 
                    dt.nigem.deltas.short[,.(scenario.short,
                                             model.short,
                                             country=country.exiobase,
                                             year,
                                             capital_delta, 
                                             labor_delta,
                                             energy_use_delta)] ,by=c("scenario.short","model.short","country","year"),all.x=T)

dt.all.nigem[,Kq_GFCF.2020:=Kq_GFCF[year==2020],.(model.short, scenario.short, id,country,sector)]
dt.all.nigem[,H_EMP.2020:=H_EMP[year==2020],.(model.short, scenario.short, id,country,sector)]

#fwrite(unique(dt.all.nigem[,.(sector)]),paste0(path_input,"baccianti_sigmas.csv"))

baccianti_macrosectors<-fread(paste0(path_input,"baccianti_macrosectors.csv"))
#fwrite(unique(baccianti_macrosectors[,.(macrosector)]),paste0(path_input,"baccianti_sigmas.csv"))
baccianti_sigmas<-fread(paste0(path_input,"baccianti_sigmas.csv"))

baccianti_sigmas1<-merge(baccianti_macrosectors,baccianti_sigmas,by="macrosector")

#baccianti_sigmas1[,sigma_KL_E:=sigma_KL_E*3.5]

dt.all.nigem<-merge(dt.all.nigem,baccianti_sigmas1,by="sector")


dt.all.nigem[scenario.short=="current",capital_delta:=0]
dt.all.nigem[scenario.short=="current",labor_delta:=0]
dt.all.nigem[scenario.short=="current",energy_use_delta:=0]

# dt.all.nigem[,sigma_KL:=.91]
# dt.all.nigem[,sigma_KL_E:=.3]
#dt.all.nigem[,sigma_EM:=2]


dt.all.nigem[,rho_KL:=(sigma_KL-1)/(sigma_KL)]
dt.all.nigem[,rho_KL_E:=(sigma_KL_E-1)/(sigma_KL_E)]
#dt.all.nigem[,rho_EM:=(sigma_EM-1)/(sigma_EM)]

dt.all.nigem[,KL.2020:=(cap_share/(cap_share+labor_share)*Kq_GFCF.2020^rho_KL+(labor_share/(cap_share+labor_share)*H_EMP.2020)^rho_KL)^(1/rho_KL)]

#dt.all.nigem[,EM.2020:=(IIE_share/(II_share)*series.2020^rho_EM+((1-II_share)/(II_share)*materials.2020)^rho_EM)^(1/rho_EM)]


#dt.all.nigem[,A.2020 := gross_output.2020/((cap_share+labor_share)*KL.2020^rho_KL_E+
#                                             IIE_share*series.2020^rho_KL_E)^(1/rho_KL_E)]

#11/26 using II_share now
dt.all.nigem[,A.2020 := gross_output.2020/((cap_share+labor_share)*KL.2020^rho_KL_E+
                                             II_share*series.2020^rho_KL_E)^(1/rho_KL_E)]


dt.all.nigem[,KL:=(cap_share/(cap_share+labor_share)*((1+capital_delta/100)*Kq_GFCF)^rho_KL+(labor_share/(cap_share+labor_share)*((1+labor_delta/100)*H_EMP))^rho_KL)^(1/rho_KL)]
#dt.all.nigem[,EM:=(IIE_share/(II_share)*(series/(1+energy_use_delta/100))^rho_EM+((1-IIE_share)/(II_share)*materials.2020)^rho_EM)^(1/rho_EM)]

#dt.all.nigem[,series.out7 :=A.2020 * ((cap_share+labor_share)*KL^rho_KL_E+IIE_share*(series/(1+energy_use_delta/100))^rho_KL_E)^(1/rho_KL_E)]

dt.all.nigem[,series.out7 :=A.2020 * ((cap_share+labor_share)*KL^rho_KL_E+II_share*(series/(1+energy_use_delta/100))^rho_KL_E)^(1/rho_KL_E)]


dt.summary.output7<-summarystats(dt.all.nigem,"series.out7","current","netzero", 2050,2050)
mycountry<-"US"
graph_sector_iam.2(dt.summary.output7[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/method7_" ),N,gsub("\\|","\\\\|",set_sectors.IO),0)


dt.summary.outputsidebyside2<-rbind( 
  dt.summary.output2[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="nodeltas",id ,country,sector ,my.series.2.over.1)],
  dt.summary.output7[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="EOSpaper_deltas",id ,country,sector ,my.series.2.over.1)])

mycountry<-"US"
graph_sector_iam.2(dt.summary.outputsidebyside2[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/sidebyside7_" ),N,set_sectors.IO,1)

##########################################
#7a.  - using IIE_share 
##########################################



dt.all.out<-merge(dt.all, klems_shares[,.(id, labor_share, cap_share, IIE_share, IINE_share, II_share,Kq_GFCF ,   H_EMP)] ,by="id")
dt.all.nigem<-merge(dt.all.out[model.short=="message"&year<=2050], 
                    dt.nigem.deltas.short[,.(scenario.short,
                                             model.short,
                                             country=country.exiobase,
                                             year,
                                             capital_delta, 
                                             labor_delta,
                                             energy_use_delta)] ,by=c("scenario.short","model.short","country","year"),all.x=T)

dt.all.nigem[,Kq_GFCF.2020:=Kq_GFCF[year==2020],.(model.short, scenario.short, id,country,sector)]
dt.all.nigem[,H_EMP.2020:=H_EMP[year==2020],.(model.short, scenario.short, id,country,sector)]

#fwrite(unique(dt.all.nigem[,.(sector)]),paste0(path_input,"baccianti_sigmas.csv"))

baccianti_macrosectors<-fread(paste0(path_input,"baccianti_macrosectors.csv"))
#fwrite(unique(baccianti_macrosectors[,.(macrosector)]),paste0(path_input,"baccianti_sigmas.csv"))
baccianti_sigmas<-fread(paste0(path_input,"baccianti_sigmas.csv"))

baccianti_sigmas1<-merge(baccianti_macrosectors,baccianti_sigmas,by="macrosector")

#baccianti_sigmas1[,sigma_KL_E:=sigma_KL_E*3.5]

dt.all.nigem<-merge(dt.all.nigem,baccianti_sigmas1,by="sector")


dt.all.nigem[scenario.short=="current",capital_delta:=0]
dt.all.nigem[scenario.short=="current",labor_delta:=0]
dt.all.nigem[scenario.short=="current",energy_use_delta:=0]

# dt.all.nigem[,sigma_KL:=.91]
# dt.all.nigem[,sigma_KL_E:=.3]
#dt.all.nigem[,sigma_EM:=2]


dt.all.nigem[,rho_KL:=(sigma_KL-1)/(sigma_KL)]
dt.all.nigem[,rho_KL_E:=(sigma_KL_E-1)/(sigma_KL_E)]
#dt.all.nigem[,rho_EM:=(sigma_EM-1)/(sigma_EM)]

dt.all.nigem[,KL.2020:=(cap_share/(cap_share+labor_share)*Kq_GFCF.2020^rho_KL+(labor_share/(cap_share+labor_share)*H_EMP.2020)^rho_KL)^(1/rho_KL)]

#dt.all.nigem[,EM.2020:=(IIE_share/(II_share)*series.2020^rho_EM+((1-II_share)/(II_share)*materials.2020)^rho_EM)^(1/rho_EM)]


dt.all.nigem[,A.2020 := gross_output.2020/((cap_share+labor_share)*KL.2020^rho_KL_E+
                                            IIE_share*series.2020^rho_KL_E)^(1/rho_KL_E)]



dt.all.nigem[,KL:=(cap_share/(cap_share+labor_share)*((1+capital_delta/100)*Kq_GFCF)^rho_KL+(labor_share/(cap_share+labor_share)*((1+labor_delta/100)*H_EMP))^rho_KL)^(1/rho_KL)]
#dt.all.nigem[,EM:=(IIE_share/(II_share)*(series/(1+energy_use_delta/100))^rho_EM+((1-IIE_share)/(II_share)*materials.2020)^rho_EM)^(1/rho_EM)]


dt.all.nigem[,series.out7a :=A.2020 * ((cap_share+labor_share)*KL^rho_KL_E+IIE_share*(series/(1+energy_use_delta/100))^rho_KL_E)^(1/rho_KL_E)]


dt.summary.output7a<-summarystats(dt.all.nigem,"series.out7a","current","netzero", 2050,2050)
mycountry<-"US"
graph_sector_iam.2(dt.summary.output7a[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/method7a_" ),N,gsub("\\|","\\\\|",set_sectors.IO),0)


dt.summary.outputsidebyside7_7a<-rbind( 
  dt.summary.output7[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="bacc13_deltas",id ,country,sector ,my.series.2.over.1)],
  dt.summary.output7a[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="bacc13_deltas_IIE",id ,country,sector ,my.series.2.over.1)])

mycountry<-"US"
graph_sector_iam.2(dt.summary.outputsidebyside7_7a[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/sidebyside7_7a_" ),N,set_sectors.IO,1)




#############################################################################
# 8) Method  (enint and caplabdeltas) but EOS from baccianti. Trying to incorporate materials
#             Conclusion : materials doesn't work, it's sensitive to units. 
#############################################################################


dt.all.out<-merge(dt.all, klems_shares[,.(id, labor_share, cap_share, IIE_share, IINE_share, II_share,Kq_GFCF ,   H_EMP)] ,by="id")
dt.all.nigem<-merge(dt.all.out[model.short=="message"&year<=2050], 
                    dt.nigem.deltas.short[,.(scenario.short,
                                             model.short,
                                             country=country.exiobase,
                                             year,
                                             capital_delta, 
                                             labor_delta,
                                             energy_use_delta)] ,by=c("scenario.short","model.short","country","year"),all.x=T)

dt.all.nigem[,Kq_GFCF.2020:=Kq_GFCF[year==2020],.(model.short, scenario.short, id,country,sector)]
dt.all.nigem[,H_EMP.2020:=H_EMP[year==2020],.(model.short, scenario.short, id,country,sector)]

#fwrite(unique(dt.all.nigem[,.(sector)]),paste0(path_input,"baccianti_sigmas.csv"))

baccianti_macrosectors<-fread(paste0(path_input,"baccianti_macrosectors.csv"))
#fwrite(unique(baccianti_macrosectors[,.(macrosector)]),paste0(path_input,"baccianti_sigmas.csv"))
baccianti_sigmas<-fread(paste0(path_input,"baccianti_sigmas.csv"))

baccianti_sigmas1<-merge(baccianti_macrosectors,baccianti_sigmas,by="macrosector")
dt.all.nigem<-merge(dt.all.nigem,baccianti_sigmas1,by="sector")


dt.all.nigem[scenario.short=="current",capital_delta:=0]
dt.all.nigem[scenario.short=="current",labor_delta:=0]
dt.all.nigem[scenario.short=="current",energy_use_delta:=0]

# dt.all.nigem[,sigma_KL:=.91]
# dt.all.nigem[,sigma_KL_E:=.3]
dt.all.nigem[,sigma_EM:=4]


dt.all.nigem[,rho_KL:=(sigma_KL-1)/(sigma_KL)]
dt.all.nigem[,rho_KL_E:=(sigma_KL_E-1)/(sigma_KL_E)]
dt.all.nigem[,rho_EM:=(sigma_EM-1)/(sigma_EM)]

dt.all.nigem[,materials:=.5]
dt.all.nigem[,materials.2020:=.5]

dt.all.nigem[,KL.2020:=(cap_share/(cap_share+labor_share)*Kq_GFCF.2020^rho_KL+(labor_share/(cap_share+labor_share)*H_EMP.2020)^rho_KL)^(1/rho_KL)]

dt.all.nigem[,EM.2020:=(IIE_share/(II_share)*series.2020^rho_EM+((1-II_share)/(II_share)*materials.2020)^rho_EM)^(1/rho_EM)]


dt.all.nigem[,A.2020 := gross_output.2020/((cap_share+labor_share)*KL.2020^rho_KL_E+
                                             II_share*EM.2020^rho_KL_E)^(1/rho_KL_E)]

dt.all.nigem[,KL:=(cap_share/(cap_share+labor_share)*((1+capital_delta/100)*Kq_GFCF)^rho_KL+(labor_share/(cap_share+labor_share)*((1+labor_delta/100)*H_EMP))^rho_KL)^(1/rho_KL)]
dt.all.nigem[,EM:=(IIE_share/(II_share)*(series/(1+energy_use_delta/100))^rho_EM+((1-IIE_share)/(II_share)*materials.2020)^rho_EM)^(1/rho_EM)]

dt.all.nigem[,series.out8 :=A.2020 * ((cap_share+labor_share)*KL^rho_KL_E+II_share*EM^rho_KL_E)^(1/rho_KL_E)]



dt.summary.output8<-summarystats(dt.all.nigem,"series.out8","current","netzero", 2050,2050)
mycountry<-"US"
graph_sector_iam.2(dt.summary.output8[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/method7_" ),N,gsub("\\|","\\\\|",set_sectors.IO),0)


dt.summary.outputsidebyside2<-rbind( 
  dt.summary.output2[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="nodeltas",id ,country,sector ,my.series.2.over.1)],
  dt.summary.output8[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="EOSpaper_deltas",id ,country,sector ,my.series.2.over.1)])

mycountry<-"US"
graph_sector_iam.2(dt.summary.outputsidebyside2[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/sidebyside8_" ),N,set_sectors.IO,1)


