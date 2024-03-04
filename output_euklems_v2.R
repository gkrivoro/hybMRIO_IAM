####################################################################
#   [Update 2/23/24] Now, we include the EU-KLEMS elasticity estimates, 
#               that also leverage IEA country-level price and sectoral quantities.  
#               these leverage a VdW-style panel regression. 
#
#
#. PRIOR UPDATE: [Update 11/22/23] add NiGEM deltas. Using 9-other disaggregation.(hence v4 xwalks.) 
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
path_wd <- "~/workspace/climate/NGFS/codes/" 
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
source("functions/graphs.R")
source("functions/general.R")



xwalk.CPRS_IAM<-fread(paste0(path_input,"/cprs_sector_iam_v4.csv"))
xwalk.CPRS_IAM[, I_standalone:=0]
xwalk.CPRS_IAM[sector.iam!="", I_standalone:=1]



set_sectors.IO<-unique(xwalk.CPRS_IAM[I_standalone==0, sector])
dt.all<-fread(paste0(path_run_out,"final_energy_series.csv" ))


dt.all[,series.2020:=series[year==2020],.(model.short, scenario.short, id,country,sector)]
dt.all[,series.2050:=series[year==2050],.(model.short, scenario.short, id,country,sector)]


#############################################################################
# No NiGEM deltas, so skipping that step from first version.
#############################################################################


#############################################################################
# Load KLEMS shares and capital stock/labor levels For certain very small sectors in small countries, we may be missing capital and labor. 
#     For output=0 country-sectors, we're missing shares.
#############################################################################

klems_shares<-fread(paste0(path_output,"klems_shares.csv"))

#need to add id var
klems_shares<-merge(klems_shares, industries.cprs$industry.cntry.tab[,.(country, sector_id,id)],by=c("country","sector_id"))

klems_shares[,IIE_share:=II_share*energy_share_II.12]
klems_shares[,IINE_share:=II_share*(1-energy_share_II.12)]



sum.E_EM<-fread(paste0(path_input,"sum.E_EM.ctry.csv"))
sum.EM_Y<-fread(paste0(path_input,"sum.EM_Y.ctry.csv"))
sum.KL_Y<-fread(paste0(path_input,"sum.KL_Y.ctry.csv"))
sum.E_Y<-fread(paste0(path_input,"sum.E_Y.ctry.csv"))

xwalk.eos_sectors<-fread(paste0(path_input,"EOS_supersectors.csv"))

sum.KL_Y
sum.EM_Y
sum.E_EM
sum.E_Y


#############################################################################
# 1) Merge KLEMS shares and do initial calculations without NiGEM deltas. 
#       also with keeping capital and labor together.
#############################################################################

dt.all.out<-merge(dt.all, klems_shares[,.(id, labor_share, cap_share, IIE_share, IINE_share, II_share,
                                          IIM.12,IIM_P.12,    IIE.12, IIE_P.12, IIE_QI.12, IIM_QI.12  )] ,by="id")


dt.all.out[,Omega.2020 := gross_output.2020^(1-II_share)*series.2020^(II_share)]
dt.all.out[,series.out1 :=Omega.2020 * (series*gross_output.2020)^II_share]

##summarize

dt.summary.output1<-summarystats(dt.all.out,"series.out1","current","netzero", 2050,2050)

mycountry<-"US"
graph_sector_iam.2(dt.summary.output1[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050","netzero.ov.current.output_noint.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/method1_" ),N,set_sectors.IO,1)





#############################################################################
# 2) - (2) from prior version confirmed that splitting out K and L doesn't change anything (so worth keep all in Value Added form. )
#         prior version exercise also confirms that operating through energy and assuming E-M complementarity 
#                 is like going through an EM aggregate.
#       in this, we attach KLEMS shares and elasticities. 
#     we need to assume E and M are complements due to lack of EQ reaction again. So use E_Y elasticity. 
#
#
#       will present this as positing that energy = II and a more simple structure. 
#
#############################################################################

dt.all.out<-merge(dt.all, klems_shares[,.(id, labor_share, cap_share,
                                          IIE_share, IINE_share, II_share,Kq_GFCF ,   H_EMP, VA_CP, VA_Q,GO_CP, GO_Q,II_Q, II_CP,
                                          IIM.12,IIM_P.12   , IIE.12 ,IIE_P.12, IIE_QI.12 ,IIM_QI.12 )] ,by="id")
dt.all.out[,Kq_GFCF.2020:=Kq_GFCF[year==2020],.(model.short, scenario.short, id,country,sector)]
dt.all.out[,H_EMP.2020:=H_EMP[year==2020],.(model.short, scenario.short, id,country,sector)]

#Now merge elasticities from klems_estimation_v3.R (loaded at the top)


#################
#run together block
dt.all.eos<-merge(dt.all.out, xwalk.eos_sectors, by=c("sector"))
# minimum is that they are perfect complements
min_EOS<-.1

dt.all.eos<-merge(dt.all.eos,sum.E_EM[,.(sigma_E_EM.mean=ifelse(mean<min_EOS,min_EOS,mean),
                                         sigma_E_EM.p25=ifelse(p25<min_EOS,min_EOS,p25),
                                         sigma_E_EM.p75=ifelse(p75<min_EOS,min_EOS,p75),
                                         sector.eos
                                         )],  by="sector.eos")

dt.all.eos<-merge(dt.all.eos,sum.EM_Y[,.(sigma_EM_Y.mean=ifelse(mean<min_EOS,min_EOS,mean),
                                         sigma_EM_Y.p25=ifelse(p25<min_EOS,min_EOS,p25),
                                         sigma_EM_Y.p75=ifelse(p75<min_EOS,min_EOS,p75),
                                         sector.eos
)],  by="sector.eos")

dt.all.eos<-merge(dt.all.eos,sum.KL_Y[,.(sigma_KL_Y.mean=ifelse(mean<min_EOS,min_EOS,mean),
                                         sigma_KL_Y.p25=ifelse(p25<min_EOS,min_EOS,p25),
                                         sigma_KL_Y.p75=ifelse(p75<min_EOS,min_EOS,p75),
                                         sector.eos
)],  by="sector.eos")
dt.all.eos<-merge(dt.all.eos,sum.E_Y[,.(sigma_E_Y.mean=ifelse(mean<min_EOS,min_EOS,mean),
                                         sigma_E_Y.p25=ifelse(p25<min_EOS,min_EOS,p25),
                                         sigma_E_Y.p75=ifelse(p75<min_EOS,min_EOS,p75),
                                         sector.eos
)],  by="sector.eos")
###################


dt.all.eos[,rho_E_EM.mean:=(sigma_E_EM.mean-1)/(sigma_E_EM.mean)]
dt.all.eos[,rho_EM_Y.mean:=(sigma_EM_Y.mean-1)/(sigma_EM_Y.mean)]

dt.all.eos[,rho_KL_Y.mean:=(sigma_KL_Y.mean-1)/(sigma_KL_Y.mean)]

#I think this doesn't matter
dt.all.eos[,rho_K_KL.mean:=(1)]

dt.all.eos[,KL.2020:=((cap_share/(cap_share+labor_share)*Kq_GFCF.2020)^rho_K_KL.mean+(labor_share/(cap_share+labor_share)*H_EMP.2020)^rho_K_KL.mean)^(1/rho_K_KL.mean)]

# these are basically just normalizations for the level. 

dt.all.eos[,KL:=(cap_share/(cap_share+labor_share)*Kq_GFCF^rho_K_KL.mean+labor_share/(cap_share+labor_share)*H_EMP^rho_K_KL.mean)^(1/rho_K_KL.mean)]


dt.all.eos[,rho_E_Y.mean:=(sigma_EM_Y.mean-1)/(sigma_EM_Y.mean)]
dt.all.eos[,rho_E_Y.p25:=(sigma_EM_Y.p25-1)/(sigma_EM_Y.p25)]
dt.all.eos[,rho_E_Y.p75:=(sigma_EM_Y.p75-1)/(sigma_EM_Y.p75)]

# dt.all.eos[,rho_E_Y.mean:=(sigma_E_Y.mean-1)/(sigma_E_Y.mean)]
# dt.all.eos[,rho_E_Y.p25:=(sigma_E_Y.p25-1)/(sigma_E_Y.p25)]
# dt.all.eos[,rho_E_Y.p75:=(sigma_E_Y.p75-1)/(sigma_E_Y.p75)]

dt.all.eos[,gross_output.2020.nm.1:=gross_output.2020/sum(gross_output.2020),.(year,scenario.short,country)]

dt.all.eos[,A.2020.mean := gross_output.2020/((cap_share+labor_share)*KL.2020^rho_E_Y.mean+
                                               II_share*series.2020^rho_E_Y.mean)^(1/rho_E_Y.mean)]
dt.all.eos[,A.2020.p25 := gross_output.2020/((cap_share+labor_share)*KL.2020^rho_E_Y.p25+
                                                     II_share*series.2020^rho_E_Y.p25)^(1/rho_E_Y.p25)]
dt.all.eos[,A.2020.p75 := gross_output.2020/((cap_share+labor_share)*KL.2020^rho_E_Y.p75+
                                                    II_share*series.2020^rho_E_Y.p75)^(1/rho_E_Y.p75)]

# 
# dt.all.eos[,series.out.2.mean :=A.2020.mean * ((cap_share+labor_share)*KL.2020^rho_E_Y.mean+II_share*(series)^rho_E_Y.mean)^(1/rho_E_Y.mean)]
# dt.all.eos[,series.out.2.p25 :=A.2020.p25* ((cap_share+labor_share)*KL.2020^rho_E_Y.p25+II_share*(series)^rho_E_Y.p25)^(1/rho_E_Y.p25)]
# dt.all.eos[,series.out.2.p75 :=A.2020.p75 * ((cap_share+labor_share)*KL.2020^rho_E_Y.p75+II_share*(series)^rho_E_Y.p75)^(1/rho_E_Y.p75)]


dt.all.eos[,value_added_comp.mean:=(cap_share+labor_share)*KL.2020^rho_E_Y.mean]
dt.all.eos[,value_added_comp.p25:=(cap_share+labor_share)*KL.2020^rho_E_Y.p25]
dt.all.eos[,value_added_comp.p75:=(cap_share+labor_share)*KL.2020^rho_E_Y.p75]


dt.all.eos[,series.out.2.mean :=A.2020.mean* ((cap_share+labor_share)*KL.2020^rho_E_Y.mean+II_share*(series)^rho_E_Y.mean)^(1/rho_E_Y.mean)]
dt.all.eos[,series.out.2.p25 :=A.2020.p25*((cap_share+labor_share)*KL.2020^rho_E_Y.p25+II_share*(series)^rho_E_Y.p25)^(1/rho_E_Y.p25)]
dt.all.eos[,series.out.2.p75 := A.2020.p75*((cap_share+labor_share)*KL.2020^rho_E_Y.p75+II_share*(series)^rho_E_Y.p75)^(1/rho_E_Y.p75)]

#I think it's so invariant due to the fact that they're so close to complements, KL aggregate is so small. 

test1<-dt.all.eos[sector=="3-energy-intensive|non-fossil mining"&country=="US"&model.short=="message"&scenario.short=="netzero",
                  .(year, sigma_E_Y.mean,sigma_E_Y.p25,sigma_E_Y.p75,series.out.2.mean ,value_added_comp.mean,value_added_comp.p25,value_added_comp.p75 ,series.out.2.p25,series.out.2.p75)]

test2<-dt.all.eos[sector=="3-energy-intensive|non-fossil mining"&country=="US"&model.short=="message"&scenario.short=="current",
                  .(year, sigma_E_Y.mean,sigma_E_Y.p25,sigma_E_Y.p75,
                    series.out.2.mean, series.out.2.p25, series.out.2.p75 ,value_added_comp.mean,value_added_comp.p25,value_added_comp.p75)]

dt1<-merge(test1,test2,by="year", suffix = c(".nz",".base"))

dt1[,nz_over_base.mean:=series.out.2.mean.nz/series.out.2.mean.base]
dt1[,nz_over_base.p25:=series.out.2.p25.nz/series.out.2.p25.base]
dt1[,nz_over_base.p75:=series.out.2.p75.nz/series.out.2.p75.base]

dt1



dt.summary.output2.mean<-summarystats(dt.all.eos,"series.out.2.mean","current","netzero", 2050,2050)


mycountry<-"US"
graph_sector_iam.2(dt.summary.output2.mean[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/method2.mean_" ),N,gsub("\\|","\\\\|",set_sectors.IO),1)


#### OK now do p25 and p75. 

dt.summary.output2.p25<-summarystats(dt.all.eos,"series.out.2.p25","current","netzero", 2050,2050)
mycountry<-"US"
graph_sector_iam.2(dt.summary.output2.p25[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/method2.p25_" ),N,gsub("\\|","\\\\|",set_sectors.IO),0)

#### OK now do p25 and p75. 

dt.summary.output2.p75<-summarystats(dt.all.eos,"series.out.2.p75","current","netzero", 2050,2050)
mycountry<-"US"
graph_sector_iam.2(dt.summary.output2.p75[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/method2.p75_" ),N,gsub("\\|","\\\\|",set_sectors.IO),0)





dt.summary.outputsidebyside1_2<-rbind( 
  dt.summary.output1[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="energy_only",id ,country,sector ,my.series.2.over.1)],
  dt.summary.output2.mean[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="mean",id ,country,sector ,my.series.2.over.1)],
  dt.summary.output2.p25[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="p25",id ,country,sector ,my.series.2.over.1)],
  dt.summary.output2.p75[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="p75",id ,country,sector ,my.series.2.over.1)])

#dt.summary.output2.p25[country=="US"&model.short=="message"&!is.na(my.series.2.over.1),.(model.short="EUklems_EOS.p25",id ,country,sector ,my.series.2.over.1)]


mycountry<-"US"
graph_sector_iam.2(dt.summary.outputsidebyside1_2[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050,\n(KL)E Structure, EOS Sensitivity","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/sidebyside_KL_E" ),N,set_sectors.IO,1)

ggsave(paste0(path_graphs,""))




#############################################################################
# 3) the lack of sensitivity is due to the fact that the factors are near complements in the outer nest. 
#
# so using the full nesting structure could actually provide better sensitivity. 
#
#     caveats - lots of fill ins for IIE and IIM, most of them are just total US values. 
#
#############################################################################


dt.all.eos[,rho_E_EM.mean:=(sigma_E_EM.mean-1)/(sigma_E_EM.mean)]
dt.all.eos[,rho_E_EM.p25:=(sigma_E_EM.p25-1)/(sigma_E_EM.p25)]
dt.all.eos[,rho_E_EM.p75:=(sigma_E_EM.p75-1)/(sigma_E_EM.p75)]

dt.all.eos[,rho_EM_Y.mean:=(sigma_EM_Y.mean-1)/(sigma_EM_Y.mean)]
dt.all.eos[,rho_EM_Y.p25:=(sigma_EM_Y.p25-1)/(sigma_EM_Y.p25)]
dt.all.eos[,rho_EM_Y.p75:=(sigma_EM_Y.p75-1)/(sigma_EM_Y.p75)]

dt.all.eos[,rho_KL_Y.mean:=(sigma_KL_Y.mean-1)/(sigma_KL_Y.mean)]
dt.all.eos[,rho_KL_Y.p25:=(sigma_KL_Y.p25-1)/(sigma_KL_Y.p25)]
dt.all.eos[,rho_KL_Y.p75:=(sigma_KL_Y.p75-1)/(sigma_KL_Y.p75)]

#I think this doesn't matter
dt.all.eos[,rho_K_KL.mean:=(1)]


#this is not exactly right since materials may be more expensive than energy, but we don't have those price series. 
dt.all.eos[,IINE_placeholder:=IIM.12]
#renormalize IIE according to IIE.12



dt.all.eos[,EM.1.mean:=((1-IIE_share)/II_share*IINE_placeholder^rho_E_EM.mean+IIE_share/II_share*(series*IIE.12/series.2020)^rho_E_EM.mean)^(1/rho_E_EM.mean)]
dt.all.eos[,EM.1.p25:=((1-IIE_share)/II_share*IINE_placeholder^rho_E_EM.p25+IIE_share/II_share*(series*IIE.12/series.2020)^rho_E_EM.p25)^(1/rho_E_EM.p25)]
dt.all.eos[,EM.1.p75:=((1-IIE_share)/II_share*IINE_placeholder^rho_E_EM.p75+IIE_share/II_share*(series*IIE.12/series.2020)^rho_E_EM.p75)^(1/rho_E_EM.p75)]

dt.all.eos[,KL.1:=(cap_share/(cap_share+labor_share)*Kq_GFCF^rho_K_KL.mean+labor_share/(cap_share+labor_share)*H_EMP^rho_K_KL.mean)^(1/rho_K_KL.mean)]

dt.all.eos[,EM.1.mean.2020:=EM.1.mean[year==2020],.(model.short, scenario.short, id,country,sector)]
dt.all.eos[,EM.1.p25.2020:=EM.1.p25[year==2020],.(model.short, scenario.short, id,country,sector)]
dt.all.eos[,EM.1.p75.2020:=EM.1.p75[year==2020],.(model.short, scenario.short, id,country,sector)]

dt.all.eos[,II_Q.2020:=II_Q[year==2020],.(model.short, scenario.short, id,country,sector)]

dt.all.eos[,KL.1.2020:=KL.1[year==2020],.(model.short, scenario.short, id,country,sector)]
dt.all.eos[,VA_Q.2020:=VA_Q[year==2020],.(model.short, scenario.short, id,country,sector)]




#then renormalize according to chain linked volumes. 
dt.all.eos[,A_EM_norm.mean:=II_Q.2020/EM.1.mean.2020]
dt.all.eos[,EM.mean:=A_EM_norm.mean*EM.1.mean]
dt.all.eos[,A_EM_norm.p25:=II_Q.2020/EM.1.p25.2020]
dt.all.eos[,EM.p25:=A_EM_norm.p25*EM.1.p25]
dt.all.eos[,A_EM_norm.p75:=II_Q.2020/EM.1.p75.2020]
dt.all.eos[,EM.p75:=A_EM_norm.p75*EM.1.p75]

dt.all.eos[,A_KL_norm:=VA_Q.2020/KL.1.2020]
dt.all.eos[,KL:=A_KL_norm*KL.1]

dt.all.eos[,series.nested.mean:=((1-II_share)*KL^rho_EM_Y.mean + (II_share)*EM.mean^rho_EM_Y.mean)^(1/rho_EM_Y.mean)]
 # dt.all.eos[,series.nested.p25:=((1-II_share)*KL^rho_EM_Y.mean + (II_share)*EM.p25^rho_EM_Y.mean)^(1/rho_EM_Y.mean)]
 # dt.all.eos[,series.nested.p75:=((1-II_share)*KL^rho_EM_Y.mean + (II_share)*EM.p75^rho_EM_Y.mean)^(1/rho_EM_Y.mean)]

#choice whether to do high side for ALL nests? 
# dt.all.eos[,series.nested.p25:=((1-II_share)*KL^rho_EM_Y.p25 + (II_share)*EM.p25^rho_EM_Y.p25)^(1/rho_EM_Y.p25)]
dt.all.eos[,series.nested.p75:=((1-II_share)*KL^rho_EM_Y.p75 + (II_share)*EM.p75^rho_EM_Y.p75)^(1/rho_EM_Y.p75)]
 dt.all.eos[,series.nested.p25:=((1-II_share)*KL^rho_KL_Y.p25 + (II_share)*EM.p25^rho_KL_Y.p25)^(1/rho_KL_Y.p25)]
 #dt.all.eos[,series.nested.p75:=((1-II_share)*KL^rho_KL_Y.p75 + (II_share)*EM.p75^rho_KL_Y.mean)^(1/rho_KL_Y.p75)]
# 


dt.summary.output.nested.mean<-summarystats(dt.all.eos,"series.nested.mean","current","netzero", 2050,2050)
dt.summary.output.nested.p25<-summarystats(dt.all.eos,"series.nested.p25","current","netzero", 2050,2050)
dt.summary.output.nested.p75<-summarystats(dt.all.eos,"series.nested.p75","current","netzero", 2050,2050)

dt.summary.output.nested.p75[country=="US"&is.na(my.series.2)]

mycountry<-"US"
graph_sector_iam.2(dt.summary.output.nested.mean[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/nest.mean_" ),N,gsub("\\|","\\\\|",set_sectors.IO),0)


mycountry<-"US"
graph_sector_iam.2(dt.summary.output.nested.p25[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/nest.p25_" ),N,gsub("\\|","\\\\|",set_sectors.IO),0)

mycountry<-"US"
graph_sector_iam.2(dt.summary.output.nested.p75[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/nest.p75_" ),N,gsub("\\|","\\\\|",set_sectors.IO),0)


sum.E_EM
####SO KL_Y is more rigid it seems, and thus amplifies. 


dt.summary.outputsidebyside_nested<-rbind( 
  dt.summary.output1[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="energy_only",id ,country,sector ,my.series.2.over.1)],
  dt.summary.output.nested.mean[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="mean",id ,country,sector ,my.series.2.over.1)],
  dt.summary.output.nested.p25[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="p25",id ,country,sector ,my.series.2.over.1)],
  dt.summary.output.nested.p75[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="p75",id ,country,sector ,my.series.2.over.1)])


mycountry<-"US"
graph_sector_iam.2(dt.summary.outputsidebyside_nested[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050,\n(KL)EM Structure, EOS Sensitivity","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/sidebyside_KL_E_M" ),N,set_sectors.IO,1)





#############################################################################
# 3) the lack of sensitivity is due to the fact that the factors are near complements in the outer nest. 
#
# so using the full nesting structure could actually provide better sensitivity. 
#
#     caveats - lots of fill ins for IIE and IIM, most of them are just total US values. 
#
#############################################################################

dt.summary.outputsidebyside_nested<-rbind( 
  dt.summary.output2.mean[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="(KL)E",id ,country,sector ,my.series.2.over.1)],
  dt.summary.output.nested.mean[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="(KL)EM mn",id ,country,sector ,my.series.2.over.1)],
  dt.summary.output.nested.p25[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="(KL)EM p25",id ,country,sector ,my.series.2.over.1)],
  dt.summary.output.nested.p75[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="(KL)EM p75",id ,country,sector ,my.series.2.over.1)])



mycountry<-"US"
graph_sector_iam.2(dt.summary.outputsidebyside_nested[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050,\n (KL)E vs. (KL)EM, EOS Sensitivity","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/sidebyside_structdiff" ),N,set_sectors.IO,1)



