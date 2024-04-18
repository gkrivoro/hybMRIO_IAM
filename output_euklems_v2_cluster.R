####################################################################
#   [Update 3/15/24] 
#     include energy intensity changes from the IAM, as well as do clustered SD from EU KLEMS
#
#
#   February update: Now, we include the EU-KLEMS elasticity estimates, 
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


length(unique(dt.all[,sector]))
#############################################################################
# Load KLEMS shares and capital stock/labor levels For certain very small sectors in small countries, we may be missing capital and labor. 
#     For output=0 country-sectors, we're missing shares.
#############################################################################

klems_shares<-fread(paste0(path_output,"klems_shares.csv"))

#need to add id var
klems_shares<-merge(klems_shares, industries.cprs$industry.cntry.tab[,.(country, sector_id,id)],by=c("country","sector_id"))

klems_shares[,IIE_share:=II_share*energy_share_II.12]
klems_shares[,IINE_share:=II_share*(1-energy_share_II.12)]


# 
# sum.E_EM<-fread(paste0(path_input,"sum.E_EM.ctry.csv"))
# sum.EM_Y<-fread(paste0(path_input,"sum.EM_Y.ctry.csv"))
# #sum.KL_Y<-fread(paste0(path_input,"sum.KL_Y.ctry.csv"))
# sum.E_Y<-fread(paste0(path_input,"sum.E_Y.ctry.csv"))
# 

sum.E_EM<-fread(paste0(path_input,"sum.E_EM.cluster.csv"))
sum.EM_Y<-fread(paste0(path_input,"sum.EM_Y.cluster.csv"))
#fwrite(sum.KL_Y,paste0(path_input,"sum.KL_Y.ctry.csv"))
sum.E_Y<-fread(paste0(path_input,"sum.E_Y.cluster.csv"))


#sum.E_EM




xwalk.eos_sectors<-fread(paste0(path_input,"EOS_supersectors.csv"))


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

dt.all.eos<-merge(dt.all.eos,sum.E_EM[,.(sigma_E_EM.mean=ifelse(EOS<min_EOS,min_EOS,EOS),
                                         sigma_E_EM.low=ifelse(EOS_low95conf<min_EOS,min_EOS,EOS_low95conf),
                                         sigma_E_EM.high=ifelse(EOS_high95conf<min_EOS,min_EOS,EOS_high95conf),
                                         sector.eos
)],  by="sector.eos")

dt.all.eos<-merge(dt.all.eos,sum.EM_Y[,.(sigma_EM_Y.mean=ifelse(EOS<min_EOS,min_EOS,EOS),
                                         sigma_EM_Y.low=ifelse(EOS_low95conf<min_EOS,min_EOS,EOS_low95conf),
                                         sigma_EM_Y.high=ifelse(EOS_high95conf<min_EOS,min_EOS,EOS_high95conf),
                                         sector.eos
)],  by="sector.eos")

# dt.all.eos<-merge(dt.all.eos,sum.KL_Y[,.(sigma_KL_Y.mean=ifelse(EOS<min_EOS,min_EOS,EOS),
#                                          sigma_KL_Y.EOS_low95conf=ifelse(EOS_low95conf<min_EOS,min_EOS,EOS_low95conf),
#                                          sigma_KL_Y.high=ifelse(p75<min_EOS,min_EOS,p75),
#                                          sector.eos
# )],  by="sector.eos")
dt.all.eos<-merge(dt.all.eos,sum.E_Y[,.(sigma_E_Y.mean=ifelse(EOS<min_EOS,min_EOS,EOS),
                                        sigma_E_Y.low=ifelse(EOS_low95conf<min_EOS,min_EOS,EOS_low95conf),
                                        sigma_E_Y.high=ifelse(EOS_high95conf<min_EOS,min_EOS,EOS_high95conf),
                                        sector.eos
)],  by="sector.eos")
###################


dt.all.eos[,rho_E_EM.mean:=(sigma_E_EM.mean-1)/(sigma_E_EM.mean)]
dt.all.eos[,rho_EM_Y.mean:=(sigma_EM_Y.mean-1)/(sigma_EM_Y.mean)]

#dt.all.eos[,rho_KL_Y.mean:=(sigma_KL_Y.mean-1)/(sigma_KL_Y.mean)]

#I think this doesn't matter
dt.all.eos[,rho_K_KL.mean:=(1)]

dt.all.eos[,KL.2020:=((cap_share/(cap_share+labor_share)*Kq_GFCF.2020)^rho_K_KL.mean+(labor_share/(cap_share+labor_share)*H_EMP.2020)^rho_K_KL.mean)^(1/rho_K_KL.mean)]

# these are basically just normalizations for the level. 

dt.all.eos[,KL:=(cap_share/(cap_share+labor_share)*Kq_GFCF^rho_K_KL.mean+labor_share/(cap_share+labor_share)*H_EMP^rho_K_KL.mean)^(1/rho_K_KL.mean)]


dt.all.eos[,rho_E_Y.mean:=(sigma_EM_Y.mean-1)/(sigma_EM_Y.mean)]
dt.all.eos[,rho_E_Y.low:=(sigma_EM_Y.low-1)/(sigma_EM_Y.low)]
dt.all.eos[,rho_E_Y.high:=(sigma_EM_Y.high-1)/(sigma_EM_Y.high)]

# dt.all.eos[,rho_E_Y.mean:=(sigma_E_Y.mean-1)/(sigma_E_Y.mean)]
# dt.all.eos[,rho_E_Y.low:=(sigma_E_Y.low-1)/(sigma_E_Y.low)]
# dt.all.eos[,rho_E_Y.high:=(sigma_E_Y.high-1)/(sigma_E_Y.high)]

dt.all.eos[,gross_output.2020.nm.1:=gross_output.2020/sum(gross_output.2020),.(year,scenario.short,country)]

dt.all.eos[,A.2020.mean := gross_output.2020/((cap_share+labor_share)*KL.2020^rho_E_Y.mean+
                                                II_share*series.2020^rho_E_Y.mean)^(1/rho_E_Y.mean)]
dt.all.eos[,A.2020.low := gross_output.2020/((cap_share+labor_share)*KL.2020^rho_E_Y.low+
                                               II_share*series.2020^rho_E_Y.low)^(1/rho_E_Y.low)]
dt.all.eos[,A.2020.high := gross_output.2020/((cap_share+labor_share)*KL.2020^rho_E_Y.high+
                                               II_share*series.2020^rho_E_Y.high)^(1/rho_E_Y.high)]

# 
# dt.all.eos[,series.out.2.mean :=A.2020.mean * ((cap_share+labor_share)*KL.2020^rho_E_Y.mean+II_share*(series)^rho_E_Y.mean)^(1/rho_E_Y.mean)]
# dt.all.eos[,series.out.2.low :=A.2020.low* ((cap_share+labor_share)*KL.2020^rho_E_Y.low+II_share*(series)^rho_E_Y.low)^(1/rho_E_Y.low)]
# dt.all.eos[,series.out.2.high :=A.2020.high * ((cap_share+labor_share)*KL.2020^rho_E_Y.high+II_share*(series)^rho_E_Y.high)^(1/rho_E_Y.high)]


dt.all.eos[,value_added_comp.mean:=(cap_share+labor_share)*KL.2020^rho_E_Y.mean]
dt.all.eos[,value_added_comp.low:=(cap_share+labor_share)*KL.2020^rho_E_Y.low]
dt.all.eos[,value_added_comp.high:=(cap_share+labor_share)*KL.2020^rho_E_Y.high]


dt.all.eos[,series.out.2.mean :=A.2020.mean* ((cap_share+labor_share)*KL.2020^rho_E_Y.mean+II_share*(series)^rho_E_Y.mean)^(1/rho_E_Y.mean)]
dt.all.eos[,series.out.2.low :=A.2020.low*((cap_share+labor_share)*KL.2020^rho_E_Y.low+II_share*(series)^rho_E_Y.low)^(1/rho_E_Y.low)]
dt.all.eos[,series.out.2.high := A.2020.high*((cap_share+labor_share)*KL.2020^rho_E_Y.high+II_share*(series)^rho_E_Y.high)^(1/rho_E_Y.high)]

#I think it's so invariant due to the fact that they're so close to complements, KL aggregate is so small. 
# 
# test1<-dt.all.eos[sector=="3-energy-intensive|non-fossil mining"&country=="US"&model.short=="message"&scenario.short=="netzero",
#                   .(year, sigma_E_Y.mean,sigma_E_Y.low,sigma_E_Y.high,series.out.2.mean ,value_added_comp.mean,value_added_comp.low,value_added_comp.high ,series.out.2.low,series.out.2.high)]
# 
# test2<-dt.all.eos[sector=="3-energy-intensive|non-fossil mining"&country=="US"&model.short=="message"&scenario.short=="current",
#                   .(year, sigma_E_Y.mean,sigma_E_Y.low,sigma_E_Y.high,
#                     series.out.2.mean, series.out.2.low, series.out.2.high ,value_added_comp.mean,value_added_comp.low,value_added_comp.high)]
# 
# dt1<-merge(test1,test2,by="year", suffix = c(".nz",".base"))
# 
# dt1[,nz_over_base.mean:=series.out.2.mean.nz/series.out.2.mean.base]
# dt1[,nz_over_base.low:=series.out.2.low.nz/series.out.2.low.base]
# dt1[,nz_over_base.high:=series.out.2.high.nz/series.out.2.high.base]
# 
# dt1



dt.summary.output2.mean<-summarystats(dt.all.eos,"series.out.2.mean","current","netzero", 2050,2050)


mycountry<-"US"
graph_sector_iam.2(dt.summary.output2.mean[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/method2.mean_" ),N,gsub("\\|","\\\\|",set_sectors.IO),1)


#### OK now do p25 and p75. 

dt.summary.output2.low<-summarystats(dt.all.eos,"series.out.2.low","current","netzero", 2050,2050)
mycountry<-"US"
graph_sector_iam.2(dt.summary.output2.low[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/method2.low_" ),N,gsub("\\|","\\\\|",set_sectors.IO),1)

#### OK now do p25 and p75. 

dt.summary.output2.high<-summarystats(dt.all.eos,"series.out.2.high","current","netzero", 2050,2050)
mycountry<-"US"
graph_sector_iam.2(dt.summary.output2.high[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/method2.high_" ),N,gsub("\\|","\\\\|",set_sectors.IO),1)





dt.summary.outputsidebyside1_2<-rbind( 
  dt.summary.output1[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="Energy Only",id ,country,sector ,my.series.2.over.1)],
  dt.summary.output2.mean[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="(KL)E",id ,country,sector ,my.series.2.over.1)])

#dt.summary.output2.low[country=="US"&model.short=="message"&!is.na(my.series.2.over.1),.(model.short="EUklems_EOS.low",id ,country,sector ,my.series.2.over.1)]


mycountry<-"US"
graph_sector_iam.2(dt.summary.outputsidebyside1_2[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, MESSAGE, Net Zero vs. Current Scenario \n% Diff. in 2050,\n(KL)E Structure","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/sidebyside_KL_E_message" ),N,set_sectors.IO,0)





dt.summary.outputsidebyside1_2<-rbind( 
  dt.summary.output1[model.short=="remind"&!is.na(my.series.2.over.1),.(model.short="Energy Only",id ,country,sector ,my.series.2.over.1)],
  dt.summary.output2.mean[model.short=="remind"&!is.na(my.series.2.over.1),.(model.short="(KL)E",id ,country,sector ,my.series.2.over.1)])

#dt.summary.output2.low[country=="US"&model.short=="message"&!is.na(my.series.2.over.1),.(model.short="EUklems_EOS.low",id ,country,sector ,my.series.2.over.1)]


mycountry<-"US"
graph_sector_iam.2(dt.summary.outputsidebyside1_2[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, REMIND, Net Zero vs. Current Scenario \n% Diff. in 2050,\n(KL)E Structure","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/sidebyside_KL_E_remind" ),N,set_sectors.IO,0)



dt.summary.outputsidebyside1_2<-rbind( 
  dt.summary.output1[model.short=="gcam"&!is.na(my.series.2.over.1),.(model.short="Energy Only",id ,country,sector ,my.series.2.over.1)],
  dt.summary.output2.mean[model.short=="gcam"&!is.na(my.series.2.over.1),.(model.short="(KL)E",id ,country,sector ,my.series.2.over.1)])

#dt.summary.output2.low[country=="US"&model.short=="message"&!is.na(my.series.2.over.1),.(model.short="EUklems_EOS.low",id ,country,sector ,my.series.2.over.1)]


mycountry<-"US"
graph_sector_iam.2(dt.summary.outputsidebyside1_2[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, GCAM, Net Zero vs. Current Scenario \n% Diff. in 2050,\n(KL)E Structure","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/sidebyside_KL_E_gcam" ),N,set_sectors.IO,0)



#############################################################################
# 3) the lack of sensitivity is due to the fact that the factors are near complements in the outer nest. 
#
# so using the full nesting structure could actually provide better sensitivity. 
#
#     caveats - lots of fill ins for IIE and IIM, most of them are just total US values. 
#
#############################################################################




dt.all.eos[,rho_E_EM.mean:=(sigma_E_EM.mean-1)/(sigma_E_EM.mean)]
dt.all.eos[,rho_E_EM.low:=(sigma_E_EM.low-1)/(sigma_E_EM.low)]
dt.all.eos[,rho_E_EM.high:=(sigma_E_EM.high-1)/(sigma_E_EM.high)]

dt.all.eos[,rho_EM_Y.mean:=(sigma_EM_Y.mean-1)/(sigma_EM_Y.mean)]
dt.all.eos[,rho_EM_Y.low:=(sigma_EM_Y.low-1)/(sigma_EM_Y.low)]
dt.all.eos[,rho_EM_Y.high:=(sigma_EM_Y.high-1)/(sigma_EM_Y.high)]

# dt.all.eos[,rho_KL_Y.mean:=(sigma_KL_Y.mean-1)/(sigma_KL_Y.mean)]
# dt.all.eos[,rho_KL_Y.low:=(sigma_KL_Y.low-1)/(sigma_KL_Y.low)]
# dt.all.eos[,rho_KL_Y.high:=(sigma_KL_Y.high-1)/(sigma_KL_Y.high)]

#I think this doesn't matter
dt.all.eos[,rho_K_KL.mean:=(1)]


#this is not exactly right since materials may be more expensive than energy, but we don't have those price series. 
dt.all.eos[,IINE_placeholder:=IIM.12]
#renormalize IIE according to IIE.12



dt.all.eos[,EM.1.mean:=((1-IIE_share)/II_share*IINE_placeholder^rho_E_EM.mean+IIE_share/II_share*(series*IIE.12/series.2020)^rho_E_EM.mean)^(1/rho_E_EM.mean)]
dt.all.eos[,EM.1.low:=((1-IIE_share)/II_share*IINE_placeholder^rho_E_EM.low+IIE_share/II_share*(series*IIE.12/series.2020)^rho_E_EM.low)^(1/rho_E_EM.low)]
dt.all.eos[,EM.1.high:=((1-IIE_share)/II_share*IINE_placeholder^rho_E_EM.high+IIE_share/II_share*(series*IIE.12/series.2020)^rho_E_EM.high)^(1/rho_E_EM.high)]

dt.all.eos[,KL.1:=(cap_share/(cap_share+labor_share)*Kq_GFCF^rho_K_KL.mean+labor_share/(cap_share+labor_share)*H_EMP^rho_K_KL.mean)^(1/rho_K_KL.mean)]

dt.all.eos[,EM.1.mean.2020:=EM.1.mean[year==2020],.(model.short, scenario.short, id,country,sector)]
dt.all.eos[,EM.1.low.2020:=EM.1.low[year==2020],.(model.short, scenario.short, id,country,sector)]
dt.all.eos[,EM.1.high.2020:=EM.1.high[year==2020],.(model.short, scenario.short, id,country,sector)]

dt.all.eos[,II_Q.2020:=II_Q[year==2020],.(model.short, scenario.short, id,country,sector)]

dt.all.eos[,KL.1.2020:=KL.1[year==2020],.(model.short, scenario.short, id,country,sector)]
dt.all.eos[,VA_Q.2020:=VA_Q[year==2020],.(model.short, scenario.short, id,country,sector)]




#then renormalize according to chain linked volumes. 
dt.all.eos[,A_EM_norm.mean:=II_Q.2020/EM.1.mean.2020]
dt.all.eos[,EM.mean:=A_EM_norm.mean*EM.1.mean]
dt.all.eos[,A_EM_norm.low:=II_Q.2020/EM.1.low.2020]
dt.all.eos[,EM.low:=A_EM_norm.low*EM.1.low]
dt.all.eos[,A_EM_norm.high:=II_Q.2020/EM.1.high.2020]
dt.all.eos[,EM.high:=A_EM_norm.high*EM.1.high]

dt.all.eos[,A_KL_norm:=VA_Q.2020/KL.1.2020]
dt.all.eos[,KL:=A_KL_norm*KL.1]

dt.all.eos[,series.nested.mean:=((1-II_share)*KL^rho_EM_Y.mean + (II_share)*EM.mean^rho_EM_Y.mean)^(1/rho_EM_Y.mean)]
# dt.all.eos[,series.nested.low:=((1-II_share)*KL^rho_EM_Y.mean + (II_share)*EM.low^rho_EM_Y.mean)^(1/rho_EM_Y.mean)]
# dt.all.eos[,series.nested.high:=((1-II_share)*KL^rho_EM_Y.mean + (II_share)*EM.high^rho_EM_Y.mean)^(1/rho_EM_Y.mean)]

#choice whether to do high side for ALL nests? 
# dt.all.eos[,series.nested.low:=((1-II_share)*KL^rho_EM_Y.low + (II_share)*EM.low^rho_EM_Y.low)^(1/rho_EM_Y.low)]
dt.all.eos[,series.nested.high:=((1-II_share)*KL^rho_EM_Y.high + (II_share)*EM.high^rho_EM_Y.high)^(1/rho_EM_Y.high)]
dt.all.eos[,series.nested.low:=((1-II_share)*KL^rho_EM_Y.low + (II_share)*EM.low^rho_EM_Y.low)^(1/rho_EM_Y.low)]
#dt.all.eos[,series.nested.high:=((1-II_share)*KL^rho_KL_Y.high + (II_share)*EM.high^rho_KL_Y.mean)^(1/rho_KL_Y.high)]
# 


dt.summary.output.nested.mean<-summarystats(dt.all.eos,"series.nested.mean","current","netzero", 2050,2050)
dt.summary.output.nested.low<-summarystats(dt.all.eos,"series.nested.low","current","netzero", 2050,2050)
dt.summary.output.nested.high<-summarystats(dt.all.eos,"series.nested.high","current","netzero", 2050,2050)

dt.summary.output.nested.high[country=="US"&is.na(my.series.2)]

mycountry<-"US"
graph_sector_iam.2(dt.summary.output.nested.mean[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/nest.mean_" ),N,gsub("\\|","\\\\|",set_sectors.IO),0)


mycountry<-"US"
graph_sector_iam.2(dt.summary.output.nested.low[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/nest.low_" ),N,gsub("\\|","\\\\|",set_sectors.IO),0)

mycountry<-"US"
graph_sector_iam.2(dt.summary.output.nested.high[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/nest.high_" ),N,gsub("\\|","\\\\|",set_sectors.IO),0)


sum.E_EM
####SO KL_Y is more rigid it seems, and thus amplifies. 


dt.summary.outputsidebyside_nested<-rbind( 
  dt.summary.output1[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="Energy Only",id ,country,sector ,my.series.2.over.1)],
  dt.summary.output.nested.mean[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="(KL)(EM) mean",id ,country,sector ,my.series.2.over.1)],
  dt.summary.output.nested.low[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="(KL)(EM) p2.5",id ,country,sector ,my.series.2.over.1)],
  dt.summary.output.nested.high[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="(KL)(EM) p97.5",id ,country,sector ,my.series.2.over.1)])


mycountry<-"US"
graph_sector_iam.2(dt.summary.outputsidebyside_nested[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, MESSAGE Net Zero vs. Current Scenario\n% Diff. in 2050,\n(KL)(EM) Structure, EOS Sensitivity","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/sidebyside_KL_E_M" ),N,set_sectors.IO,0)





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
  dt.summary.output.nested.low[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="(KL)EM p2.5",id ,country,sector ,my.series.2.over.1)],
  dt.summary.output.nested.high[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="(KL)EM p97.5",id ,country,sector ,my.series.2.over.1)])



mycountry<-"US"
graph_sector_iam.2(dt.summary.outputsidebyside_nested[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050,\n (KL)E vs. (KL)EM, EOS Sensitivity","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/sidebyside_structdiff" ),N,set_sectors.IO,0)


ggplot(data = dt.all.eos[country=="US"&sector=="2-utility|electricity|renewable"&model.short=="gcam"]) + geom_line(aes(x = year,y = series.nested.mean,color=scenario.short))
ggplot(data = dt.all.eos[country=="US"&sector=="2-utility|electricity|renewable"&model.short=="gcam"]) + geom_line(aes(x = year,y = series,color=scenario.short))
ggplot(data = dt.all.eos[country=="US"&sector=="2-utility|electricity|renewable"&model.short=="gcam"]) + geom_line(aes(x = year,y = series.out.2.mean,color=scenario.short))
ggplot(data = dt.all.eos[country=="US"&sector=="2-utility|electricity|renewable"&model.short=="gcam"]) + geom_line(aes(x = year,y = EM.1.mean,color=scenario.short))



ggplot(data = dt.all.eos[country=="US"&sector=="3-energy-intensive|non-fossil mining"&model.short=="gcam"]) + geom_line(aes(x = year,y = series.nested.mean,color=scenario.short))
ggplot(data = dt.all.eos[country=="US"&sector=="3-energy-intensive|non-fossil mining"&model.short=="gcam"]) + geom_line(aes(x = year,y = series,color=scenario.short))
ggplot(data = dt.all.eos[country=="US"&sector=="3-energy-intensive|non-fossil mining"&model.short=="message"]) + geom_line(aes(x = year,y = series,color=scenario.short))
ggplot(data = dt.all.eos[country=="US"&sector=="3-energy-intensive|non-fossil mining"&model.short=="message"]) + geom_line(aes(x = year,y = series.nested.mean,color=scenario.short))

#############################################################################
# 3) Now introducing energy intensity variation
#
#############################################################################


dt.int1<-fread(paste0(path_output,"dt.enintensity.csv"))


dt.all.eos.int<-merge(dt.all.eos,dt.int1[sector=="all",.(Region, year,model.short, scenario.short, energy_intensity=value)],
                      by.x=c("region.iam","model.short","scenario.short","year"),by.y=c("Region","model.short" ,"scenario.short","year"))

dt.all.eos.int[,rho_K_KL.mean:=(1)]
dt.all.eos.int[,KL.2020:=((cap_share/(cap_share+labor_share)*Kq_GFCF.2020)^rho_K_KL.mean+(labor_share/(cap_share+labor_share)*H_EMP.2020)^rho_K_KL.mean)^(1/rho_K_KL.mean)]
dt.all.eos.int[,KL:=(cap_share/(cap_share+labor_share)*Kq_GFCF^rho_K_KL.mean+labor_share/(cap_share+labor_share)*H_EMP^rho_K_KL.mean)^(1/rho_K_KL.mean)]



dt.all.eos.int[, gamma_t.mean :=(((1/energy_intensity)^(rho_EM_Y.mean) - (1/series)^rho_EM_Y.mean*(KL/series)^rho_EM_Y.mean)/(series^(rho_EM_Y.mean-1)))^(1/rho_EM_Y.mean)]
dt.all.eos.int[, gamma_t.low :=(((1/energy_intensity)^(rho_EM_Y.low) - (1/series)^rho_EM_Y.low*(KL/series)^rho_EM_Y.mean)/(series^(rho_EM_Y.low-1)))^(1/rho_EM_Y.low)]
dt.all.eos.int[, gamma_t.high :=(((1/energy_intensity)^(rho_EM_Y.high) - (1/series)^rho_EM_Y.mean*(KL/series)^rho_EM_Y.high)/(series^(rho_EM_Y.high-1)))^(1/rho_EM_Y.high)]


#check it for the US
ggplot(data = dt.all.eos.int[country=="US"&sector=="3-energy-intensive"&model.short=="message"])+
  geom_line(aes(x =year, y=energy_intensity ,color=scenario.short))

ggplot(data = dt.all.eos.int[country=="US"&sector=="3-energy-intensive"&model.short=="message"])+
  geom_line(aes(x =year, y=gamma_t.mean ,color=scenario.short))+ geom_line(aes(x =year, y=gamma_t.low ,color=scenario.short))+ geom_line(aes(x =year, y=gamma_t.high ,color=scenario.short))

dt.all.eos.int[country=="US"&sector=="3-energy-intensive",.(year,gamma_t,model.short)]


dt.all.eos.int[,rho_E_Y.mean:=(sigma_EM_Y.mean-1)/(sigma_EM_Y.mean)]
dt.all.eos.int[,rho_E_Y.low:=(sigma_EM_Y.low-1)/(sigma_EM_Y.low)]
dt.all.eos.int[,rho_E_Y.high:=(sigma_EM_Y.high-1)/(sigma_EM_Y.high)]

# dt.all.eos[,rho_E_Y.mean:=(sigma_E_Y.mean-1)/(sigma_E_Y.mean)]
# dt.all.eos[,rho_E_Y.low:=(sigma_E_Y.low-1)/(sigma_E_Y.low)]
# dt.all.eos[,rho_E_Y.high:=(sigma_E_Y.high-1)/(sigma_E_Y.high)]

dt.all.eos.int[,gross_output.2020.nm.1:=gross_output.2020/sum(gross_output.2020),.(year,scenario.short,country)]

dt.all.eos.int[,A.2020.mean := gross_output.2020/((cap_share+labor_share)*KL.2020^rho_E_Y.mean+
                                                II_share*(gamma_t.mean*series.2020)^rho_E_Y.mean)^(1/rho_E_Y.mean)]
dt.all.eos.int[,A.2020.low := gross_output.2020/((cap_share+labor_share)*KL.2020^rho_E_Y.low+
                                               II_share*(gamma_t.low*series.2020)^rho_E_Y.low)^(1/rho_E_Y.low)]
dt.all.eos.int[,A.2020.high := gross_output.2020/((cap_share+labor_share)*KL.2020^rho_E_Y.high+
                                                II_share*(gamma_t.high*series.2020)^rho_E_Y.high)^(1/rho_E_Y.high)]

# 
# dt.all.eos[,series.out.2.mean :=A.2020.mean * ((cap_share+labor_share)*KL.2020^rho_E_Y.mean+II_share*(series)^rho_E_Y.mean)^(1/rho_E_Y.mean)]
# dt.all.eos[,series.out.2.low :=A.2020.low* ((cap_share+labor_share)*KL.2020^rho_E_Y.low+II_share*(series)^rho_E_Y.low)^(1/rho_E_Y.low)]
# dt.all.eos[,series.out.2.high :=A.2020.high * ((cap_share+labor_share)*KL.2020^rho_E_Y.high+II_share*(series)^rho_E_Y.high)^(1/rho_E_Y.high)]


dt.all.eos.int[,value_added_comp.mean:=(cap_share+labor_share)*KL.2020^rho_E_Y.mean]
dt.all.eos.int[,value_added_comp.low:=(cap_share+labor_share)*KL.2020^rho_E_Y.low]
dt.all.eos.int[,value_added_comp.high:=(cap_share+labor_share)*KL.2020^rho_E_Y.high]


dt.all.eos.int[,series.out.2.mean :=A.2020.mean* ((cap_share+labor_share)*KL.2020^rho_E_Y.mean+II_share*(series*gamma_t.mean)^rho_E_Y.mean)^(1/rho_E_Y.mean)]
dt.all.eos.int[,series.out.2.low :=A.2020.low*((cap_share+labor_share)*KL.2020^rho_E_Y.low+II_share*(series*gamma_t.low)^rho_E_Y.low)^(1/rho_E_Y.low)]
dt.all.eos.int[,series.out.2.high := A.2020.high*((cap_share+labor_share)*KL.2020^rho_E_Y.high+II_share*(series*gamma_t.high)^rho_E_Y.high)^(1/rho_E_Y.high)]

dt.summary.output2.mean.int<-summarystats(dt.all.eos.int,"series.out.2.mean","current","netzero", 2050,2050)


graph_sector_iam.2(dt.summary.output2.mean.int,country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"intensity/overview/" ),N,gsub("\\|","\\\\|",set_sectors.IO),0)


dt.summary.output2.mean


dt.summary.outputsidebyside1_2.int<-rbind( 
  dt.summary.output1[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="energy_only",id ,country,sector ,my.series.2.over.1)],
  dt.summary.output2.mean[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="mean",id ,country,sector ,my.series.2.over.1)],
  dt.summary.output2.mean.int[model.short=="message"&!is.na(my.series.2.over.1),.(model.short="mean.int",id ,country,sector ,my.series.2.over.1)])

#dt.summary.output2.low[country=="US"&model.short=="message"&!is.na(my.series.2.over.1),.(model.short="EUklems_EOS.low",id ,country,sector ,my.series.2.over.1)]


mycountry<-"US"
graph_sector_iam.2(dt.summary.outputsidebyside1_2.int[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050,\n(KL)E Structure, EOS Sensitivity","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/sidebyside_KL_E" ),N,set_sectors.IO,1)


###BASICALLY NO DIFFERENCE WHEN ADDING ENERGY INTENSITY. Let's try the other specification. 



#############################################################################
# 4) Now adding this same specification to the (KL)(EM) specification
#
#############################################################################




dt.all.eos.int[,rho_E_EM.mean:=(sigma_E_EM.mean-1)/(sigma_E_EM.mean)]
dt.all.eos.int[,rho_E_EM.low:=(sigma_E_EM.low-1)/(sigma_E_EM.low)]
dt.all.eos.int[,rho_E_EM.high:=(sigma_E_EM.high-1)/(sigma_E_EM.high)]

dt.all.eos.int[,rho_EM_Y.mean:=(sigma_EM_Y.mean-1)/(sigma_EM_Y.mean)]
dt.all.eos.int[,rho_EM_Y.low:=(sigma_EM_Y.low-1)/(sigma_EM_Y.low)]
dt.all.eos.int[,rho_EM_Y.high:=(sigma_EM_Y.high-1)/(sigma_EM_Y.high)]

# dt.all.eos[,rho_KL_Y.mean:=(sigma_KL_Y.mean-1)/(sigma_KL_Y.mean)]
# dt.all.eos[,rho_KL_Y.low:=(sigma_KL_Y.low-1)/(sigma_KL_Y.low)]
# dt.all.eos[,rho_KL_Y.high:=(sigma_KL_Y.high-1)/(sigma_KL_Y.high)]

#I think this doesn't matter
dt.all.eos.int[,rho_K_KL.mean:=(1)]


#this is not exactly right since materials may be more expensive than energy, but we don't have those price series. 
dt.all.eos.int[,IINE_placeholder:=IIM.12]
#renormalize IIE according to IIE.12



dt.all.eos.int[,EM.1.mean:=((1-IIE_share)/II_share*IINE_placeholder^rho_E_EM.mean+IIE_share/II_share*(series*IIE.12/series.2020*gamma_t.mean)^rho_E_EM.mean)^(1/rho_E_EM.mean)]
dt.all.eos.int[,EM.1.low:=((1-IIE_share)/II_share*IINE_placeholder^rho_E_EM.low+IIE_share/II_share*(series*IIE.12/series.2020*gamma_t.low)^rho_E_EM.low)^(1/rho_E_EM.low)]
dt.all.eos.int[,EM.1.high:=((1-IIE_share)/II_share*IINE_placeholder^rho_E_EM.high+IIE_share/II_share*(series*IIE.12/series.2020*gamma_t.high)^rho_E_EM.high)^(1/rho_E_EM.high)]

dt.all.eos.int[,KL.1:=(cap_share/(cap_share+labor_share)*Kq_GFCF^rho_K_KL.mean+labor_share/(cap_share+labor_share)*H_EMP^rho_K_KL.mean)^(1/rho_K_KL.mean)]

dt.all.eos.int[,EM.1.mean.2020:=EM.1.mean[year==2020],.(model.short, scenario.short, id,country,sector)]
dt.all.eos.int[,EM.1.low.2020:=EM.1.low[year==2020],.(model.short, scenario.short, id,country,sector)]
dt.all.eos.int[,EM.1.high.2020:=EM.1.high[year==2020],.(model.short, scenario.short, id,country,sector)]

dt.all.eos.int[,II_Q.2020:=II_Q[year==2020],.(model.short, scenario.short, id,country,sector)]

dt.all.eos.int[,KL.1.2020:=KL.1[year==2020],.(model.short, scenario.short, id,country,sector)]
dt.all.eos.int[,VA_Q.2020:=VA_Q[year==2020],.(model.short, scenario.short, id,country,sector)]




#then renormalize according to chain linked volumes. 
dt.all.eos.int[,A_EM_norm.mean:=II_Q.2020/EM.1.mean.2020]
dt.all.eos.int[,EM.mean:=A_EM_norm.mean*EM.1.mean]
dt.all.eos.int[,A_EM_norm.low:=II_Q.2020/EM.1.low.2020]
dt.all.eos.int[,EM.low:=A_EM_norm.low*EM.1.low]
dt.all.eos.int[,A_EM_norm.high:=II_Q.2020/EM.1.high.2020]
dt.all.eos.int[,EM.high:=A_EM_norm.high*EM.1.high]

dt.all.eos.int[,A_KL_norm:=VA_Q.2020/KL.1.2020]
dt.all.eos.int[,KL:=A_KL_norm*KL.1]

dt.all.eos.int[,series.nested.mean:=((1-II_share)*KL^rho_EM_Y.mean + (II_share)*EM.mean^rho_EM_Y.mean)^(1/rho_EM_Y.mean)]
# dt.all.eos[,series.nested.low:=((1-II_share)*KL^rho_EM_Y.mean + (II_share)*EM.low^rho_EM_Y.mean)^(1/rho_EM_Y.mean)]
# dt.all.eos[,series.nested.high:=((1-II_share)*KL^rho_EM_Y.mean + (II_share)*EM.high^rho_EM_Y.mean)^(1/rho_EM_Y.mean)]

#choice whether to do high side for ALL nests? 
# dt.all.eos[,series.nested.low:=((1-II_share)*KL^rho_EM_Y.low + (II_share)*EM.low^rho_EM_Y.low)^(1/rho_EM_Y.low)]
dt.all.eos.int[,series.nested.high:=((1-II_share)*KL^rho_EM_Y.high + (II_share)*EM.high^rho_EM_Y.high)^(1/rho_EM_Y.high)]
dt.all.eos.int[,series.nested.low:=((1-II_share)*KL^rho_EM_Y.low + (II_share)*EM.low^rho_EM_Y.low)^(1/rho_EM_Y.low)]
#dt.all.eos[,series.nested.high:=((1-II_share)*KL^rho_KL_Y.high + (II_share)*EM.high^rho_KL_Y.mean)^(1/rho_KL_Y.high)]
# 


dt.summary.output.nested.int.mean<-summarystats(dt.all.eos.int,"series.nested.mean","current","netzero", 2050,2050)
dt.summary.output.nested.int.low<-summarystats(dt.all.eos.int,"series.nested.low","current","netzero", 2050,2050)
dt.summary.output.nested.int.high<-summarystats(dt.all.eos.int,"series.nested.high","current","netzero", 2050,2050)

dt.summary.output.nested.int.high[country=="US"&is.na(my.series.2)]

mycountry<-"US"
graph_sector_iam.2(dt.summary.output.nested.int.mean[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/nest.int.mean_" ),N,gsub("\\|","\\\\|",set_sectors.IO),0)


mycountry<-"US"
graph_sector_iam.2(dt.summary.output.nested.low[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/nest.int.low_" ),N,gsub("\\|","\\\\|",set_sectors.IO),0)

mycountry<-"US"
graph_sector_iam.2(dt.summary.output.nested.high[!is.na(my.series.2.over.1)],country_iam_xwalk,model_xwalk,
                   mycountry,"Output, Net Zero vs. Current Scenario, % Diff. in 2050","netzero.ov.current.output.2050","my.series.2.over.1","% difference",
                   paste0(path_graphs,"/output/nest.int.high_" ),N,gsub("\\|","\\\\|",set_sectors.IO),0)




