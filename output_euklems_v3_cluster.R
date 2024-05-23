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
path_wd <- "C:/Users/boldrin/Sectoral Granularity/repo/" 
setwd(path_wd)


path_data <- "C:/Users/boldrin/Sectoral Granularity/repo/data/IO/EXIOBASE/IOT_2022_ixi/"
#path_data_IAM <-"~/data/macro/IAM_out/"

#run_id<-"2023-11-13_15-58"
run_id<-"2024-05-23_12-22"

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


#### Time series damages
list.ts.dt.summary.output <- list()

ts.years <- seq(2020,2100, by = 5)

for (i in seq_along(ts.years)){
  
  yr <- ts.years[i]
  
  temp.df.mean <- summarystats(dt.all.out,"series.out1","current","netzero", yr,yr)
  temp.df.mean <- temp.df.mean[,.(model.short, country,sector, year = yr, my.series.1, my.series.2, my.series.2.over.1)]

  list.ts.dt.summary.output[[i]] <- temp.df.mean

}

ts.dt.summary.output <- bind_rows(list.ts.dt.summary.output)


ts.dt.summary.output.e <- ts.dt.summary.output[,.(model.short, eos_type = "Energy Only", country,sector, year, my.series.1, my.series.2, my.series.2.over.1)]
                             

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

dt.all.out<-merge(dt.all.out[, .SD,.SDcols=c(colnames(dt.all),'series.out1')], klems_shares[,.(id, labor_share, cap_share,
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


#### Time series damages
list.ts.dt.summary.output.mean <- list()
list.ts.dt.summary.output.low <- list()
list.ts.dt.summary.output.high <- list()

ts.years <- seq(2020,2100, by = 5)

for (i in seq_along(ts.years)){
  
  yr <- ts.years[i]
  
  temp.df.mean <- summarystats(dt.all.eos,"series.out.2.mean","current","netzero", yr, yr)
  temp.df.mean <- temp.df.mean[,.(model.short, country,sector, year = yr, my.series.1, my.series.2, my.series.2.over.1)]
  
  temp.df.low <- summarystats(dt.all.eos,"series.out.2.low","current","netzero", yr, yr)
  temp.df.low <- temp.df.low[,.(model.short, country,sector, year = yr, my.series.1, my.series.2, my.series.2.over.1)]
  
  temp.df.high <- summarystats(dt.all.eos,"series.out.2.high","current","netzero", yr, yr)
  temp.df.high <- temp.df.high[,.(model.short, country,sector, year = yr, my.series.1, my.series.2, my.series.2.over.1)]
  
  
  list.ts.dt.summary.output.mean[[i]] <- temp.df.mean
  list.ts.dt.summary.output.low[[i]] <- temp.df.low
  list.ts.dt.summary.output.high[[i]] <- temp.df.high
  
  
  
}

ts.dt.summary.output.mean <- bind_rows(list.ts.dt.summary.output.mean)
ts.dt.summary.output.low <- bind_rows(list.ts.dt.summary.output.low)
ts.dt.summary.output.high <- bind_rows(list.ts.dt.summary.output.high)



ts.dt.summary.output.kle <- rbind(ts.dt.summary.output.mean[,.(model.short, eos_type = "(KL)E mean", country,sector, year, my.series.1, my.series.2, my.series.2.over.1)],
                              ts.dt.summary.output.low[,.(model.short, eos_type = "(KL)E p2.5", country,sector, year, my.series.1, my.series.2, my.series.2.over.1)],
                              ts.dt.summary.output.high[,.(model.short, eos_type = "(KL)E p97.5", country,sector, year, my.series.1, my.series.2, my.series.2.over.1)] )






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


list.ts.dt.summary.output.nested.mean <- list()
list.ts.dt.summary.output.nested.low <- list()
list.ts.dt.summary.output.nested.high <- list()
 
ts.years <- seq(2020,2100, by = 5)

for (i in seq_along(ts.years)){
  
  yr <- ts.years[i]
  
  temp.df.mean <- summarystats(dt.all.eos,"series.nested.mean","current","netzero", yr,yr)
  temp.df.mean <- temp.df.mean[,.(model.short, country,sector, year = yr, my.series.1, my.series.2, my.series.2.over.1)]
  
  temp.df.low <- summarystats(dt.all.eos,"series.nested.low","current","netzero", yr,yr)
  temp.df.low <- temp.df.low[,.(model.short, country,sector, year = yr, my.series.1, my.series.2, my.series.2.over.1)]
  
  temp.df.high <- summarystats(dt.all.eos,"series.nested.high","current","netzero", yr,yr)
  temp.df.high <- temp.df.high[,.(model.short, country,sector, year = yr, my.series.1, my.series.2, my.series.2.over.1)]
  
  
  list.ts.dt.summary.output.nested.mean[[i]] <- temp.df.mean
  list.ts.dt.summary.output.nested.low[[i]] <- temp.df.low
  list.ts.dt.summary.output.nested.high[[i]] <- temp.df.high
  

  
}

ts.dt.summary.output.nested.mean <- bind_rows(list.ts.dt.summary.output.nested.mean)
ts.dt.summary.output.nested.low <- bind_rows(list.ts.dt.summary.output.nested.low)
ts.dt.summary.output.nested.high <- bind_rows(list.ts.dt.summary.output.nested.high)



ts.dt.summary.output.klem <- rbind(ts.dt.summary.output.nested.mean[,.(model.short, eos_type = "(KL)(EM) mean", country,sector, year, my.series.1, my.series.2, my.series.2.over.1)],
      ts.dt.summary.output.nested.low[,.( model.short, eos_type = "(KL)(EM) p2.5", country,sector, year, my.series.1, my.series.2, my.series.2.over.1)],
      ts.dt.summary.output.nested.high[,.(model.short, eos_type = "(KL)(EM) p97.5", country,sector, year, my.series.1, my.series.2, my.series.2.over.1)] )



ts.dt.summary.output.final <- rbind(ts.dt.summary.output.e, ts.dt.summary.output.kle, ts.dt.summary.output.klem)



# 
# ts.dt.summary.output.final %>% arrange(model.short, country,sector, year) %>% write.csv('output_data/dt.summary.output.cp.nz.csv')


## Level Analysis


dt.all.eos.ts <- dt.all.eos[,.(model.short, scenario=Scenario, country,sector, year, series.out1,
              series.nested.mean, series.nested.high, series.nested.low,
              series.out.2.mean, series.out.2.high, series.out.2.low
)] 

dt.all.eos.ts <- melt(setDT(dt.all.eos.ts), id.vars = c('model.short','scenario', 'country','sector', 'year' ), variable.name = "eos_type")


dt.all.eos.ts$eos_type <- plyr::mapvalues(dt.all.eos.ts$eos_type, from=c('series.out1', 'series.nested.mean', 'series.nested.high', 'series.nested.low', 
                                         'series.out.2.mean', 'series.out.2.high', 'series.out.2.low'),
          to=c('Energy Only', '(KL)(EM) mean', '(KL)(EM) p97.5', '(KL)(EM) p2.5', 
               '(KL)E mean', '(KL)E p97.5', '(KL)E p2.5'))


dt.all.eos.ts[,value.2020:=value[year==2020],.(model.short,scenario, country, sector, year, eos_type,value)]

dt.all.eos.ts %>% group_by(model.short, scenario, country,
                           sector, eos_type) %>% 
  mutate(value.2020 = zoo::na.locf(value.2020, na.rm = FALSE)) %>% 
  mutate(value.index = value/value.2020) %>% ungroup() %>% 
  pivot_wider(id_cols = c('model.short', 'country',
                          'sector', 'year','eos_type'),
              names_from = scenario, values_from = value.index) %>%
  write.csv(paste0(path_run_out, '/dt_all_eos_ts.csv'), row.names = F)
  





