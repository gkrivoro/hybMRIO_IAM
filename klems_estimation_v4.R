# ###################################################################
# Calculate capital and labor shares from EU KLEMS. Cleaned version
#  2/20/2024
#           Generally, the principle is to have minimal missings and so I use a waterfall to take from the country-level when sector-country is not available.
#   [edit: 2/20 ] this version is a challenge to the US KLEMS version with slightly different variables . 
#                 this means doing a time series regression instead of cross-sectionally ala VdW. 
# ###################################################################

library(parallel)
detectCores()
library(ggplot2)
library(grid)
library(gridExtra)
library(data.table)
library(readxl)


#Set working directory here. This would include the functions folder. 
path_wd <- "C:/Users/boldrin/Sectoral Granularity/repo/" 
setwd(path_wd)


path_data_IO <-"C:/Users/boldrin/Sectoral Granularity/repo/data/IO/EXIOBASE/IOT_2022_ixi/"
path_data_KLEMS <-"C:/Users/boldrin/Sectoral Granularity/repo/data/macro/euklems/"
path_data_energy <-"C:/Users/boldrin/Sectoral Granularity/repo/data/iea/"

#path_data_energy <-"C:/Users/boldrin/Sectoral Granularity/repo/data/eurostat/"

run_id<-"2024-05-23_12-22"

path_run_out <-paste0(path_wd,"run/",run_id,"/out/" )
path_input <-paste0(path_wd,"input_data/" )
path_graphs <-paste0(path_wd,"run/",run_id,"graphs/" )

path_output<-paste0(path_wd,"output_data/")


industries.cprs<-readRDS(paste0(path_run_out,"industries.cprs.v4.RDS"))

xwalk.exiobase_euklems<-fread(paste0(path_input,"exiobase_klems_xwalk.csv"))

x1<-fread(paste0(path_data_IO,"x.txt"))

#full outer join here
xwalk.cprs_klems<-merge(industries.cprs$industry.tab[,.(sector_id,sector, sector.old)], 
                        xwalk.exiobase_euklems[,.(sector.old,sector.klems)], by="sector.old",all=T)



# dt.iea.bal<-fread(paste0(path_data_energy,"/WBAL-2023.csv"))
# 
# unique(dt.iea.bal[,FLOW])
# 
# table(dt.iea.bal[,FLOW],dt.iea.bal[,Flow],exclude=NULL)

dt.natl<-fread(paste0(path_data_KLEMS,"national accounts.csv"))

dt.growth<-fread(paste0(path_data_KLEMS,"growth accounts.csv"))
dt.natl<-dt.natl[order(geo_code, nace_r2_code, year)]

# dt.energy.prices<-fread(paste0(path_data_energy,"klems_energy_fin.csv"))

dt.energy.prices<-fread(paste0(path_data_energy,"klems_energy_fin.csv"))

#need capital stock and labor quantities for our C-D production function. Hours worked is in the standard national accounts
dt.capital<-fread(paste0(path_data_KLEMS,"capital accounts.csv"))

#############################################################################################
# Taking energy shares from the latest available EU KLEMS data, 2012.
#       It seems only to be populated for the US though. Use BEA data for next iteration since it is later and directly from the source.
#############################################################################################


dt.out.12<-fread(paste0(path_data_KLEMS,"2012/dataverse_files-2/12I_output.csv"))
dt.out.12.vars<-dt.out.12[var%in%c("LAB","CAP","II","IIE","IIM","IIS","GO","IIM_P","IIE_P","IIM_QI","IIE_QI"),.( isic4, iso3, year, alt_sort_id,var, value)]
dt.out.12.wide<-dcast(dt.out.12.vars, isic4 +iso3 +year ~var, value.var="value")
dt.out.12.wide[, labor_share:= LAB/GO]
dt.out.12.wide[, cap_share:= CAP/GO]
dt.out.12.wide[, II_share:= II/GO]
dt.out.12.wide[, energy_share:= IIE/GO]
dt.out.12.wide[, materials_share:= IIM/GO]
dt.out.12.wide[, services_share:= IIS/GO]
dt.out.12.wide[, energy_share_II:= IIE/II]
dt.out.12.wide[, materials_share_II:= IIM/II]
dt.out.12.wide[, services_share_II:= IIS/II]
dt.out.12.wide[,sum_share:=labor_share+cap_share+II_share]

#head(dt.out.12.wide[,.(year,CAP)],n=100L)


###############################
# take labor and capital compensation from analytical growth accounts. This is for the share parameters.
###############################

dt.growth.labcap<-dt.growth[var%in%c("LAB","CAP"),.( nace_r2_code, geo_code, year, nace_r2_name,geo_name,var, value)]
dt.growth.labcap.wide<-dcast(dt.growth.labcap, nace_r2_code +geo_code +year+ nace_r2_name+geo_name ~var, value.var="value")

#################
#   adding in the price and quantity indices too - 2/20/24
#################
dt.ykl<-merge(dt.growth.labcap.wide,
              dt.natl[,.(geo_code, nace_r2_code, year,GO_CP, GO_Q, GO_PI, II_CP, II_Q,II_PI ,VA_CP,VA_PI, VA_Q, COMP, H_EMP.1=H_EMP)],
              by = c("geo_code","nace_r2_code", "year"))

## Adjust missing quantities
dt.ykl <- dt.ykl %>% mutate(GO_Q = na_if(GO_Q,0),
                            II_Q = na_if(II_Q,0)) %>% 
  mutate(GO_Q = na_if(GO_Q,Inf),
         II_Q = na_if(II_Q,Inf)) %>% 
  group_by(geo_code,nace_r2_code) %>% arrange(year) %>% 
  mutate(GO_Q = zoo::na.locf(GO_Q, na.rm = FALSE),
         II_Q = zoo::na.locf(II_Q, na.rm = FALSE)) %>% 
  mutate(GO_Q = zoo::na.locf(GO_Q, na.rm = FALSE, fromLast= TRUE),
         II_Q = zoo::na.locf(II_Q, na.rm = FALSE, fromLast= TRUE)) %>% 
  arrange(geo_code,nace_r2_code,year) %>% ungroup() %>% as.data.table()




### ****** growth has 49k, natl has 59k, so need to figure out what's missing exactly.   ****** 

dt.ykl[,labor_share.1 := LAB/GO_CP]
# compare with compensation share
dt.ykl[, labor_share.COMP.1:= COMP/GO_CP]





dt.ykl[,cap_share.1 := CAP/GO_CP]
dt.ykl[,II_share.1 := II_CP/GO_CP]
dt.ykl[,sum_share.1:=labor_share.1+cap_share.1+II_share.1]

## Backfill from TOT if missing. 
dt.ykl<-merge(dt.ykl,
              dt.ykl[nace_r2_code=="TOT",.(geo_code, year,
                                           GO_CP.tot=GO_CP, 
                                           II_CP.tot=II_CP,
                                           VA_CP.tot=VA_CP, 
                                           COMP.tot =COMP,
                                           LAB.tot = LAB,
                                           CAP.tot = CAP,
                                           H_EMP.tot=H_EMP.1)],
              by = c("geo_code", "year"))
#backfill 
dt.ykl[,labor_share :=  labor_share.1]
dt.ykl[,labor_share.COMP :=  labor_share.COMP.1]

dt.ykl[,cap_share := cap_share.1]
dt.ykl[,II_share := II_share.1]
dt.ykl[,sum_share :=sum_share.1]
dt.ykl[is.na(sum_share.1),labor_share :=  LAB.tot/GO_CP.tot]
dt.ykl[is.na(sum_share.1),labor_share.COMP :=  COMP.tot/GO_CP.tot]

dt.ykl[is.na(sum_share.1),cap_share := CAP.tot/GO_CP.tot]
dt.ykl[is.na(sum_share.1),II_share := II_CP.tot/GO_CP.tot]
dt.ykl[is.na(sum_share.1),sum_share :=labor_share+ cap_share+II_share]

dt.ykl[,H_EMP:=  H_EMP.1]

dt.ykl[,pc_GO:=GO_CP/GO_CP.tot]

dt.ykl[,H_EMP.impute:=  H_EMP.tot*pc_GO]

dt.ykl[is.na(H_EMP.1),H_EMP:=  H_EMP.impute]




## Adding in capital quantity (Kq_GFCF) aka net capital stock, total assets, chain linked volumes, millions of national currency. K_GFCF might include prices so we don't use it
#fill in from country-level TOT * times sectoral output share if missing
dt.ykl1<-merge(dt.ykl, dt.capital[,.(nace_r2_code, geo_code,year,Kq_GFCF.1=Kq_GFCF,K_GFCF.1=K_GFCF)], by=c("year","geo_code", "nace_r2_code"))
dt.ykl1<-merge(dt.ykl1, dt.capital[nace_r2_code=="TOT",.(geo_code,year,Kq_GFCF.tot=Kq_GFCF,K_GFCF.tot=K_GFCF)], by=c("year","geo_code"))



dt.ykl1[,K_GFCF.impute:=pc_GO*K_GFCF.tot]
dt.ykl1[,Kq_GFCF.impute:=pc_GO*Kq_GFCF.tot]

dt.ykl1[,K_GFCF := K_GFCF.1]
dt.ykl1[,Kq_GFCF := Kq_GFCF.1]
dt.ykl1[is.na(K_GFCF.1),K_GFCF := K_GFCF.impute]
dt.ykl1[is.na(Kq_GFCF),Kq_GFCF := Kq_GFCF.impute]



xwalk.exiobase_euklems_countries<-fread(paste0(path_input, "xwalk.exiobase_euklems_countries.csv"))


x1.klems<-merge(x1[,.(country=region,sector.old=sector, gross_output=indout)],xwalk.exiobase_euklems[,.(sector.old, sector_id.klems, sector.klems)], by=c("sector.old"))
###nrows(x1.klems) = nrows(x1)

######################################
# [2/23] Why do we add x1 here? what does the net output of EXIOBASE give us here when we have GO_CP. 
######################################

x1.klems.ctry<-merge(x1.klems,xwalk.exiobase_euklems_countries[,.(country=country.exiobase, country.euklems)] ,by="country" ,all.x=T)
x1.klems.shares<-merge(x1.klems.ctry, dt.ykl1[year==2020], by.x=c("country.euklems","sector_id.klems"), by.y=c("geo_code","nace_r2_code"))

###2/23 remove this restriction here. 
#  dt.ykl1.exiobase<-merge(dt.ykl1,unique(x1.klems.ctry[,.(country, sector.old, country.euklems,sector_id.klems)]), 
#                          by.x=c("geo_code","nace_r2_code"), by.y=c("country.euklems","sector_id.klems"),all.x=T)
# # #7987 rows-> 4890 rows

xwalk_iso3_geocode<-fread(paste0(path_input,"xwalk.iso3_geoname.csv"))


####################################################
####Here add in energy share from 2010 data. ######
####################################################
dt.energyshare<-merge(dt.out.12.wide[year== 2010,.(iso3,isic4,  materials_share_II, services_share_II, energy_share_II,
                                                   IIM.12=IIM,IIM_P.12=IIM_P,IIE.12=IIE,IIE_P.12=IIE_P,IIE_QI.12=IIE_QI, IIM_QI.12=IIM_QI)]
                      ,xwalk_iso3_geocode,by="iso3")


###Basically only the US has sectoral energy share data anyway
x1.klems.shares1<-merge(x1.klems.shares,dt.energyshare[geo_code=="US",.(isic4,  materials_share_II, services_share_II, energy_share_II, 
                                                                        IIM.12, IIM_P.12, IIE.12, IIE_P.12, IIE_QI.12, IIM_QI.12)]
                        , by.x=c("sector_id.klems"), by.y=c("isic4"),all.x=T)

###Make a waterfall, backfill those sectors with NA with the total US energy share
dt.energyshare.tot.us<-dt.energyshare[geo_code=="US"&isic4=="TOT",.(isic4,  materials_share_II, services_share_II, energy_share_II,IIM.12 ,IIM_P.12 ,IIE.12, IIE_P.12 ,IIE_QI.12, IIM_QI.12)]
x1.klems.shares1[is.na(materials_share_II),materials_share_II:=dt.energyshare.tot.us[,materials_share_II]]
x1.klems.shares1[is.na(services_share_II),services_share_II:=dt.energyshare.tot.us[,services_share_II]]
x1.klems.shares1[is.na(energy_share_II),energy_share_II:=dt.energyshare.tot.us[,energy_share_II]]

###Make a waterfall, backfill prices and quantities 
x1.klems.shares1[is.na(IIM.12),IIM.12:=dt.energyshare.tot.us[,IIM.12]*GO_CP/sum(GO_CP),.(nace_r2_name, geo_name,year)]
x1.klems.shares1[is.na(IIM_P.12),IIM_P.12:=dt.energyshare.tot.us[,IIM_P.12]]
x1.klems.shares1[is.na(IIM_QI.12),IIM_QI.12:=dt.energyshare.tot.us[,IIM_QI.12]]
x1.klems.shares1[is.na(IIE.12),IIE.12:=dt.energyshare.tot.us[,IIE.12]*GO_CP/sum(GO_CP),.(nace_r2_name, geo_name,year)]
x1.klems.shares1[is.na(IIE_P.12),IIE_P.12:=dt.energyshare.tot.us[,IIE_P.12]]
x1.klems.shares1[is.na(IIE_QI.12),IIE_QI.12:=dt.energyshare.tot.us[,IIE_QI.12]]

#then for the 


####################################################
## 2/23 NEW
# weighting by GO_CP now. 
####################################################


x1.klems.shares.cprs<-merge(x1.klems.shares1,industries.cprs$industry.tab[,.(sector.old, sector,sector_id)],by="sector.old")

dt.ykl1.cprs1<-merge(dt.ykl1, unique(x1.klems.shares.cprs[,.(sector, sector_id, country,nace_r2_name, geo_name)]), by=c("geo_name","nace_r2_name"))

##### Also adding derived price differences before the summation to avoid some kind of jensens inequality effect.

dt.ykl1.cprs1<-dt.ykl1.cprs1[order(geo_name, nace_r2_name, year)]


dt.ykl1.cprs1[,log.EM_Y_share:=log(II_CP/GO_CP)]
dt.ykl1.cprs1[,log.EM_Y_p:=log(II_PI/GO_PI)]
dt.ykl1.cprs1[,log.EM_Y_q:=log(II_Q/GO_Q)]
dt.ykl1.cprs1[,D1.log.EM_Y_q := (log.EM_Y_q-shift(log.EM_Y_q)),.(geo_name, nace_r2_name)]
dt.ykl1.cprs1[,D1.log.EM_Y_share := (log.EM_Y_share-shift(log.EM_Y_share)),.(geo_name, nace_r2_name)]
dt.ykl1.cprs1[,D1.log.EM_Y_p.derived.pre := D1.log.EM_Y_share - D1.log.EM_Y_q]


xwalk.nace.klems.oecd<-fread(paste0(path_input, "xwalk.nace.klems.oecd.csv"))

dt.energy.prices1<-merge(dt.energy.prices,xwalk.nace.klems.oecd, by.x="nace", by.y="nace.oecd",allow.cartesian = T )

#we now need a unique series for each nace.klems and no duplicates.
# the units seem a little different, so I think it's best to generate log first differences and then average them
dt.energy.prices1[,log.E_pq:=log(ENERGY)]
dt.energy.prices1[,log.E_p:=log(energy_price_index)]
dt.energy.prices1[,log.E_q:=log(TOTAL)]

dt.energy.prices1<-dt.energy.prices1[order(code2,nace.klems,year)]

dt.energy.prices1[,D1.log.E_q := (log.E_q-shift(log.E_q)),.(code2,nace.klems)]
dt.energy.prices1[,D1.log.E_p := (log.E_p-shift(log.E_p)),.(code2,nace.klems)]
dt.energy.prices1[,D1.log.E_pq := (log.E_pq-shift(log.E_pq)),.(code2,nace.klems)]


dt.energy.prices.rollup<-dt.energy.prices1[,.(D1.log.E_q=mean(D1.log.E_q),
                                              D1.log.E_p = mean(D1.log.E_p),
                                              D1.log.E_pq = mean(D1.log.E_pq)) ,.(code2,nace.klems,year)]
dt.energy.prices.rollup[code2=="UK", code2:="GB"]

dt.ykl1.cprs1.enprice<-merge(dt.ykl1.cprs1,dt.energy.prices.rollup, 
                             by.x=c("country","nace_r2_code","year"), by.y = c("code2","nace.klems","year"))



dt.ykl1.cprs<-dt.ykl1.cprs1.enprice[,.(GO_CP=sum(GO_CP),
                                       GO_Q=sum(GO_Q*GO_CP)/sum(GO_CP),
                                       GO_PI=sum(GO_PI*GO_CP)/sum(GO_CP),
                                       VA_CP=sum(VA_CP),
                                       VA_Q=sum(VA_Q*VA_CP)/sum(VA_CP),
                                       VA_PI=sum(GO_PI*GO_CP)/sum(VA_PI),
                                       labor_share=sum(labor_share*GO_CP)/sum(GO_CP),
                                       COMP=sum(COMP*GO_CP)/sum(GO_CP),
                                       D1.log.E_q = sum(D1.log.E_q*GO_CP,na.rm=T)/sum(GO_CP,na.rm=T),
                                       D1.log.E_p = sum(D1.log.E_p*GO_CP,na.rm=T)/sum(GO_CP,na.rm=T),
                                       D1.log.E_pq = sum(D1.log.E_pq*GO_CP,na.rm=T)/sum(GO_CP,na.rm=T),
                                       D1.log.EM_Y_p.derived.pre = sum(D1.log.EM_Y_p.derived.pre*GO_CP,na.rm=T)/sum(GO_CP,na.rm=T),
                                       cap_share=sum(cap_share*GO_CP)/sum(GO_CP),
                                       II_share=sum(II_share*GO_CP)/sum(GO_CP),
                                       II_CP=sum(II_CP*GO_CP)/sum(GO_CP),
                                       II_PI=sum(II_PI*GO_CP)/sum(GO_CP),
                                       II_Q=sum(II_Q*GO_CP)/sum(GO_CP),
                                       sum_share=sum(sum_share*GO_CP)/sum(GO_CP),
                                       H_EMP = sum(H_EMP),
                                       Kq_GFCF = sum(Kq_GFCF),
                                       K_GFCF = sum(K_GFCF)
),.(country, sector_id, sector,year)]



dt.ykl1.cprs[,log.Y_pq:=log(GO_CP)]
dt.ykl1.cprs[,log.Y_p:=log(GO_PI)]
dt.ykl1.cprs[,log.Y_q:=log(GO_Q)]

dt.ykl1.cprs[,log.EM_pq:=log(II_CP)]
dt.ykl1.cprs[,log.EM_p:=log(II_PI)]
dt.ykl1.cprs[,log.EM_q:=log(II_Q)]

dt.ykl1.cprs[,log.KL_pq:=log(VA_CP)]
dt.ykl1.cprs[,log.KL_p:=log(VA_PI)]
dt.ykl1.cprs[,log.KL_q:=log(VA_Q)]

dt.ykl1.cprs[,log.K_pq:=log(K_GFCF)]
dt.ykl1.cprs[,log.K_p:=log(VA_PI)]
dt.ykl1.cprs[,log.K_q:=log(Kq_GFCF)]

dt.ykl1.cprs[,log.L_pq:=log(K_GFCF)]
dt.ykl1.cprs[,log.L_p:=log(H_EMP)]
#dt.ykl1.cprs[,log.L_q:=log(Kq_GFCF)]

dt.ykl1.cprs[,log.EM_Y_share:=log(II_CP/GO_CP)]
dt.ykl1.cprs[,log.EM_Y_p:=log(II_PI/GO_PI)]
dt.ykl1.cprs[,log.EM_Y_q:=log(II_Q/GO_Q)]


dt.ykl1.cprs<-dt.ykl1.cprs[order(country, sector_id, year)]


dt.ykl1.cprs[,D1.log.Y_pq:=(log.Y_pq-shift(log.Y_pq)),.(country, sector_id)]
dt.ykl1.cprs[,D1.log.Y_p:=(log.Y_p-shift(log.Y_p)),.(country, sector_id)]
dt.ykl1.cprs[,D1.log.Y_q:=(log.Y_q-shift(log.Y_q)),.(country, sector_id)]

dt.ykl1.cprs[,D1.log.EM_Y_q := (log.EM_Y_q-shift(log.EM_Y_q)),.(country, sector_id)]
dt.ykl1.cprs[,D1.log.EM_Y_share := (log.EM_Y_share-shift(log.EM_Y_share)),.(country, sector_id)]
dt.ykl1.cprs[,D1.log.EM_Y_p := (log.EM_Y_p-shift(log.EM_Y_p)),.(country, sector_id)]


dt.ykl1.cprs[,D1.log.EM_pq := (log.EM_pq-shift(log.EM_pq)),.(country, sector_id)]
dt.ykl1.cprs[,D1.log.EM_q := (log.EM_q-shift(log.EM_q)),.(country, sector_id)]
dt.ykl1.cprs[,D1.log.EM_p := (log.EM_p-shift(log.EM_p)),.(country, sector_id)]


dt.ykl1.cprs[,D1.log.KL_pq := (log.KL_pq-shift(log.KL_pq)),.(country, sector_id)]
dt.ykl1.cprs[,D1.log.KL_q := (log.KL_q-shift(log.KL_q)),.(country, sector_id)]
dt.ykl1.cprs[,D1.log.KL_p := (log.KL_p-shift(log.KL_p)),.(country, sector_id)]


xwalk.eos_sectors<-fread(paste0(path_input,"EOS_supersectors.csv"))

dt.ykl1.cprs<-merge(dt.ykl1.cprs, xwalk.eos_sectors, by="sector")




dt.ykl1.cprs[,D1.log.E_Y_q := D1.log.E_q-D1.log.Y_q]
dt.ykl1.cprs[,D1.log.E_Y_pq := D1.log.E_pq-D1.log.Y_pq]
dt.ykl1.cprs[,D1.log.E_Y_p := D1.log.E_p-D1.log.Y_p]
dt.ykl1.cprs[,D1.log.E_EM_q := D1.log.E_q-D1.log.EM_q]
dt.ykl1.cprs[,D1.log.E_EM_pq := D1.log.E_pq-D1.log.EM_pq]
dt.ykl1.cprs[,D1.log.E_EM_p := D1.log.E_p-D1.log.EM_p]


dt.ykl1.cprs[,D1.log.KL_Y_q := D1.log.KL_q-D1.log.Y_q]
dt.ykl1.cprs[,D1.log.KL_Y_pq := D1.log.KL_pq-D1.log.Y_pq]
dt.ykl1.cprs[,D1.log.KL_Y_p := D1.log.KL_p-D1.log.Y_p]



getEOS<-function(dat,y,x,segment1,segment_var=NULL,outlier_bounds=2, N_min=50){
  list.res<-list()
  if (is.null(segment_var)){
    for (mysegment1 in unique(dat[,get(segment1)])){
      dat.estimation<-dt.ykl1.cprs[(abs(get(x))<outlier_bounds&abs(get(y))<outlier_bounds)&get(segment1) == mysegment1&
                                     !is.na(get(y))&!is.na(get(x))& 
                                     is.finite(get(y))&is.finite(get(x))]
      print(mysegment1)
      print(nrow(dat.estimation))
      if (nrow(dat.estimation)>N_min){
        results1<-glm(as.formula(paste0(y,"~",x)) , family="gaussian",data=dat.estimation, weights=GO_Q)
        dt.temp<-data.table(coefficients = results1$coefficients,sd = summary(results1)$coefficients[,2],
                            varnames = names(results1$coefficients), segment1=mysegment1)
        setnames(dt.temp,"segment1",segment1)
        list.res[[mysegment1]]<-dt.temp
      }
      dt.res<-rbindlist(list.res)
      dt.res.wide<-dcast(dt.res, formula = as.formula(paste0(segment1,"~varnames")), value.var=c("coefficients","sd"))
      setnames(dt.res.wide,c("sd_(Intercept)","coefficients_(Intercept)"),c("sd_Intercept","coefficients_(Intercept)"))
      
      dt.res.wide[,EOS:=(-get(paste0("coefficients_",x)))]
      dt.res.wide[,EOS_low95conf:=(-get(paste0("coefficients_",x))-1.96*get(paste0("sd_",x)))]
      dt.res.wide[,EOS_high95conf:=(-get(paste0("coefficients_",x))+1.96*get(paste0("sd_",x)))]
      
      
    }
  } else {
    for (mysegment1 in unique(dat[,get(segment1)])){
      for (mysegment_var in unique(dat[,get(segment_var)])){
        dat.estimation<-dt.ykl1.cprs[(abs(get(x))<outlier_bounds&abs(get(y))<outlier_bounds)&get(segment1) == mysegment1
                                     &get(segment_var) == mysegment_var
                                     &  !is.na(get(y))&!is.na(get(x))& 
                                       is.finite(get(y))&is.finite(get(x))]
        print(mysegment1)
        print(mysegment_var)
        
        print(nrow(dat.estimation))
        if (nrow(dat.estimation)>N_min){
          results1<-glm(as.formula(paste0(y,"~",x)), family="gaussian",data=dat.estimation, weights=GO_Q)
          dt.temp<-data.table(coefficients = results1$coefficients,sd = summary(results1)$coefficients[,2],
                              varnames = names(results1$coefficients), segment1=mysegment1, segment_var=mysegment_var)
          setnames(dt.temp,"segment1",segment1)
          setnames(dt.temp,"segment_var",segment_var)
          
          list.res[[paste0(mysegment1,"_",mysegment_var)]]<-dt.temp
        }
        dt.res<-rbindlist(list.res)
        #print(list.res)
        
        dt.res.wide<-dcast(dt.res, formula = as.formula(paste0(segment1,"+",segment_var,"~varnames")), value.var=c("coefficients","sd"))
        
        
        setnames(dt.res.wide,c("sd_(Intercept)","coefficients_(Intercept)"),c("sd_Intercept","coefficients_(Intercept)"))
        
        dt.res.wide[,EOS:=(-get(paste0("coefficients_",x)))]
        dt.res.wide[,EOS_low95conf:=(-get(paste0("coefficients_",x))-1.96*get(paste0("sd_",x)))]
        dt.res.wide[,EOS_high95conf:=(-get(paste0("coefficients_",x))+1.96*get(paste0("sd_",x)))]
        
      }
    }
  }
  dt.res.wide
}

out1.E_EM<-getEOS(dt.ykl1.cprs,"D1.log.E_EM_q","D1.log.E_EM_p","sector.eos","country")
out1.EM_Y<-getEOS(dt.ykl1.cprs,"D1.log.EM_Y_q","D1.log.EM_Y_p","sector.eos","country")
out1.E_Y<-getEOS(dt.ykl1.cprs,"D1.log.E_Y_q","D1.log.E_Y_p","sector.eos","country")

out1.KL_Y<-getEOS(dt.ykl1.cprs,"D1.log.KL_Y_q","D1.log.KL_Y_p","sector.eos","country")


sum.E_EM<-out1.E_EM[,.(mean = mean(EOS),
                       p10 = quantile(EOS,c(.1)),
                       p25 = quantile(EOS,c(.25)),
                       p50 = quantile(EOS,c(.5)),
                       p75 = quantile(EOS,c(.75)),
                       p90 = quantile(EOS,c(.9))),.(sector.eos)]

sum.EM_Y<-out1.EM_Y[,.(mean = mean(EOS),
                       p10 = quantile(EOS,c(.1)),
                       p25 = quantile(EOS,c(.25)),
                       p50 = quantile(EOS,c(.5)),
                       p75 = quantile(EOS,c(.75)),
                       p90 = quantile(EOS,c(.9))),.(sector.eos)]

sum.E_Y<-out1.E_Y[,.(mean = mean(EOS),
                       p10 = quantile(EOS,c(.1)),
                       p25 = quantile(EOS,c(.25)),
                       p50 = quantile(EOS,c(.5)),
                       p75 = quantile(EOS,c(.75)),
                       p90 = quantile(EOS,c(.9))),.(sector.eos)]


sum.KL_Y<-out1.KL_Y[,.(mean = mean(EOS),
                       p10 = quantile(EOS,c(.1)),
                       p25 = quantile(EOS,c(.25)),
                       p50 = quantile(EOS,c(.5)),
                       p75 = quantile(EOS,c(.75)),
                       p90 = quantile(EOS,c(.9))),.(sector.eos)]

fwrite(sum.E_EM,paste0(path_input,"sum.E_EM.ctry.csv"))
fwrite(sum.EM_Y,paste0(path_input,"sum.EM_Y.ctry.csv"))
fwrite(sum.KL_Y,paste0(path_input,"sum.KL_Y.ctry.csv"))
fwrite(sum.E_Y,paste0(path_input,"sum.E_Y.ctry.csv"))






x1.klems.shares.cprs<-merge(x1.klems.shares1,industries.cprs$industry.tab[,.(sector.old, sector,sector_id)],by="sector.old")



x1.cprs.shares_II<-x1.klems.shares.cprs[,.(materials_share_II.12 = sum(materials_share_II*GO_CP)/sum(GO_CP),
                                        services_share_II.12 = sum(services_share_II*GO_CP)/sum(GO_CP),
                                        energy_share_II.12 = sum(energy_share_II*GO_CP)/sum(GO_CP),
                                        IIM.12=sum(IIM.12*GO_CP)/sum(GO_CP), 
                                        IIM_P.12=sum(IIM_P.12*GO_CP)/sum(GO_CP),
                                        IIE.12=sum(IIE.12*GO_CP)/sum(GO_CP),
                                        IIE_P.12=sum(IIE_P.12*GO_CP)/sum(GO_CP),
                                        IIE_QI.12=sum(IIE_QI.12*GO_CP)/sum(GO_CP),
                                        IIM_QI.12 =sum(IIM_QI.12*GO_CP)/sum(GO_CP)
                                        
),.(country, sector_id, sector,year)]


x1.cprs.shares_II[sector=="3-energy-intensive"]
x1.klems.shares.cprs[sector=="3-energy-intensive"]




x1.cprs.shares<-dt.ykl1.cprs[,.(GO_CP=sum(GO_CP),
                                GO_Q=sum(GO_Q),
                                GO_PI=sum(GO_PI*GO_CP)/sum(GO_CP),
                                VA_CP=sum(VA_CP*GO_CP)/sum(GO_CP),
                                VA_Q=sum(VA_Q),
                                VA_PI=sum(VA_PI*GO_CP)/sum(GO_CP),
                                II_CP=sum(II_CP*GO_CP)/sum(GO_CP),
                                II_Q=sum(II_Q),
                                II_PI=sum(II_PI*GO_CP)/sum(GO_CP),
                                    labor_share=sum(labor_share*GO_CP)/sum(GO_CP),
                                        cap_share=sum(cap_share*GO_CP)/sum(GO_CP),
                                        II_share=sum(II_share*GO_CP)/sum(GO_CP),
                                        sum_share=sum(sum_share*GO_CP)/sum(GO_CP),
                                        H_EMP = sum(H_EMP),
                                        Kq_GFCF = sum(Kq_GFCF),
                                        K_GFCF = sum(K_GFCF)
),.(country, sector_id, sector,year)]


x1.cprs.shares.all<-merge(x1.cprs.shares, x1.cprs.shares_II, by=c("country","sector_id","sector","year"))


fwrite(x1.cprs.shares.all,paste0(path_output,"klems_shares.csv"))


