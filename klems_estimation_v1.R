####################################################################
# Calculate capital and labor shares from EU KLEMS
#  11/21/23
#           Generally, the principle is to have minimal missings and so I use a waterfall to take from the country-level when sector-country is not available. 
####################################################################

library(parallel)
detectCores()
library(ggplot2)
library(grid)
library(gridExtra)
library(data.table)
library(readxl)


#Set working directory here. This would include the functions folder. 
path_wd <- "~/workspace/climate/NGFS/hybMRIO_IAM_v2/" 
setwd(path_wd)


path_data_IO <-"~/data/IO/EXIOBASE/IOT_2022_ixi/"
path_data_IAM <-"~/data/macro/IAM_out/"
path_data_KLEMS <-"~/data/macro/euklems/"

run_id<-"2023-12-08_08-55"

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





dt.natl<-fread(paste0(path_data_KLEMS,"national accounts.csv"))

dt.growth<-fread(paste0(path_data_KLEMS,"growth accounts.csv"))
dt.natl<-dt.natl[order(geo_code, nace_r2_code, year)]


#need capital stock and labor quantities for our C-D production function. Hours worked is in the standard national accounts
dt.capital<-fread(paste0(path_data_KLEMS,"capital accounts.csv"))

#############################################################################################
# Taking energy shares from the latest available EU KLEMS data, 2012.
#       It seems only to be populated for the US though. Use BEA data for next iteration since it is later and directly from the source.
#############################################################################################


dt.out.12<-fread(paste0(path_data_KLEMS,"2012/dataverse_files-2/12I_output.csv"))
dt.out.12.vars<-dt.out.12[var%in%c("LAB","CAP","II","IIE","IIM","IIS","GO"),.( isic4, iso3, year, alt_sort_id,var, value)]
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


###############################
# take labor and capital compensation from analytical growth accounts. This is for the share parameters.
###############################

dt.growth.labcap<-dt.growth[var%in%c("LAB","CAP"),.( nace_r2_code, geo_code, year, nace_r2_name,geo_name,var, value)]
dt.growth.labcap.wide<-dcast(dt.growth.labcap, nace_r2_code +geo_code +year+ nace_r2_name+geo_name ~var, value.var="value")


dt.ykl<-merge(dt.growth.labcap.wide,
              dt.natl[,.(geo_code, nace_r2_code, year,GO_CP, II_CP,VA_CP, COMP, H_EMP.1=H_EMP)],
              by = c("geo_code","nace_r2_code", "year"))

### ****** growth has 49k, natl has 59k, so need to figure out what's missing exactly.   ****** 

dt.ykl[,labor_share.1 := LAB/GO_CP]
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
dt.ykl[,cap_share := cap_share.1]
dt.ykl[,II_share := II_share.1]
dt.ykl[,sum_share :=sum_share.1]
dt.ykl[is.na(sum_share.1),labor_share :=  LAB.tot/GO_CP.tot]
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

x1.klems.ctry<-merge(x1.klems,xwalk.exiobase_euklems_countries[,.(country=country.exiobase, country.euklems)] ,by="country" ,all.x=T)
x1.klems.shares<-merge(x1.klems.ctry, dt.ykl1[year==2020], by.x=c("country.euklems","sector_id.klems"), by.y=c("geo_code","nace_r2_code"))
#7987 rows-> 4890 rows

xwalk_iso3_geocode<-fread(paste0(path_input,"xwalk.iso3_geoname.csv"))

####################################################
####Here add in energy share from 2010 data. ######
####################################################
dt.energyshare<-merge(dt.out.12.wide[year== 2010,.(iso3,isic4,  materials_share_II, services_share_II, energy_share_II)],xwalk_iso3_geocode,by="iso3")



###Basically only the US has sectoral energy share data anyway
x1.klems.shares1<-merge(x1.klems.shares,dt.energyshare[geo_code=="US",.(isic4,  materials_share_II, services_share_II, energy_share_II)]
                        , by.x=c("sector_id.klems"), by.y=c("isic4"),all.x=T)

###Make a waterfall, backfill those sectors with NA with the total US energy share
dt.energyshare.tot.us<-dt.energyshare[geo_code=="US"&isic4=="TOT",.(isic4,  materials_share_II, services_share_II, energy_share_II)]
x1.klems.shares1[is.na(materials_share_II),materials_share_II:=dt.energyshare.tot.us[,materials_share_II]]
x1.klems.shares1[is.na(services_share_II),services_share_II:=dt.energyshare.tot.us[,services_share_II]]
x1.klems.shares1[is.na(energy_share_II),energy_share_II:=dt.energyshare.tot.us[,energy_share_II]]

####################################################
####################################################
x1.klems.shares.cprs<-merge(x1.klems.shares1,industries.cprs$industry.tab[,.(sector.old, sector,sector_id)],by="sector.old")



x1.cprs.shares<-x1.klems.shares.cprs[,.(gross_output=sum(gross_output),labor_share.wt=sum(labor_share*gross_output)/sum(gross_output),
                                        cap_share.wt=sum(cap_share*gross_output)/sum(gross_output),
                                        II_share.wt=sum(II_share*gross_output)/sum(gross_output),
                                        sum_share.wt=sum(sum_share*gross_output)/sum(gross_output),
                                        materials_share_II = sum(materials_share_II*gross_output)/sum(gross_output),
                                        services_share_II = sum(services_share_II*gross_output)/sum(gross_output),
                                        energy_share_II = sum(energy_share_II*gross_output)/sum(gross_output),
                                        H_EMP = sum(H_EMP),
                                        Kq_GFCF = sum(Kq_GFCF),
                                        K_GFCF = sum(K_GFCF)
                                        
),.(country, sector_id, sector)]

#gross output is 0 in all of these so it's all good. 
x1.cprs.shares[is.na(cap_share.wt)]
x1.cprs.shares[is.na(H_EMP)]

#capital and labor shares aren't too good though. Japan, Luxembourg have a lot of missings. This must be because they were missing GO_PC for that sector. 



fwrite(x1.cprs.shares[,.(country, sector_id, sector, gross_output, 
                           labor_share =labor_share.wt, 
                           cap_share=cap_share.wt , 
                           II_share=II_share.wt ,
                           materials_share_II,
                           services_share_II,
                           energy_share_II,
                         Kq_GFCF,
                         H_EMP
)],paste0(path_output,"klems_shares.csv"))



