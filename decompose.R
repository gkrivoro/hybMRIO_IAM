
######################################
#       New version for v8 (aka 9-other disaggregation and NGFS phase IV)
#         here we specify the run number and run specific things. 
#     This script decomposes the series that were generated in exiobasemon_run_v5.R into constituents.
#         First one runs the function decompose_country() to decompose the country of interest
#         Then, you can graph the individual series/model you want with graph.IO.decompose()
###############################



#rm(list=ls())
library(parallel)
detectCores()
library(ggplot2)
library(grid)
library(gridExtra)
library(data.table)

path_wd <- "~/workspace/climate/NGFS/codes/" 
setwd(path_wd)
path_data <-"~/data/IO/EXIOBASE/IOT_2022_ixi/"
#specify run_id that you want to decompose here

#run_id<-"2023-11-13_15-58"
run_id<-"2023-11-22_15-05"
#this output path has the run-invariant outputs, like direct data from IO table. 
path_output <-paste0(path_wd,"output_data/disagg/" )

path_run_out <-paste0(path_wd,"run/", run_id, "/out/")
path_input <-paste0(path_wd,"input_data/" )
path_graphs <-paste0(path_wd,"run/",run_id,"/graphs/" )


industries.cprs<-readRDS(paste0(path_run_out,"industries.cprs.v4.RDS"))
model_xwalk<-fread(paste0(path_run_out,"model_xwalk.v4.csv" ))


xwalk.CPRS_IAM<-fread(paste0(path_input,"/cprs_sector_iam_v4.csv"))
xwalk.CPRS_IAM[, I_standalone:=0]
xwalk.CPRS_IAM[sector.iam!="", I_standalone:=1]
  


set_sectors.IO<-unique(xwalk.CPRS_IAM[I_standalone==0, sector])
set_countries<-unique(industries.cprs$industry.cntry.tab[,country])
set_models<-unique(model_xwalk[,model.short])
# set_countries<-"EE"
# set_sectors<-"7-finance"
# set_models<-"message"



#this is the weight for the IAM series
dt.alpha_IAMIO_b_IOIO<-fread(paste0(path_run_out,"dt.alpha_IAMIO_b_IOIO.csv" ))
#these are the values for the IAM series. 
dt.standalone.iam<-fread(paste0(path_run_out,"dt.standalone.iam.csv" ))


#this is used for the weight for the IO series. 
dt.b_ij_IO<-fread(paste0(path_run_out,"dt.b_ij_IO.csv" ))
dt.other.iam.energy<-fread(paste0(path_run_out,"dt.other.iam.energy.csv" ))



######################################################################
# This basically recreates the matrices in step (3d) but does not sum them, leaving them ready to be graphed. 
#     it's best to do it country by country like this since all together it's huge. 
#     After that we can graph series by series or model by model. 
######################################################################
source("functions/general.R")
source("functions/graphs.R")




mycountry<-"US"


dt.decomp.cntry<-decompose_country(mycountry,dt.b_ij_IO, dt.alpha_IAMIO_b_IOIO,dt.other.iam.energy,dt.standalone.iam ,model_xwalk,industries.cprs)

for (mymodel in set_models){
  for (mysector in set_sectors.IO){
    graph.IO.decompose(dt.decomp.cntry, mycountry,mysector, mymodel, model_xwalk, paste0(path_graphs,"decompose/",mycountry,"/",mymodel,"/"))
  }
}


dt.out<-dt.all[,.(gross_output.2020= mean(gross_output.2020)),.(country, sector)]


dt.out[,out_share.2020 :=gross_output.2020/sum(gross_output.2020,na.rm=T),.(country)]

dt.out[country=="CN",.(sector, out_share.2020)]

