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

path_data <- "C:/Users/boldrin/Sectoral Granularity/repo/input_data/"

iam_raw <-read_excel(paste0(path_data,"/IAM_data.xlsx"))

regions <- list(
  "World",
  "GCAM 6.0 NGFS|Africa_Eastern",
  "GCAM 6.0 NGFS|Africa_Northern",
  "GCAM 6.0 NGFS|Africa_Southern",
  "GCAM 6.0 NGFS|Africa_Western",
  "GCAM 6.0 NGFS|Argentina",
  "GCAM 6.0 NGFS|Australia_NZ",
  "GCAM 6.0 NGFS|Brazil",
  "GCAM 6.0 NGFS|Canada",
  "GCAM 6.0 NGFS|Central America and Caribbean",
  "GCAM 6.0 NGFS|Central Asia",
  "GCAM 6.0 NGFS|China",
  "GCAM 6.0 NGFS|Colombia",
  "GCAM 6.0 NGFS|EU-12",
  "GCAM 6.0 NGFS|EU-15",
  "GCAM 6.0 NGFS|Europe_Eastern",
  "GCAM 6.0 NGFS|Europe_Non_EU",
  "GCAM 6.0 NGFS|European Free Trade Association",
  "GCAM 6.0 NGFS|India",
  "GCAM 6.0 NGFS|Indonesia",
  "GCAM 6.0 NGFS|Japan",
  "GCAM 6.0 NGFS|Mexico",
  "GCAM 6.0 NGFS|Middle East",
  "GCAM 6.0 NGFS|Pakistan",
  "GCAM 6.0 NGFS|Russia",
  "GCAM 6.0 NGFS|South Africa",
  "GCAM 6.0 NGFS|South America_Northern",
  "GCAM 6.0 NGFS|South America_Southern",
  "GCAM 6.0 NGFS|South Asia",
  "GCAM 6.0 NGFS|South Korea",
  "GCAM 6.0 NGFS|Southeast Asia",
  "GCAM 6.0 NGFS|Taiwan",
  "GCAM 6.0 NGFS|USA",
  "MESSAGEix-GLOBIOM 1.1-R12|Sub-saharan Africa",
  "MESSAGEix-GLOBIOM 1.1-R12|Rest Centrally Planned Asia",
  "MESSAGEix-GLOBIOM 1.1-R12|China",
  "MESSAGEix-GLOBIOM 1.1-R12|Eastern Europe",
  "MESSAGEix-GLOBIOM 1.1-R12|Former Soviet Union",
  "MESSAGEix-GLOBIOM 1.1-R12|Latin America and the Caribbean",
  "MESSAGEix-GLOBIOM 1.1-R12|Middle East and North Africa",
  "MESSAGEix-GLOBIOM 1.1-R12|North America",
  "MESSAGEix-GLOBIOM 1.1-R12|Pacific OECD",
  "MESSAGEix-GLOBIOM 1.1-R12|Other Pacific Asia",
  "MESSAGEix-GLOBIOM 1.1-R12|South Asia",
  "MESSAGEix-GLOBIOM 1.1-R12|Western Europe",
  "REMIND-MAgPIE 3.2-4.6|Canada, NZ, Australia",
  "REMIND-MAgPIE 3.2-4.6|China",
  "REMIND-MAgPIE 3.2-4.6|EU 28",
  "REMIND-MAgPIE 3.2-4.6|India",
  "REMIND-MAgPIE 3.2-4.6|Japan",
  "REMIND-MAgPIE 3.2-4.6|Latin America and the Caribbean",
  "REMIND-MAgPIE 3.2-4.6|Middle East, North Africa, Central Asia",
  "REMIND-MAgPIE 3.2-4.6|Non-EU28 Europe",
  "REMIND-MAgPIE 3.2-4.6|Other Asia",
  "REMIND-MAgPIE 3.2-4.6|Countries from the Reforming Economies of the Former Soviet Union",
  "REMIND-MAgPIE 3.2-4.6|Sub-saharan Africa",
  "REMIND-MAgPIE 3.2-4.6|United States of America"
)

variables <- list(
  "Secondary Energy|Electricity|Oil",
  "Secondary Energy|Liquids|Oil",
  "Final Energy|Transportation",
  "Primary Energy|Coal",
  "Final Energy|Industry",
  "Secondary Energy|Electricity|Coal",
  "Secondary Energy|Electricity|Gas",
  "Primary Energy|Gas",
  "Final Energy|Residential and Commercial",
  "Secondary Energy|Electricity|Biomass",
  "Final Energy|Industry|Non-ferrous metals",
  "Final Energy|Transportation|Aviation|Passenger",
  "Secondary Energy|Electricity|Non-Biomass Renewables",
  "Final Energy|Transportation|Rail",
  "Final Energy|Transportation|Road|Passenger",
  "Final Energy|Industry|Chemicals",
  "Final Energy|Industry|Cement",
  "Final Energy|Industry|Steel",
  "Final Energy|Transportation|Road|Freight",
  "Final Energy|Transportation|Aviation",
  "Final Energy|Transportation|Maritime"
)

models <- c('GCAM 6.0 NGFS',
            'MESSAGEix-GLOBIOM 1.1-M-R12',
            'REMIND-MAgPIE 3.2-4.6'
)


dt.wide <- iam_raw %>% filter(Region %in% regions,
                   Variable %in% variables, 
                   Model %in% models) %>% as.data.table() %>%
  write.csv(paste0(path_data,"/IAM_data_extract.csv"), row.names = F)


