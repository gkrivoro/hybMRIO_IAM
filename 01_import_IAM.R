####################################################################
#   01 Import_IAM.R
#  The script cleans and select the IAM energy series necessary for the
#  sectoral disaggregation. 
# 
#  Source of https://data.ene.iiasa.ac.at/ngfs/#/login?redirect=%2Fdownloads
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
path_wd <- "C:/Users/boldrin/Sectoral Granularity/repo_final/" 
setwd(path_wd)

path_data <- "C:/Users/boldrin/Sectoral Granularity/repo_final/input_data/"


## Pre release version
iam_raw <-read_csv("C:/Users/boldrin/Downloads/1720092079042-snapshot-81/output/snapshot_all_regions.csv")
iam_raw <- iam_raw %>%
  mutate(Scenario = recode(Scenario,
                           "d_delfrag" = "Delayed transition",
                           "d_strain" = "Fragmented World",
                           "h_ndc" = "Nationally Determined Contributions (NDCs)",
                           "h_cpol" = "Current Policies",
                           "o_1p5c" = "Net Zero 2050",
                           "o_2c" = "Below 2°C",
                           "o_lowdem" = "Low demand"))

scenarios = c("Delayed transition","Fragmented World","Nationally Determined Contributions (NDCs)","Current Policies","Net Zero 2050","Below 2°C","Low demand")


#iam_raw <-read_excel(paste0(path_data,"/IAM_data.xlsx"))

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
  "MESSAGEix-GLOBIOM 2.0-R12|Sub-saharan Africa",
  "MESSAGEix-GLOBIOM 2.0-R12|Rest Centrally Planned Asia",
  "MESSAGEix-GLOBIOM 2.0-R12|China",
  "MESSAGEix-GLOBIOM 2.0-R12|Eastern Europe",
  "MESSAGEix-GLOBIOM 2.0-R12|Former Soviet Union",
  "MESSAGEix-GLOBIOM 2.0-R12|Latin America and the Caribbean",
  "MESSAGEix-GLOBIOM 2.0-R12|Middle East and North Africa",
  "MESSAGEix-GLOBIOM 2.0-R12|North America",
  "MESSAGEix-GLOBIOM 2.0-R12|Pacific OECD",
  "MESSAGEix-GLOBIOM 2.0-R12|Other Pacific Asia",
  "MESSAGEix-GLOBIOM 2.0-R12|South Asia",
  "MESSAGEix-GLOBIOM 2.0-R12|Western Europe",
  "REMIND-MAgPIE 3.3-4.8|Canada, NZ, Australia",
  "REMIND-MAgPIE 3.3-4.8|China",
  "REMIND-MAgPIE 3.3-4.8|EU 28",
  "REMIND-MAgPIE 3.3-4.8|India",
  "REMIND-MAgPIE 3.3-4.8|Japan",
  "REMIND-MAgPIE 3.3-4.8|Latin America and the Caribbean",
  "REMIND-MAgPIE 3.3-4.8|Middle East, North Africa, Central Asia",
  "REMIND-MAgPIE 3.3-4.8|Non-EU28 Europe",
  "REMIND-MAgPIE 3.3-4.8|Other Asia",
  "REMIND-MAgPIE 3.3-4.8|Countries from the Reforming Economies of the Former Soviet Union",
  "REMIND-MAgPIE 3.3-4.8|Sub-saharan Africa",
  "REMIND-MAgPIE 3.3-4.8|United States of America"
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
            'MESSAGEix-GLOBIOM 2.0-M-R12-NGFS',
            'REMIND-MAgPIE 3.3-4.8'
)


dt.wide <- iam_raw %>% filter(Region %in% regions,
                   Variable %in% variables, 
                   Model %in% models,
                   Scenario %in% scenarios) %>% as.data.table() 

dt.wide <- dt.wide %>% select(Model,
                              Scenario,
                              Region,
                              Variable,
                              Unit,
paste0(seq(2020,2100, by = 5)))


#Return a clean extract for later analysis
dt.wide %>% write.csv(paste0(path_data,"/IAM_data_extract_phaseV.csv"), row.names = F)

