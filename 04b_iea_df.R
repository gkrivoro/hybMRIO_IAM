
######################################
# 04b_iea_df.R
#   This script cleans and analyze energy balances data and energy prices from eurostat.
# The output is geographic/sectoral energy usage and a energy price index computed as 
# weighted average of electricity, natural gas and oil prices.
# Sources: # https://www.iea.org/data-and-statistics/data-product/world-energy-balances (License required)
#
###############################


library(tidyverse)
library(zoo)

cntry_list <- c('ALB', 'DZA', 'AGO', 'ARG', 'ARM', 'AUS', 'AUT', 'AZE',
                'BHR', 'BGD', 'BLR', 'BEL', 'BEN', 'BOL', 'BIH', 'BWA', 'BRA',
                'BRN', 'BGR', 'KHM', 'CMR', 'CAN', 'CHL', 'CHN', 'TWN', 'COL',
                'COG', 'COD', 'CRI', 'CIV', 'HRV', 'CUB', 'CYP', 'CZE', 'DNK',
                'DOM', 'ECU', 'EGY', 'SLV', 'ERI', 'EST', 'ETH', 'FIN', 'FRA',
                'GAB', 'GEO', 'DEU', 'GHA', 'GIB', 'GRC', 'GTM', 'HTI', 'HND',
                'HKG', 'HUN', 'ISL', 'IND', 'IDN', 'IRN', 'IRQ', 'IRL', 'ISR',
                'ITA', 'JAM', 'JPN', 'JOR', 'KAZ', 'KEN', 'KOR', 'PRK', 'KWT',
                'KGZ', 'LVA', 'LBN', 'LBY', 'LTU', 'LUX', 'MKD', 'MYS', 'MLT',
                'MEX', 'MDA', 'MNG', 'MAR', 'MOZ', 'MMR', 'NAM', 'NPL', 'NLD',
                'NZL', 'NIC', 'NGA', 'NOR', 'OMN', 'PAK', 'PAN', 'PRY', 'PER',
                'PHL', 'POL', 'PRT', 'QAT', 'ROU', 'RUS', 'SAU', 'SEN', 'SRB',
                'SGP', 'SVK', 'SVN', 'ZAF', 'ESP', 'LKA', 'SDN', 'SWE', 'CHE',
                'SYR', 'TJK', 'TZA', 'THA', 'TGO', 'TTO', 'TUN', 'TUR', 'TKM',
                'UKR', 'ARE', 'GBR', 'USA', 'URY', 'UZB', 'VEN', 'VNM', 'YEM',
                'ZMB', 'ZWE')

euro_adopt <- list(
  'AUT' = 1999,
  'BEL' = 1999,
  'FIN' = 1999,
  'FRA' = 1999,
  'DEU' = 1999,
  'IRL' = 1999,
  'ITA' = 1999,
  'LUX' = 1999,
  'NLD' = 1999,
  'PRT' = 1999,
  'ESP' = 1999,
  'GRC' = 2001,
  'SVN' = 2007,
  'CYP' = 2008,
  'MLT' = 2008,
  'SVK' = 2009,
  'EST' = 2011,
  'LVA' = 2014,
  'LTU' = 2015
)


##########################################################################
#    (1)    Energy Balances - source IEA (https://www.oecd-ilibrary.org/energy/data/iea-world-energy-statistics-and-balances/world-energy-balances-edition-2023_4a0c7aae-en)
##########################################################################


# Read CSV file
wbal <- read.csv('data/iea/WBAL-2023.csv')

iea_flow_map <- readxl::read_excel('input_data/iea_to_nace.xlsx', sheet = 'flow_nace')


wbal$Value <- abs(wbal$Value)

# Filter wbal dataframe to only have KTOE and relevant flows
wbal <- wbal[wbal$UNIT == 'KTOE' & wbal$FLOW %in% iea_flow_map$flow & wbal$COUNTRY %in% cntry_list, ]


wbal_clean <- wbal %>% rename(cntry_code = COUNTRY,
                flow = FLOW,
                year = TIME,
                value = Value,
                product = PRODUCT,
                product_name = Product) %>% 
  select(cntry_code, flow, year, value, product, product_name)





### Create mapping between IEA flows and NACE sectors. This is performed manually
## on the base of the IEA World Energy Balances documentation (https://www.oecd-ilibrary.org/energy/world-energy-balances_25186442)

# Drop NA values
iea_flow_map <- iea_flow_map[complete.cases(iea_flow_map), ]
iea_flow_map$nace <- strsplit(iea_flow_map$nace, ',')
iea_flow_map <- iea_flow_map %>%
  unnest(c(nace))


### Create NACE level energy balances using the main flows
wbal_clean <- wbal_clean[!grepl("Memo", wbal_clean$product_name), ] 

list_var <- c('COAL','PEAT','CRNGFEED','TOTPRODS','NATGAS','NUCLEAR',
              'HYDRO','GEOTHERM','SOLWIND','COMRENEW',
              'HEATNS','ELECTR','HEAT','OILSHALE','TOTAL')

wbal_fin <- wbal_clean %>%
  pivot_wider(names_from = product, values_from = value) %>%
  left_join(iea_flow_map, by = "flow") %>% 
  group_by(cntry_code, year, nace) %>%
  summarise_at(list_var, sum, na.rm = TRUE)


# Compute the share of each energy component

for (col in list_var){
  wbal_fin[[paste0("share_", col)]] <- wbal_fin[[col]] / wbal_fin$TOTAL
}


write.csv(wbal_fin, "output_data/wbal_nace_R.csv", row.names = FALSE)


##########################################################################
#    (2)    Energy Prices index - Source (https://www.oecd-ilibrary.org/energy/data/iea-energy-prices-and-taxes-statistics/end-use-prices-energy-prices-in-national-currency-per-unit-edition-2023_b56880cb-en)
#NCEXTAX_TOE	Price excluding taxes (nat. cur./toe NCV) / NCPRICE_TOE	Total price (nat. cur./toe NCV
##########################################################################

# Import data
toe_prices_raw <- read.csv('data/iea/END_TOE-2023.csv') %>%
  rename(
    cntry_code = IEA_LOCATION,
    product = IEA_PRODUCT,
    sector = SECTOR,
    price = IEA_FLOW,
    year = TIME,
    value = Value
  )

prod_labels <- toe_prices_raw %>%
  select(product, Product) %>%
  distinct()


## Adjust region code
iso_regions <- read.csv('input_data/iso_region.csv') %>%
  rename(code3 = 'alpha.3') %>%
  select(code3, region, sub.region.code)


toe_prices <- toe_prices_raw %>%
  select(cntry_code, product, sector, price, year, value) %>%
  filter(!grepl("Q", year)) %>% 
  mutate(year = as.numeric(year))



# Import exchange rate data to convert everything in USD
exg_rate <- read.csv('data/macro/wb/NUS.FCRF.csv', skip = 4)
exg_rate <- reshape2::melt(exg_rate %>% select(Country.Code, names(exg_rate)[grep(paste0("^", 'X'), names(exg_rate))]), 
                           id.vars = 'Country.Code',
                           variable.name = 'year') %>% 
  filter(year !='X') %>% 
  rename(cntry_code = Country.Code, exg_rate = value) %>% 
  mutate(year = as.numeric(gsub('X','', year)))


## Since the energy data is in euro also for past observations, we take the latest available
## exchange rate (the date of euro adoption) and we backfill the exchange rate serie
for (cntry in names(euro_adopt)) {
  if (euro_adopt[[cntry]] == 1999) {
    exg_rate[exg_rate$cntry_code == cntry & exg_rate$year < euro_adopt[[cntry]], 'exg_rate'] <- NA
    exg_rate[exg_rate$cntry_code == cntry, 'exg_rate'] <- na.locf(exg_rate[exg_rate$cntry_code == cntry, 'exg_rate'], fromLast = TRUE)
  } else {
    exg_rate[exg_rate$cntry_code == cntry & exg_rate$year < euro_adopt[[cntry]], 'exg_rate'] <- exg_rate[exg_rate$cntry_code == 'AUT' & exg_rate$year < euro_adopt[[cntry]], 'exg_rate']
  }
}


toe_prices <- merge(toe_prices, exg_rate, by = c('cntry_code', 'year'), all.x = TRUE) %>% 
  mutate(value_usd = value/exg_rate)



toe_prices_wide <- toe_prices %>% filter(between(year, 1993, 2022)) %>%
  pivot_wider(id_cols = c('cntry_code', 'sector', 'year'), names_from = c('product', 'price'), values_from = value_usd)
  
names(toe_prices_wide) <- gsub("NCPRICE_TOE", "tot", names(toe_prices_wide))
names(toe_prices_wide) <- gsub("NCEXTAX_TOE", "extax", names(toe_prices_wide))


toe_prices_tot <- toe_prices_wide %>% 
  filter(sector == 'INDUSTRY') %>% 
  select(cntry_code, sector, year, ends_with('tot')) %>% 
  merge(iso_regions, by.x = 'cntry_code', 
        by.y = 'code3')

## Compute sub-region average of energy balances
toe_prices_tot_avg <- toe_prices_tot %>%
  group_by(year, sub.region.code) %>%
  
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  ungroup()

# Merge toe_prices_tot with toe_prices_tot_avg
toe_prices_tot <- merge(toe_prices_tot, toe_prices_tot_avg, by = c('year', 'sub.region.code'), suffixes = c('', '_avg'), all.x = TRUE)

for (c in grep("_tot$", names(toe_prices_wide), value = TRUE)) {
  toe_prices_tot[, c] <- ifelse(is.na(toe_prices_tot[, c]), toe_prices_tot[, paste0(c, "_avg")], toe_prices_tot[, c])
}


### Fix missing values for certain energy prices by proximity. Specifically, when missing
## either the value is backfilled/forwardfill or the value of the closest region is selected

var_list <- c('BITCOAL_tot', 'COKCOAL_tot','DIESEL_tot','HSFO_tot',  'LFO_tot', 'LPG_tot',
              'NATGAS_tot', 'ELECTR_tot')

## Fix South-America Nat Gas
toe_prices_tot$NATGAS_tot[toe_prices_tot$cntry_code == 'MEX'] <- ifelse(is.na(toe_prices_tot$NATGAS_tot[toe_prices_tot$cntry_code == 'MEX']),
       toe_prices_tot$NATGAS_tot[toe_prices_tot$cntry_code == 'USA'],
       toe_prices_tot$NATGAS_tot[toe_prices_tot$cntry_code == 'MEX'])

# Fix NATGAS_tot for CHL, COL, and CRI
for (c in c('CHL', 'COL', 'CRI')) {
  toe_prices_tot$NATGAS_tot[toe_prices_tot$cntry_code == c] <- ifelse(
    is.na(toe_prices_tot$NATGAS_tot[toe_prices_tot$cntry_code == c]),
    toe_prices_tot$NATGAS_tot[toe_prices_tot$cntry_code == 'MEX'],
    toe_prices_tot$NATGAS_tot[toe_prices_tot$cntry_code == c]
  )
}

# Fix BITCOAL_tot for ESP, GRC, ITA, PRT, and SVN
for (c in c('ESP', 'GRC', 'ITA', 'PRT', 'SVN')) {
  toe_prices_tot$BITCOAL_tot[toe_prices_tot$cntry_code == c] <- ifelse(
    is.na(toe_prices_tot$BITCOAL_tot[toe_prices_tot$cntry_code == c]),
    toe_prices_tot$BITCOAL_tot_avg[toe_prices_tot$cntry_code == 'AUT'],
    toe_prices_tot$BITCOAL_tot[toe_prices_tot$cntry_code == c]
  )
}

# Fix BITCOAL_tot for AUS, CHL, COL, CRI, MEX, and NZL
for (c in c('AUS', 'CHL', 'COL', 'CRI', 'MEX', 'NZL')) {
  toe_prices_tot$BITCOAL_tot[toe_prices_tot$cntry_code == c] <- ifelse(
    is.na(toe_prices_tot$BITCOAL_tot[toe_prices_tot$cntry_code == c]),
    toe_prices_tot$BITCOAL_tot_avg[toe_prices_tot$cntry_code == 'USA'],
    toe_prices_tot$BITCOAL_tot[toe_prices_tot$cntry_code == c]
  )
}

# Fix COKCOAL_tot for ESP, GRC, ITA, PRT, and SVN
for (c in c('ESP', 'GRC', 'ITA', 'PRT', 'SVN')) {
  toe_prices_tot$COKCOAL_tot[toe_prices_tot$cntry_code == c] <- ifelse(
    is.na(toe_prices_tot$COKCOAL_tot[toe_prices_tot$cntry_code == c]),
    toe_prices_tot$COKCOAL_tot_avg[toe_prices_tot$cntry_code == 'AUT'],
    toe_prices_tot$COKCOAL_tot[toe_prices_tot$cntry_code == c]
  )
}

# Fix COKCOAL_tot for AUS, CHL, COL, CRI, MEX, and NZL
for (c in c('AUS', 'CHL', 'COL', 'CRI', 'MEX', 'NZL')) {
  toe_prices_tot$COKCOAL_tot[toe_prices_tot$cntry_code == c] <- ifelse(
    is.na(toe_prices_tot$COKCOAL_tot[toe_prices_tot$cntry_code == c]),
    toe_prices_tot$COKCOAL_tot_avg[toe_prices_tot$cntry_code == 'USA'],
    toe_prices_tot$COKCOAL_tot[toe_prices_tot$cntry_code == c]
  )
}

toe_prices_tot$DIESEL_tot[toe_prices_tot$cntry_code == 'JPN'] <- na.locf(toe_prices_tot$DIESEL_tot[toe_prices_tot$cntry_code == 'JPN'],  fromLast = TRUE)
toe_prices_tot$DIESEL_tot[toe_prices_tot$cntry_code == 'KOR'] <- na.locf(toe_prices_tot$DIESEL_tot[toe_prices_tot$cntry_code == 'KOR'],  fromLast = TRUE)

for (c in c('AUT', 'BEL', 'CHE', 'DEU', 'ESP', 'FRA', 
            'GRC', 'ITA', 'LUX', 'NLD', 'PRT', 'SVN')) {
  toe_prices_tot$HSFO_tot[toe_prices_tot$cntry_code == c] <- ifelse(
    is.na(toe_prices_tot$HSFO_tot[toe_prices_tot$cntry_code == c]),
    toe_prices_tot$HSFO_tot_avg[toe_prices_tot$cntry_code == 'HUN'],
    toe_prices_tot$HSFO_tot[toe_prices_tot$cntry_code == c]
  )
}

toe_prices_tot$HSFO_tot[toe_prices_tot$cntry_code == 'AUS'] <- na.locf(toe_prices_tot$HSFO_tot[toe_prices_tot$cntry_code == 'AUS'])
toe_prices_tot$HSFO_tot[toe_prices_tot$cntry_code == 'NZL'] <- na.locf(toe_prices_tot$HSFO_tot[toe_prices_tot$cntry_code == 'NZL'])


for (c in c('AUS', 'CHL', 'COL', 'CRI', 'MEX', 'NZL')){
  toe_prices_tot$LFO_tot[toe_prices_tot$cntry_code == c] <- na.locf(toe_prices_tot$LFO_tot[toe_prices_tot$cntry_code == c])
  
  
}



## Compute COAL and OIL price indexes as mean of relevant energy component.

toe_prices_tot$COAL_price_tot <- rowMeans(subset(toe_prices_tot, select = c('BITCOAL_tot', 'COKCOAL_tot')), na.rm = TRUE)
toe_prices_tot$TOTPRODS_price_tot <- rowMeans(subset(toe_prices_tot, select = c('DIESEL_tot', 'HSFO_tot', 'LFO_tot')), na.rm = TRUE)



### 3 - Finalize the dataset in line with KELMS Structure 


## Put energy balances and prices together 


klems_cntry <- c('AT', 'FR', 'JP', 'DE', 'NL', 'SI', 'UK', 'HR', 'BG', 'LT',
                 'PL', 'ES', 'SK',  'HU', 'LU', 'DK', 'LV', 'PT', 
                 'EE', 'US',  'IE', 'RO', 'FI', 'IT',
                 'MT', 'SE', 'CZ', 'GR', 'CY',  'BE')



iso_regions <- read.csv('input_data/iso_region.csv') %>%
  rename(code3 = 'alpha.3',
         code2 = 'alpha.2') %>% select(code2, code3)



wbal_fin <- read.csv('output_data/wbal_nace_R.csv') %>% filter(between(year, 1993, 2021), !is.na(nace)) %>% 
  merge(iso_regions, by.x = 'cntry_code', by.y = 'code3', all.x = TRUE)


klems_energy <- wbal_fin %>% filter(code2 %in% klems_cntry ) %>% 
  group_by(code2, year) %>% 
  summarise_at(c('COAL', 'TOTPRODS', 'NATGAS', 'ELECTR'), sum, na.rm = TRUE) 
klems_energy$TOTAL <- rowSums(klems_energy[c('COAL', 'TOTPRODS', 'NATGAS', 'ELECTR')], na.rm = TRUE)


## The country-level energy price index is computed by combining IEA energy quantities 
# and the IEA prices as the weighted average where weights are the country'energy mix.

for (c in c('COAL','TOTPRODS','NATGAS','ELECTR')){
  
  klems_energy[[paste0(c, '_share')]] <- klems_energy[[c]] / klems_energy$TOTAL
}



toe_prices_tot <- toe_prices_tot %>% merge(iso_regions, by.x = 'cntry_code', 
                         by.y = 'code3', all.x = TRUE)

klems_energy <- klems_energy %>% merge(toe_prices_tot %>% select(year, code2,
                                                 COAL_price_tot, TOTPRODS_price_tot, 
                                                 NATGAS_tot, ELECTR_tot), by = c('year', 'code2'), all.x =  TRUE)


klems_energy$energy_price_index <- with(klems_energy, 
                                        COAL_share * COAL_price_tot + 
                                          TOTPRODS_share * TOTPRODS_price_tot +
                                          NATGAS_share * NATGAS_tot +
                                          ELECTR_share * ELECTR_tot)
klems_energy_price = klems_energy %>% select(year, code2, energy_price_index)

eu_country_codes <- c('AT', 'BE', 'BG', 'CY', 'CZ', 'DE', 'DK', 'EE', 'ES', 'FI', 'FR',
                      'HR', 'HU', 'GR', 'IE', 'IT', 'LT', 'LU', 'LV', 'MT', 'NL', 'PL',
                      'PT', 'RO', 'SE', 'SI', 'SK')

# Compute EU average


klems_energy_price_eu <- klems_energy_price %>%
  filter(code2 %in% eu_country_codes) %>%
  group_by(year) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)

klems_energy_price <- merge(klems_energy_price, klems_energy_price_eu, 
                            by.x = 'year', 
                            by.y = 'year',
                            all.x = TRUE, suffixes = c('', '_ea'))


klems_energy_price$energy_price_index <- ifelse(
  is.na(klems_energy_price$energy_price_index),
  klems_energy_price$energy_price_index_ea,
  klems_energy_price$energy_price_index
)


klems_energy_fin = wbal_fin %>% filter(code2 %in% klems_cntry) %>% 
  group_by(code2, year, nace) %>% 
  summarise_at(vars('TOTAL'), sum, na.rm = TRUE) %>% 
  merge(klems_energy_price %>% select(-energy_price_index_ea),
        by = c('code2', 'year'), all.x =  TRUE)


klems_energy_fin$ENERGY <- klems_energy_fin$TOTAL*1000* klems_energy_fin$energy_price_index/1000000

## The final dataset contains three main columns:

# TOTAL is the KTOE of energy used by each sector.
# energy_price_index Energy price index represented as USD/toe)
# ENERGY: is the total cost of energy expressed in million USD


write.csv(klems_energy_fin, "output_data/klems_energy_fin_R.csv", row.names = FALSE)

