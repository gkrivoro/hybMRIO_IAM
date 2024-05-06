# Load required libraries
library(dplyr)
library(readr)
library(tidyr)
library(readxl)
library(plyr)


# Set display options
options(max.print=500)

# Define energy types dictionary
energy_type_eurostat <- c(
  "TOTAL",
  "C0000X0350-0370",
  "C0350-0370",
  "P1000",
  "S2000",
  "O4000XBIO",
  "G3000",
  "RA000",
  "W6100_6220",
  "N900H",
  "H8000",
  "E7000"
)


energy_type_eurostat_label <- c(
  "total",
  "solid_ff",
  "manufactured_gases",
  "peat",
  "oil_shale",
  "oil_products",
  "natural_gas",
  "renewables",
  "nonrenewable_waste",
  "nuclear_heat",
  "heat",
  "electricity"
)

# Define EU KLEMS countries
euklems_cntry <- c(
  "AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "GR", "ES",
  "FI", "FR", "HR", "HU", "IE", "IT", "JP", "LT", "LU", "LV",
  "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK", "UK", "US"
)


#Set working directory here. This would include the functions folder. 
path_wd <- "C:/Users/boldrin/Sectoral Granularity/repo/" 
setwd(path_wd)






# Load the Excel conversion  file
eurostat_nace <- read_excel("input_data/eurostat_to_nace.xlsx")
eurostat_nace <- eurostat_nace %>% filter(!is.na(nace))
eurostat_nace <- eurostat_nace %>% separate_rows(nace, sep = ",")



### Load and clean Eurostat energy balances - Source: # https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/nrg_bal_s?format=TSV&compressed=false


estat_nrg_bal <- read_tsv('data/eurostat/estat_nrg_bal_s.tsv')


colnames(estat_nrg_bal) <- c('index', 1990:2022)
estat_nrg_bal <- estat_nrg_bal %>% 
  separate(col = index, into = c('freq', 'nrg_bal', 'siec', 'unit', 'cntry_code'), sep = ',', extra = 'merge')


estat_nrg_bal$cntry_code <- ifelse(estat_nrg_bal$cntry_code == 'EL', 'GR', estat_nrg_bal$cntry_code)

estat_nrg_bal <- estat_nrg_bal %>% 
  filter(unit == 'KTOE')

estat_nrg_bal$siec_txt <-  mapvalues(estat_nrg_bal$siec, from = energy_type_eurostat, 
                                     to =  energy_type_eurostat_label)
  
  
estat_nrg_bal <- estat_nrg_bal %>% 
  select(-freq)

estat_nrg_bal <- estat_nrg_bal %>% 
  filter(nrg_bal %in% unique(eurostat_nace$code))
estat_nrg_bal[estat_nrg_bal == ': z' | estat_nrg_bal == ': '| estat_nrg_bal == ':'] <- NA
estat_nrg_bal <- estat_nrg_bal %>% relocate(siec_txt, .after = cntry_code)

estat_nrg_bal[, 6:38] <- lapply(estat_nrg_bal[, 6:38], as.numeric)


## Convert the Eurostat sectors to NACE

nace_estat_nrg_bal <- merge(estat_nrg_bal, eurostat_nace, by.x = 'nrg_bal', by.y = 'code', all.x = TRUE)
nace_estat_nrg_bal <- nace_estat_nrg_bal %>% relocate(c(label,nace), .after = cntry_code) %>% as_tibble()

nace_estat_nrg_bal <- nace_estat_nrg_bal %>%
  group_by(cntry_code, nace, siec_txt) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) 

nace_estat_nrg_bal <- pivot_longer(nace_estat_nrg_bal, cols = 4:36, names_to = 'year', values_to = 'value')
nace_estat_nrg_bal <- pivot_wider(nace_estat_nrg_bal, names_from = siec_txt, values_from = value)


for (c in c('electricity', 'natural_gas', 'oil_products')) {
  nace_estat_nrg_bal[[paste0('share_', c)]] <- nace_estat_nrg_bal[[c]] / nace_estat_nrg_bal$total
}


nace_estat_nrg_bal$tot_price_index <- rowSums(nace_estat_nrg_bal[, c('electricity', 'natural_gas', 'oil_products')], na.rm = TRUE)

for (c in c('electricity', 'natural_gas', 'oil_products')) {
  nace_estat_nrg_bal[[paste0('share_price_index_', c)]] <- nace_estat_nrg_bal[[c]] / nace_estat_nrg_bal$tot_price_index
}


bal_cols <- c('cntry_code', 'nace', 'year', 'total', 'share_price_index_electricity', 'share_price_index_natural_gas', 'share_price_index_oil_products')

nace_estat_nrg_bal_final <- nace_estat_nrg_bal %>%
  filter(cntry_code %in% euklems_cntry) %>%
  select(all_of(bal_cols)) %>%
  arrange(cntry_code, year, nace)



### Compute price index using electricity, natural gas and oil prices

### Electricity price post 2007

ele_post_2007 <- read_tsv('data/eurostat/nrg_pc_205_tabular.tsv')

ele_post_2007 <- ele_post_2007 %>%
  separate(col = 1, into = c('freq','prod','nrg_cons','unit','tax', 'currency','geo'), sep = ',', extra = 'merge')


ele_post_2007 <- ele_post_2007 %>%
  select(-c('freq', 'prod'))

ele_post_2007[ele_post_2007 == ': z' | ele_post_2007 == ': ' | ele_post_2007 == ': c' | ele_post_2007 == ': u' | ele_post_2007 == ':' ] <- NA

ele_post_2007[, grep('-S', colnames(ele_post_2007))] <- lapply(ele_post_2007[, grep('-S', colnames(ele_post_2007))], function(x) as.numeric(gsub('[pedu]', '', x)))


ele_post_2007 <- ele_post_2007 %>%
  filter(currency == 'EUR') %>%
  group_by(geo, unit, tax) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  ungroup()

ele_post_2007 <- ele_post_2007 %>%
  pivot_longer(cols = contains("-S"), names_to = "year", values_to = "value") %>%
  separate(year, into = c("year", "sem"), sep = "-") %>%
  group_by(geo, unit, tax, year) %>%
  summarise_at(c("value"), mean, na.rm = TRUE) %>% 
  ungroup() %>%
  mutate(year = as.numeric(year))

### Electricity price pre 2007

ele_pre_2007 <- read_tsv('data/eurostat/nrg_pc_205_h_tabular.tsv')

ele_pre_2007 <- ele_pre_2007 %>%
  separate(col = 1, into = c('freq','prod','nrg_cons','unit','tax', 'currency','geo'), sep = ',', extra = 'merge')


ele_pre_2007 <- ele_pre_2007 %>%
  select(-c('freq', 'prod'))

ele_pre_2007[ele_pre_2007 == ': z' | ele_pre_2007 == ': ' | ele_pre_2007 == ': c' | ele_pre_2007 == ': u' | ele_pre_2007 == ':' ] <- NA

ele_pre_2007[, grep('-S', colnames(ele_pre_2007))] <- lapply(ele_pre_2007[, grep('-S', colnames(ele_pre_2007))], function(x) as.numeric(gsub('[pedu]', '', x)))


ele_pre_2007 <- ele_pre_2007 %>%
  filter(currency == 'EUR') %>%
  group_by(geo, unit, tax) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  ungroup()

ele_pre_2007 <- ele_pre_2007 %>%
  pivot_longer(cols = contains("-S"), names_to = "year", values_to = "value") %>%
  separate(year, into = c("year", "sem"), sep = "-") %>%
  group_by(geo, unit, tax, year) %>%
  summarise_at(c("value"), mean, na.rm = TRUE) %>% 
  ungroup() %>%
  mutate(year = as.numeric(year)) %>%
  filter(year < 2007)

ele_price <- bind_rows(ele_post_2007, ele_pre_2007)
ele_price$geo <- ifelse(ele_price$geo == 'EL', 'GR', ele_price$geo)



### Gas price post 2007

gas_post_2007 <- read_tsv('data/eurostat/nrg_pc_203_tabular.tsv')
gas_post_2007 <- gas_post_2007 %>%
  separate(col = 1, into = c('freq','prod','nrg_cons','unit','tax', 'currency','geo'), sep = ',', extra = 'merge')

gas_post_2007 <- gas_post_2007 %>%
  select(-c('freq', 'prod'))

gas_post_2007[gas_post_2007 == ': z' | gas_post_2007 == ': ' | gas_post_2007 == ': c' | gas_post_2007 == ': u' | gas_post_2007 == ':' ] <- NA

gas_post_2007[, grep('-S', colnames(gas_post_2007))] <- lapply(gas_post_2007[, grep('-S', colnames(gas_post_2007))], function(x) as.numeric(gsub('[pedu]', '', x)))

gas_post_2007 <- gas_post_2007 %>%
  filter(currency == 'EUR') %>%
  group_by(geo, unit, tax) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  ungroup()

gas_post_2007 <- gas_post_2007 %>%
  pivot_longer(cols = contains("-S"), names_to = "year", values_to = "value") %>%
  separate(year, into = c("year", "sem"), sep = "-") %>%
  group_by(geo, unit, tax, year) %>%
  summarise_at(c("value"), mean, na.rm = TRUE) %>% 
  ungroup() %>%
  mutate(year = as.numeric(year))

### Gas price pre 2007

gas_pre_2007 <- read_tsv('data/eurostat/nrg_pc_203_h_tabular.tsv')
gas_pre_2007 <- gas_pre_2007 %>%
  separate(col = 1, into = c('freq','prod','nrg_cons','unit','tax', 'currency','geo'), sep = ',', extra = 'merge')

gas_pre_2007 <- gas_pre_2007 %>%
  select(-c('freq', 'prod'))

gas_pre_2007[gas_pre_2007 == ': z' | gas_pre_2007 == ': ' | gas_pre_2007 == ': c' | gas_pre_2007 == ': u' | gas_pre_2007 == ':' ] <- NA

gas_pre_2007[, grep('-S', colnames(gas_pre_2007))] <- lapply(gas_pre_2007[, grep('-S', colnames(gas_pre_2007))], function(x) as.numeric(gsub('[pedu]', '', x)))

gas_pre_2007 <- gas_pre_2007 %>%
  filter(currency == 'EUR') %>%
  group_by(geo, unit, tax) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  ungroup()

gas_pre_2007 <- gas_pre_2007 %>%
  pivot_longer(cols = contains("-S"), names_to = "year", values_to = "value") %>%
  separate(year, into = c("year", "sem"), sep = "-") %>%
  group_by(geo, unit, tax, year) %>%
  summarise_at(c("value"), mean, na.rm = TRUE) %>% 
  ungroup() %>%
  mutate(year = as.numeric(year)) %>%
  filter(year < 2007)

gas_price <- bind_rows(gas_post_2007, gas_pre_2007)
gas_price$geo <- ifelse(gas_price$geo == 'EL', 'GR', gas_price$geo)



gas_tot <- gas_price %>%
  filter(unit == 'GJ_GCV' & tax == 'I_TAX' & year >= 1991) %>%
  pivot_wider(names_from = 'geo', values_from = 'value')

eu15 <- c('AT', 'BE', 'DE', 'FI', 'FR', 'DE', 'GR', 'IE', 'IT', 'LU', 'NL', 'PT', 'ES', 'SE', 'UK')

column_names <- colnames(gas_tot)
column_names_2_chars <- column_names[nchar(column_names) == 2]

for (c in eu15) {
  gas_tot[[c]] <- coalesce(gas_tot[[c]], gas_tot$EU15)
}

for (c in names(gas_tot)[-3:0]) {
  gas_tot[[c]] <- coalesce(gas_tot[[c]], rowMeans(gas_tot[column_names_2_chars], na.rm = TRUE))
}

### Electricity price

ele_tot <- ele_price %>%
  filter(unit == 'KWH' & tax == 'I_TAX' & year >= 1991) %>%
  pivot_wider(names_from = 'geo', values_from = 'value')


for (c in eu15) {
  ele_tot[[c]] <- coalesce(ele_tot[[c]], ele_tot$EU15)
}

for (c in names(ele_tot)[-3:0]) {
  ele_tot[[c]] <- coalesce(ele_tot[[c]], rowMeans(ele_tot[column_names_2_chars], na.rm = TRUE))
}

gas_tot <- gas_tot %>% arrange(year)
ele_tot <- ele_tot %>% arrange(year)


### Oil prices
oil_prods <- c('LPG', 'diesel', 'euro95', 'heating_oil')
col_names_oil  <- array(read_excel('data/eurostat/Weekly_Oil_Bulletin_Prices_History_maticni_4web.xlsx', n_max = 1, sheet = 'Prices with taxes',  col_names = FALSE))
oil_bullettin_prices <- read_excel('data/eurostat/Weekly_Oil_Bulletin_Prices_History_maticni_4web.xlsx', skip = 2, sheet = 'Prices with taxes')

colnames(oil_bullettin_prices) <- col_names_oil

oil_bullettin_prices <- oil_bullettin_prices %>%
  select(-starts_with('CTR'))

colnames(oil_bullettin_prices)[1] <- 'date'

oil_bullettin_prices <- oil_bullettin_prices %>%
  filter(!is.na(date))
oil_bullettin_prices <- oil_bullettin_prices %>%
  filter(date != 'Notes:')

oil_bullettin_prices$year <- as.integer(format(as.Date(as.numeric(oil_bullettin_prices$date), origin = "1899-12-30"), "%Y"))

oil_bullettin_prices <- oil_bullettin_prices %>%
  select(-date) %>% 
  group_by(year) %>%
  summarise_at(vars(-group_cols()), mean, na.rm = TRUE) %>%
  ungroup()

oil_bullettin_prices <- oil_bullettin_prices %>%
  pivot_longer(cols = -year, names_to = 'variable', values_to = 'value')

oil_bullettin_prices <- oil_bullettin_prices %>%
  separate(variable, into = c('cntry', 'oil_type'), sep = '_', extra ='merge')


oil_bullettin_prices$oil_type <- gsub('price_with_tax_', '', oil_bullettin_prices$oil_type)

oil_bullettin_prices$oil_type <- ifelse(grepl('^he', oil_bullettin_prices$oil_type), 'heating_oil', oil_bullettin_prices$oil_type)

# Filter for 'diesel' and 'euro95' oil types
oil_bullettin_prices <- oil_bullettin_prices %>%
  filter(oil_type %in% c('diesel', 'euro95'))


#https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Glossary:Tonnes_of_oil_equivalent_(toe)
#https://ec.europa.eu/eurostat/documents/38154/4956218/ENERGY-BALANCE-GUIDE-DRAFT-31JANUARY2019.pdf/cf121393-919f-4b84-9059-cdf0f69ec045

oil_conversion <- list(
  diesel = 1.01,
  euro95 = 1.05
)


oil_bullettin_prices$toe_conv <- sapply(oil_bullettin_prices$oil_type, function(x) oil_conversion[[x]])
oil_bullettin_prices$value <- oil_bullettin_prices$value / oil_bullettin_prices$toe_conv


gas_tot[-3:0] <- gas_tot[-3:0] * 1000 * 41868 / 1000000
ele_tot[-3:0] <- ele_tot[-3:0] * 1000000 * 11630 / 1000000

##  Compute final values 

gas_tot <- gas_tot %>%
  select(-c('unit', 'tax')) %>% 
  pivot_longer(cols = -year, names_to = 'cntry_code', values_to = 'natgas_price') %>% 
  arrange(cntry_code, year)

ele_tot <- ele_tot %>%
  select(-c('unit', 'tax')) %>% 
  pivot_longer(cols = -year, names_to = 'cntry_code', values_to = 'electr_price') %>% 
  arrange(cntry_code, year)

oil_tot <- oil_bullettin_prices %>%
  pivot_wider(id_cols = c('year', 'cntry'), names_from = 'oil_type', values_from = 'value') %>% 
  mutate(cntry_code = cntry) %>% 
  select(cntry_code, year, euro95, diesel)

oil_tot$oil_price <- rowMeans(oil_tot[c('diesel', 'euro95')], na.rm = TRUE)

oil_tot_expanded <- expand_grid(cntry_code = unique(oil_tot$cntry_code), year = 1991:2004) %>%
  bind_rows(oil_tot) %>%
  group_by(cntry_code) %>%
  arrange(cntry_code, year) %>% 
  fill(diesel, euro95, oil_price, .direction ='updown')

## Convert to USD

exg_rate <- read.csv('data/macro/wb/NUS.FCRF.csv', skip = 4, header = TRUE) %>% select(-X)

exg_rate <- exg_rate %>% 
  select(-c("Country.Name","Indicator.Name","Indicator.Code" )) %>% 
  pivot_longer(cols = -Country.Code, names_to = 'year', values_to = 'exg_rate')

exg_rate$year <- as.numeric(gsub('X', '', exg_rate$year))

eur_usd <- subset(exg_rate, Country.Code == 'ITA')
eur_usd <- eur_usd[, c('year', 'exg_rate')]
eur_usd$exg_rate[eur_usd$year <= 1998] <- NA
eur_usd <- eur_usd %>% fill(exg_rate, .direction = ('updown'))


### Compute price index

price_index_shares <- nace_estat_nrg_bal_final %>%
  group_by(cntry_code, year) %>%
  summarise_at(vars(starts_with('share_price_index_')), mean, na.rm = TRUE) %>%
  ungroup()

price_index_shares$year <- as.numeric(price_index_shares$year)


price_index <- price_index_shares %>%
  left_join(gas_tot, by = c('cntry_code', 'year')) %>%
  left_join(ele_tot, by = c('cntry_code', 'year')) %>%
  left_join(oil_tot, by = c('cntry_code', 'year')) %>%
  left_join(eur_usd, by = 'year')

price_index$natgas_price <- price_index$natgas_price / price_index$exg_rate
price_index$electr_price <- price_index$electr_price / price_index$exg_rate
price_index$oil_price <- price_index$oil_price / price_index$exg_rate


price_index$energy_price_index_estat <- ifelse(
  is.na(price_index$oil_price),
  price_index$share_price_index_electricity * price_index$electr_price +
    (price_index$share_price_index_natural_gas + price_index$share_price_index_oil_products) * price_index$natgas_price,
  price_index$share_price_index_electricity * price_index$electr_price +
    price_index$share_price_index_natural_gas * price_index$natgas_price +
    price_index$share_price_index_oil_products * price_index$oil_price
)


price_index_eu <- price_index %>%
  filter(cntry_code %in% c('AT', 'BE', 'BG', 'CY', 'CZ', 'DE', 'DK', 'EE', 'ES', 'FI', 'FR',
                           'HR', 'HU', 'GR', 'IE', 'IT', 'LT', 'LU', 'LV', 'MT', 'NL', 'PL',
                           'PT', 'RO', 'SE', 'SI', 'SK')) %>%
  group_by(year) %>%
  summarise(across(starts_with('share_price_index_'), mean, na.rm = TRUE)) %>%
  ungroup()


price_index <- price_index %>%
  left_join(price_index_eu, by = 'year', suffix = c('', '_eu')) %>%
  mutate(
    energy_price_index_estat = coalesce(energy_price_index_estat, energy_price_index_estat_eu)
  ) %>%
  select(-energy_price_index_estat_eu)


nrg_bal_final <- nace_estat_nrg_bal_final %>%
  left_join(price_index %>% select(cntry_code, year, energy_price_index_estat), by = c('cntry_code', 'year')) %>%
  mutate(
    ENERGY = total * 1000 * energy_price_index_estat / 1000000
  )

write.csv(nrg_bal_final, file = 'output_data/klems_energy_fin_eurostat_R.csv', row.names = FALSE)













