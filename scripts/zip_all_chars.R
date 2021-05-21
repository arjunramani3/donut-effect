#####Zip all chars with census cbds#####

#####zip_all_chars.R#####
#WFH exposure from lodes
#pop density from splitwise
#CBDs from wikipedia
rm(list=ls())
library(tidyverse)


#Get population density of zip codes
#https://www.census.gov/programs-surveys/geography/technical-documentation/records-layout/2010-zcta-record-layout.html#par_textimage_5
#https://www2.census.gov/geo/docs/maps-data/data/rel/
dens <- read_csv('https://www2.census.gov/geo/docs/maps-data/data/rel/zcta_county_rel_10.txt') %>%
  rename(zip = ZCTA5, county = COUNTY, state = STATE, `2010 Population` = ZPOP, 
         area = ZAREA, land_area = ZAREALAND) %>%
  select(zip, county, state, `2010 Population`, area, land_area) %>%
  distinct(zip, .keep_all = TRUE) %>%
  mutate(area = area/2.59e6, land_area = land_area/2.59e6,
         density = `2010 Population`/land_area,
         zip = as.integer(zip))

#Get map of zipcodes to CBSA id numbers
zip_cbsa <- read_csv('https://www2.census.gov/geo/docs/maps-data/data/rel/zcta_cbsa_rel_10.txt') %>%
  select(ZCTA5, CBSA) %>%
  rename(zip = ZCTA5) %>% 
  distinct(zip, .keep_all = TRUE) %>%
  mutate(zip = as.integer(zip))

## Get Names of CBSA id numbers to names
cbsa_name <- read_csv('./data/external_data/nhgis/nhgis0003_ds244_20195_2019_cbsa.csv') %>%
  select(CBSAA, NAME_E, ALUBE001) %>%
  rename(MsaName = NAME_E, CBSA = CBSAA, `2019 CBSA Population` = ALUBE001) %>%
  separate(MsaName, c("Metro", "end"), ', ', remove = FALSE) %>%
  mutate(Metro = sub("-.*", "", Metro),
         MetroState = sub(" .*", "", end),
         MetroState = sub("-.*", "", MetroState),
         MetroShort = paste(Metro, MetroState, sep = ', ')) %>%
  select(CBSA, MsaName, MetroShort)

## GET old CBSA id numbers for 2003 (since Zillow uses old id's for certain metros)
#https://cps.ipums.org/cps/codes/metfips_2014onward_codes.shtml
cbsa_2003 <- read_csv('./data/external_data/CBSA_2003.csv') %>%
  separate(MsaName, c("Metro", "end"), ', ', remove = FALSE) %>%
  mutate(Metro = sub("-.*", "", Metro),
         MetroState = sub(" .*", "", end),
         MetroState = sub("-.*", "", MetroState),
         MetroShort = paste(Metro, MetroState, sep = ', '),
         population = NA) %>%
  select(CBSA, MsaName, MetroShort)

cbsa_name = rbind(cbsa_name, cbsa_2003) %>%
  distinct(CBSA, .keep_all = TRUE)
  
  #Get population from NHGIS
#Geography = ZCTA, Variable = Population, Data = 2015-2019 5-Year ACS
pop <- read_csv('./data/external_data/nhgis/nhgis0003_ds244_20195_2019_zcta.csv') %>%
  select(ZCTA5A, ALUBE001, ALUBM001) %>%
  rename(zip = ZCTA5A, `2019 Population` = ALUBE001, MOE_2019 = ALUBM001) %>%
  mutate(zip = as.integer(zip))


#Get wfh exposure data
wfh <- read_csv('./data/lodes_rac_wfh_zip_exposure.csv') %>%
  distinct(zip, .keep_all = TRUE) %>%
  mutate(zip = as.integer(zip))

#Get price level data
price <- read_csv('http://files.zillowstatic.com/research/public_v2/zhvi/Zip_zhvi_uc_sfr_tier_0.33_0.67_sm_sa_mon.csv', col_types = cols(RegionName = col_double())) %>% 
  rename(zip = 'RegionName') %>%
  select(!c(RegionID, SizeRank, RegionType, StateName, State, City, Metro, CountyName)) %>% 
  pivot_longer(!zip, names_to = 'date', values_to = 'zhvi') %>%
  filter(date >= as.Date('2019-01-01'), date < '2020-01-01') %>% 
  group_by(zip) %>%
  summarise(price_level = mean(zhvi, na.rm = TRUE))

#zip code business patterns
zbp <- read_csv('./data/zbp_wfh.csv') %>% 
  select(zip, estab_count) %>% mutate(zip = as.double(zip))

#merge
wfh_exp <- dens %>%
  left_join(zip_cbsa, by = 'zip') %>%
  left_join(cbsa_name, by = 'CBSA') %>%
  left_join(pop, by = 'zip') %>%
  left_join(wfh, on = 'zip') %>%
  left_join(price, on = 'zip') %>%
  left_join(zbp, on = 'zip') %>%
  mutate(density2019 = `2019 Population`/land_area)

write_csv(wfh_exp, './data/zip_all_chars.csv')

