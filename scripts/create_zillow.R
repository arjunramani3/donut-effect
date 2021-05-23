###########################################
# create_zillow_panels.R
# This script reads in data from Zillow, USPS, and Census and creates
# panel data to be read in for for analysis
###########################################

## Preliminaries
rm(list=ls())

## This function will check if a package is installed, and if not, install it
pkgTest <- function(x) {
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE);
    if(!require(x, character.only = TRUE)) stop("Package not found")
  }
}

## These lines load the required packages
packages <- c("tidyverse", "zoo")
lapply(packages, pkgTest);

## Define top twelve metro areas (short forms)
cities <- c('San Francisco, CA', 'New York, NY', 'Chicago, IL', 'Boston, MA',
            'Los Angeles, CA', 'Washington, DC', 'Atlanta, GA', 'Miami, FL',
            'Philadelphia, PA', 'Dallas, TX', 'Houston, TX', 'Phoenix, AZ')

## Read in Zillow Home Value Index data
df <- read_csv('http://files.zillowstatic.com/research/public_v2/zhvi/Zip_zhvi_uc_sfr_tier_0.33_0.67_sm_sa_mon.csv', col_types = cols(RegionName = col_double()))

## Read in zipcode level characteristics
chars <- read_csv('./data/zip_all_chars_cbd.csv')

## Tidy zillow data from wide to long
df2 <- df %>% rename(zip = 'RegionName', MsaShort = 'Metro') %>%
  mutate(MetroShort = sub("-.*", "", MsaShort),
         MetroShort = paste(MetroShort, State, sep = ', ')) %>%
  select(-c(RegionID, SizeRank, RegionType, StateName, State, City, CountyName, MsaShort)) %>%
  pivot_longer(!c(zip, MetroShort), names_to = 'date', values_to = 'zhvi')

## filter to values past Jan 2017
df3 <- df2 %>% filter(date > as.Date('2017-01-01'))

## Get panel for pre-period
df5 <- df3 %>% mutate(date = as.Date(date)) %>%
  group_by(zip) %>% mutate(
    zhvi = na.approx(zhvi, na.rm=FALSE, rule = 2),
    pre_pct_change = (zhvi - lag(zhvi, 12))/((zhvi + lag(zhvi, 12))/2)*100
  ) %>% filter(date >= as.Date('2020-02-01'), date < as.Date('2021-03-01')) %>%
  group_by(zip) %>% summarise(pre_pct_change = mean(pre_pct_change, na.rm = TRUE)) %>%
  inner_join(chars, by = 'zip') %>%
  filter(!is.na(wfh_emp), !is.na(log(density2019)), !is.infinite(log(density2019)),
         land_area > .1, `2019 Population` > 100)

## Get panel for post-period
df4 <- df3 %>% mutate(date = as.Date(date)) %>%
  group_by(zip) %>% mutate(
    zhvi = na.approx(zhvi, na.rm=FALSE, rule = 2),
    post_pct_change = (zhvi - lag(zhvi, 12))/((zhvi + lag(zhvi, 12))/2)*100
  ) %>% filter(date >= as.Date('2021-02-01'), date < as.Date('2021-03-01')) %>%
  group_by(zip) %>% summarise(post_pct_change = mean(post_pct_change, na.rm = TRUE)) %>%
  inner_join(chars, by = 'zip') %>%
  filter(!is.na(wfh_emp), !is.na(log(density2019)), !is.infinite(log(density2019)),
         land_area > .1, `2019 Population` > 100)

## Merge, filter, and save (all metros)
df6 <- df4 %>% select(zip, post_pct_change) %>% inner_join(df5, by = 'zip') %>%
  filter(!is.na(dist_to_cbd), !is.na(log(density2019)), !is.infinite(log(density2019))) %>% 
  write_csv('./data/zhvi_panel_zips.csv')

## Write to csv (top 12 metros)
df6 %>% filter(MetroShort %in% cities) %>%
  write_csv('./data/zhvi_panel_zips_top12.csv')


###########################################
# This section of the script creates the 
# MSA level home value index pct change file
###########################################

## Read in home value index dataset
df <- read_csv('http://files.zillowstatic.com/research/public_v2/zhvi/Metro_zhvi_uc_sfr_tier_0.33_0.67_sm_sa_mon.csv') %>%
  rename(MetroShort = RegionName) %>%
  mutate(MetroShort = sub(",.*", "", MetroShort),
         MetroShort = sub("-.*", "", MetroShort),
         MetroShort = paste(MetroShort, StateName, sep = ', ')) 

## Read in MSA-level characteristics
msa_chars <- read_csv('./data/msa_all_chars.csv') %>%
  filter(!is.na(MsaName))
#377 MSAs

## Create post-period dataset
df3 <- df %>% pivot_longer(!c(RegionID, SizeRank, MetroShort, RegionType, StateName),
                           names_to = 'date', values_to = 'zhvi') %>% 
  mutate(date = as.Date(date)) %>%
  group_by(MetroShort) %>% mutate(
    zhvi = na.approx(zhvi, na.rm=FALSE, rule = 2),
    post_pct_change = (zhvi - lag(zhvi, 12))/((zhvi + lag(zhvi, 12))/2)*100,
  ) %>% filter(date >= as.Date('2021-02-01'), date < as.Date('2021-03-01')) %>%
  group_by(MetroShort) %>% summarise(post_pct_change = mean(post_pct_change, na.rm = TRUE))

## Create pre-period dataset
df4 <- df %>% pivot_longer(!c(RegionID, SizeRank, MetroShort, RegionType, StateName),
                           names_to = 'date', values_to = 'zhvi') %>% 
  mutate(date = as.Date(date)) %>%
  group_by(MetroShort) %>% mutate(
    zhvi = na.approx(zhvi, na.rm=FALSE, rule = 2),
    pre_pct_change = (zhvi - lag(zhvi, 12))/((zhvi + lag(zhvi, 12))/2)*100,
  ) %>% filter(date >= as.Date('2020-02-01'), date < as.Date('2020-03-01')) %>%
  group_by(MetroShort) %>% summarise(pre_pct_change = mean(pre_pct_change, na.rm = TRUE))

df5 <- df3 %>% left_join(df4, by = 'MetroShort') %>% left_join(msa_chars, by = 'MetroShort') %>% 
  filter(!is.na(wfh_emp)) %>%
  write_csv('./data/msa_zhvi.csv')

###########################################
# This section of the script creates the rental index panel
###########################################

## Read in data
df <- read_csv('https://files.zillowstatic.com/research/public_v2/zori/Zip_ZORI_AllHomesPlusMultifamily_Smoothed.csv', col_types = cols(RegionName = col_double()))
chars <- read_csv('./data/zip_all_chars_cbd.csv')

## Define top 12 metros
cities <- c('San Francisco, CA', 'New York, NY', 'Chicago, IL', 'Boston, MA',
            'Los Angeles, CA', 'Washington, DC', 'Atlanta, GA', 'Miami, FL',
            'Philadelphia, PA', 'Dallas, TX', 'Houston, TX', 'Phoenix, AZ')

## Pivot from wide to long
df <- df %>% rename(zip = 'RegionName') %>%
  select(-c(RegionID, SizeRank, MsaName)) %>% 
  pivot_longer(!zip, names_to = 'date', values_to = 'zhvi') %>%
  mutate(date = as.Date(as.yearmon(date)) + 14)

## create pre-period panel
df3 <- df %>% mutate(date = as.Date(date)) %>%
  group_by(zip) %>% mutate(
    zhvi = na.approx(zhvi, na.rm=FALSE, rule = 2),
    post_pct_change = (zhvi - lag(zhvi, 12))/((zhvi + lag(zhvi, 12))/2)*100
  ) %>% filter(date >= as.Date('2021-02-01'), date < as.Date('2021-03-01')) %>%
  group_by(zip) %>% summarise(post_pct_change = mean(post_pct_change, na.rm = TRUE)) %>%
  inner_join(chars, by = 'zip') %>%
  filter(!is.na(wfh_emp), !is.na(log(density2019)), !is.infinite(log(density2019)),
         land_area > .1, `2019 Population` > 100)
 
## Create post-period panel
df4 <- df %>% mutate(date = as.Date(date)) %>%
  group_by(zip) %>% mutate(
    zhvi = na.approx(zhvi, na.rm=FALSE, rule = 2),
    pre_pct_change = (zhvi - lag(zhvi, 12))/((zhvi + lag(zhvi, 12))/2)*100
  ) %>% filter(date >= as.Date('2020-02-01'), date < as.Date('2020-03-01')) %>%
  group_by(zip) %>% summarise(pre_pct_change = mean(pre_pct_change, na.rm = TRUE)) %>%
  inner_join(chars, by = 'zip') %>%
  filter(!is.na(wfh_emp), !is.na(log(density2019)), !is.infinite(log(density2019)),
         land_area > .1, `2019 Population` > 100)

## Write panel to CSV (with top 100 metros)
df6 <- df3 %>% select(zip, post_pct_change) %>% inner_join(df4, by = 'zip') %>%
  mutate(pp_growth = post_pct_change - pre_pct_change) %>%
  filter(!is.na(dist_to_cbd)) %>% 
  write_csv('./data/zori_panel_zips.csv')

## Write panel to CSV (with top 12 metros)
df6 %>% filter(MetroShort %in% cities) %>%
  write_csv('./data/zori_panel_zips_top12.csv')

###########################################
# This section of the script creates the 
# MSA level rent value index pct change file
###########################################

## Read in home value index dataset
df <- read_csv('https://files.zillowstatic.com/research/public_v2/zori/Metro_ZORI_AllHomesPlusMultifamily_Smoothed.csv') %>%
  separate(RegionName, c("MetroShort", "StateName"), sep = ', ') %>%
  filter(!is.na(StateName)) %>%
  mutate(MetroShort = sub("-.*", "", MetroShort),
         MetroShort = paste(MetroShort, StateName, sep = ', '))

###this doesn't work fix it

## Read in MSA-level characteristics
msa_chars <- read_csv('./data/msa_all_chars.csv') %>%
  filter(!is.na(MsaName))
#377 MSAs

## Create post-period dataset
df3 <- df %>% pivot_longer(!c(RegionID, SizeRank, MetroShort, StateName),
                           names_to = 'date', values_to = 'zhvi') %>% 
  mutate(date = as.Date(as.yearmon(date)) + 14) %>%
  group_by(MetroShort) %>% mutate(
    zhvi = na.approx(zhvi, na.rm=FALSE, rule = 2),
    post_pct_change = (zhvi - lag(zhvi, 12))/((zhvi + lag(zhvi, 12))/2)*100,
  ) %>% filter(date >= as.Date('2021-02-01'), date < as.Date('2021-03-01')) %>%
  group_by(MetroShort) %>% summarise(post_pct_change = mean(post_pct_change, na.rm = TRUE))

## Create pre-period dataset
df4 <- df %>% pivot_longer(!c(RegionID, SizeRank, MetroShort, StateName),
                           names_to = 'date', values_to = 'zhvi') %>% 
  mutate(date = as.Date(as.yearmon(date)) + 14) %>%
  group_by(MetroShort) %>% mutate(
    zhvi = na.approx(zhvi, na.rm=FALSE, rule = 2),
    pre_pct_change = (zhvi - lag(zhvi, 12))/((zhvi + lag(zhvi, 12))/2)*100,
  ) %>% filter(date >= as.Date('2020-02-01'), date < as.Date('2020-03-01')) %>%
  group_by(MetroShort) %>% summarise(pre_pct_change = mean(pre_pct_change, na.rm = TRUE))

df5 <- df3 %>% left_join(df4, by = 'MetroShort') %>% left_join(msa_chars, by = 'MetroShort') %>% 
  filter(!is.na(wfh_emp)) %>%
  write_csv('./data/msa_zori.csv')

