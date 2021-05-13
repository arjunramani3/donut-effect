###########################################
# create_usps_panel.R
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

## Define avg number of individuals per household
#https://www.census.gov/data/tables/time-series/demo/families/households.html
household = 2.5

## Create function to read USPS data
read_USPS <- function(path) {
  df <- read_csv(path, 
                 col_types = cols('YYYYMM' = col_character(),
                                  `TOTAL FROM ZIP` = col_integer(),
                                  `TOTAL BUSINESS` = col_integer(),
                                  `TOTAL FAMILY` = col_integer(),
                                  `TOTAL INDIVIDUAL` = col_integer(),
                                  `TOTAL PERM` = col_integer(),
                                  `TOTAL TEMP` = col_integer(),
                                  `TOTAL TO ZIP` = col_integer(),
                                  `TOTAL BUSINESS_1` = col_integer(),
                                  `TOTAL FAMILY_1` = col_integer(),
                                  `TOTAL INDIVIDUAL_1` = col_integer(),
                                  `TOTAL PERM_1` = col_integer(),
                                  `TOTAL TEMP_1` = col_integer()
                 )
  )  %>%
    rename(
      zip = ZIPCODE,
      date = YYYYMM
    ) %>%
    mutate(
      zip = gsub("=", "", zip),
      zip = gsub("\"", "", zip),
      date = as.Date(as.yearmon(date, format = '%Y%m')),
      date = date + 14
    )
  return(df)
}

## Read in USPS data (obtained through FOIA request but also available online on USPS website)
df17 <- read_USPS('~/Documents/zillow/thesis/data/external_data/USPS_online/Y2017.csv')
df18 <- read_USPS('~/Documents/zillow/thesis/data/external_data/USPS_online/Y2018.csv')
df19 <- read_USPS('~/Documents/zillow/thesis/data/external_data/USPS_online/Y2019.csv')
df20 <- read_USPS('~/Documents/zillow/thesis/data/external_data/USPS_online/Y2020.csv')
df21 <- read_USPS('~/Documents/zillow/thesis/data/external_data/USPS_online/Y2021.csv')

df_all <- rbind(df17, df18, df19, df20, df21)

df_all <- df_all %>% mutate(
  across(contains('TOTAL'), ~replace(., . ==  0 , 5)),
  zip = as.integer(zip),
  net = `TOTAL TO ZIP` - `TOTAL FROM ZIP`,
  net_bus = `TOTAL BUSINESS_1` - `TOTAL BUSINESS`,
  net_fam = `TOTAL FAMILY_1` - `TOTAL FAMILY`,
  net_ind = `TOTAL INDIVIDUAL_1` - `TOTAL INDIVIDUAL`,
  net_perm = `TOTAL PERM_1` - `TOTAL PERM`,
  net_temp = `TOTAL TEMP_1` - `TOTAL TEMP`,
  net_pop = net_fam + net_ind*household  # construct population estimate by multiplying avg
                                         # household size by number of family+individual moves
) %>%
  select(zip, date, CITY, STATE, net, net_bus, net_fam, net_ind, net_perm, net_temp, net_pop) %>%
  rename(city = CITY, state = STATE) %>%
  write_csv('~/Documents/zillow/thesis/data/USPS_zips.csv')

###########################################
# Create zip pct change file from panel
###########################################
chars <- read_csv('~/Documents/zillow/thesis/data/zip_all_chars_cbd.csv', 
                  col_types = cols('zip' = col_integer())) %>% select(!estab_count)
bus_chars <- read_csv('~/Documents/zillow/thesis/data/zbp_wfh.csv',
                      col_types = cols('zip' = col_integer()))
chars <- chars %>% inner_join(bus_chars, by = 'zip')

cities <- c('San Francisco, CA', 'New York, NY', 'Chicago, IL', 'Boston, MA',
            'Los Angeles, CA', 'Washington, DC', 'Atlanta, GA', 'Miami, FL',
            'Philadelphia, PA', 'Dallas, TX', 'Houston, TX', 'Phoenix, AZ')

## Get post-period percent growth
df3 <- df_all %>%
  filter(date >= as.Date('2020-03-01'), date < as.Date('2021-03-01')) %>%
  group_by(zip) %>% summarise(
    post_net = sum(net, na.rm = TRUE),
    post_bus = sum(net_bus, na.rm = TRUE),
    post_pop = sum(net_pop, na.rm = TRUE),
    post_temp = sum(net_pop, na.rm = TRUE),
    post_perm = sum(net_perm, na.rm = TRUE)
  ) %>%
  inner_join(chars, by = 'zip') %>% mutate(
    post_net = post_net/`2019 Population`*100,
    post_bus = post_bus/estab_count*100,
    post_pop = post_pop/`2019 Population`*100,
    post_temp = post_temp/`2019 Population`*100,
    post_perm = post_perm/`2019 Population`*100
  ) %>%
  filter(!is.na(wfh_wage), !is.na(log(density2019)), !is.infinite(log(density2019)), !is.na(price_level),
         land_area > .1, `2019 Population` > 100)

## Get pre-period percent growth
df4 <- df_all %>%
  filter(date >= as.Date('2019-03-01'), date < as.Date('2020-03-01')) %>%
  group_by(zip) %>% summarise(
    pre_net = sum(net, na.rm = TRUE),
    pre_bus = sum(net_bus, na.rm = TRUE),
    pre_pop = sum(net_pop, na.rm = TRUE),
    pre_temp = sum(net_pop, na.rm = TRUE),
    pre_perm = sum(net_perm, na.rm = TRUE)
  ) %>%
  inner_join(chars, by = 'zip') %>% mutate(
    pre_net = pre_net/`2019 Population`*100,
    pre_bus = pre_bus/estab_count*100,
    pre_pop = pre_pop/`2019 Population`*100,
    pre_temp = pre_temp/`2019 Population`*100,
    pre_perm = pre_perm/`2019 Population`*100
  ) %>%
  filter(!is.na(wfh_wage), !is.na(log(density2019)), !is.infinite(log(density2019)), !is.na(price_level),
         land_area > .1, `2019 Population` > 100)

## Write all zips file to csv
df6 <- df3 %>% select(zip, post_net, post_bus, post_pop, post_temp, post_perm) %>% inner_join(df4, by = 'zip') %>%
  filter(!is.na(dist_to_cbd)) %>%
  write_csv('~/Documents/zillow/thesis/data/usps_panel_zips.csv')

## Limit to top 12 and write to csv
df6 %>% filter(MetroShort %in% cities) %>%
  write_csv('~/Documents/zillow/thesis/data/usps_panel_zips_top12.csv')

###########################################
# Create MSA pct change file from panel
###########################################

metro_chars <- read_csv('~/Documents/zillow/thesis/data/msa_all_chars.csv')
cbsa_price <- read_csv('~/Documents/zillow/thesis/data/cbsa_price_levels.csv')

## get post period pct growth
df4 <- df_all %>% inner_join(chars, by = 'zip') %>%
  group_by(MetroShort, date) %>% summarise(
    net = sum(net, na.rm = TRUE),
    net_bus = sum(net_bus, na.rm = TRUE),
    net_pop = sum(net_pop, na.rm = TRUE),
    net_temp = sum(net_temp, na.rm = TRUE),
    net_perm = sum(net_perm, na.rm = TRUE)
  ) %>% filter(date >= as.Date('2020-03-01'), date < as.Date('2021-03-01')) %>%
  group_by(MetroShort) %>% summarise(
    post_net = sum(net, na.rm = TRUE),
    post_bus = sum(net_bus, na.rm = TRUE),
    post_pop = sum(net_pop, na.rm = TRUE),
    post_temp = sum(net_pop, na.rm = TRUE),
    post_perm = sum(net_perm, na.rm = TRUE)
  ) %>%
  inner_join(metro_chars, by = 'MetroShort') %>% mutate(
    post_net = post_net/`2019 Population`*100,
    post_bus = post_bus/estab_count*100,
    post_pop = post_pop/`2019 Population`*100,
    post_temp = post_temp/`2019 Population`*100,
    post_perm = post_perm/`2019 Population`*100
  ) %>%
  filter(!is.na(wfh_wage), !is.na(log(density2019)), !is.infinite(log(density2019)), !is.na(price_level),
         land_area > .1, `2019 Population` > 100)

## get pre period pct change
df5 <- df_all %>% inner_join(chars, by = 'zip') %>%
  group_by(MetroShort, date) %>% summarise(
    net = sum(net, na.rm = TRUE),
    net_bus = sum(net_bus, na.rm = TRUE),
    net_pop = sum(net_pop, na.rm = TRUE),
    net_temp = sum(net_temp, na.rm = TRUE),
    net_perm = sum(net_perm, na.rm = TRUE)
  ) %>% filter(date >= as.Date('2019-03-01'), date < as.Date('2020-03-01')) %>%
  group_by(MetroShort) %>% summarise(
    pre_net = sum(net, na.rm = TRUE),
    pre_bus = sum(net_bus, na.rm = TRUE),
    pre_pop = sum(net_pop, na.rm = TRUE),
    pre_temp = sum(net_pop, na.rm = TRUE),
    pre_perm = sum(net_perm, na.rm = TRUE)
  ) %>%
  inner_join(metro_chars, by = 'MetroShort') %>% mutate(
    pre_net = pre_net/`2019 Population`*100,
    pre_bus = pre_bus/estab_count*100,
    pre_pop = pre_pop/`2019 Population`*100,
    pre_temp = pre_temp/`2019 Population`*100,
    pre_perm = pre_perm/`2019 Population`*100
  ) %>%
  filter(!is.na(wfh_wage), !is.na(log(density2019)), !is.infinite(log(density2019)), !is.na(price_level),
         land_area > .1, `2019 Population` > 100)

df4 %>% select(MetroShort, post_net, post_bus, post_pop, post_temp, post_perm) %>% inner_join(df5, by = 'MetroShort') %>%
  write_csv('~/Documents/zillow/thesis/data/msa_USPS.csv')
