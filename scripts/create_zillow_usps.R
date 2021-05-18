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
packages <- c("tidyverse")
lapply(packages, pkgTest)

## Define top 12 Metros
cities <- c('San Francisco, CA', 'New York, NY', 'Chicago, IL', 'Boston, MA',
            'Los Angeles, CA', 'Washington, DC', 'Atlanta, GA', 'Miami, FL',
            'Philadelphia, PA', 'Dallas, TX', 'Houston, TX', 'Phoenix, AZ')

#read in zipcode level data from USPS and Zillow
usps_zips <- read_csv('./data/usps_panel_zips_top12.csv')
zori_zips <- read_csv('./data/zori_panel_zips_top12.csv')
zhvi_zips <- read_csv('./data/zhvi_panel_zips_top12.csv')


zori_usps <- usps_zips %>% select(zip, post_net, post_bus, post_pop, post_temp, post_perm,
                                  pre_net, pre_bus, pre_pop, pre_temp, pre_perm) %>%
  inner_join(zori_zips, by = 'zip') %>%
  write_csv('./data/zori_usps.csv')

zhvi_usps <- usps_zips %>% select(zip, post_net, post_bus, post_pop, post_temp, post_perm,
                                  pre_net, pre_bus, pre_pop, pre_temp, pre_perm) %>%
  inner_join(zhvi_zips, by = 'zip') %>%
  write_csv('./data/zhvi_usps.csv')

