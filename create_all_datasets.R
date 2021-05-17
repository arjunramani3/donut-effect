###########################################
# create_all_datasets.R
# This file walks through the creation of datasets needed to run
# all the figures and tables creation scripts
###########################################

## Set the working directory to the cloned directory
setwd('~/Documents/zillow/thesis/donut-effect/')

## Run all dataset creation scripts
## WARNING: These scripts require certain exteranl datasets to be downloaded 
## and stored in './data/external_data/' Please see the README for details.

source('./scripts/lodes_rac.R')
source('./scripts/zip_bus_patterns.R')
source('./scripts/zip_all_chars.R')
source('./scripts/CBD_holian.R')
source('./scripts/msa_chars.R')
source('./scripts/create_zillow_panels.R')
source('./scripts/create_usps_panel.R')
