###########################################
# usps_zillow.R
# This script reads in cleaned pct change files from the USPS and Zillow 
# and creates combined figures and tables
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
packages <- c("tidyverse", "stargazer", 'RCurl')
lapply(packages, pkgTest)

## Obtain package for robust standard errors
url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
     envir=.GlobalEnv)

## Define top 12 Metros
cities <- c('San Francisco, CA', 'New York, NY', 'Chicago, IL', 'Boston, MA',
            'Los Angeles, CA', 'Washington, DC', 'Atlanta, GA', 'Miami, FL',
            'Philadelphia, PA', 'Dallas, TX', 'Houston, TX', 'Phoenix, AZ')

#read in zipcode level data from USPS and Zillow
usps_zips <- read_csv('~/Documents/zillow/thesis/data/usps_panel_zips_top12.csv')
zori_zips <- read_csv('~/Documents/zillow/thesis/data/zori_sfh_panel_zips_top12.csv')
zhvi_zips <- read_csv('~/Documents/zillow/thesis/data/zhvi_sfh_panel_zips_top12.csv')


zori_usps <- usps_zips %>% select(zip, post_net, post_bus, post_pop, post_temp, post_perm,
                                  pre_net, pre_bus, pre_pop, pre_temp, pre_perm) %>%
  inner_join(zori_zips, by = 'zip') %>%
  write_csv('~/Documents/zillow/thesis/data/zori_usps.csv')

zhvi_usps <- usps_zips %>% select(zip, post_net, post_bus, post_pop, post_temp, post_perm,
                                  pre_net, pre_bus, pre_pop, pre_temp, pre_perm) %>%
  inner_join(zhvi_zips, by = 'zip') %>%
  write_csv('~/Documents/zillow/thesis/data/zhvi_usps.csv')

m0 <- lm(post_pct_change ~ pre_pct_change + post_pop + log(density2019) + log(wfh_emp) + log(deaths_capita+1) + factor(MetroShort), 
             data = zori_usps, weights = zori_usps$`2019 Population`)

m2 <- lm(post_pct_change ~ pre_pct_change + post_pop + log(density2019) + log(wfh_emp) + log(deaths_capita+1) + factor(MetroShort), 
             data = zhvi_usps, weights = zhvi_usps$`2019 Population`)

############################################
# MSA level regressions
############################################

msa_usps <- read_csv("/Users/arjun/Documents/zillow/thesis/data/msa_USPS.csv")
msa_zori <- read_csv('~/Documents/zillow/thesis/data/msa_zori.csv')
msa_zhvi <- read_csv('~/Documents/zillow/thesis/data/msa_zhvi.csv')

zori_usps_msa <- msa_usps %>% select(MetroShort, post_net, post_bus, post_pop, post_temp, post_perm,
                                     pre_net, pre_bus, pre_pop, pre_temp, pre_perm) %>%
  inner_join(msa_zori, by = 'MetroShort')

## Regress home values on population inflows
m1 <- lm(post_pct_change ~ pre_pct_change + post_pop + log(density2019) + log(wfh_emp) + log(deaths_capita+1),
             data = zori_usps_msa, weights = zori_usps_msa$`2019 Population`)

zhvi_usps_msa <- msa_usps %>% select(MetroShort, post_net, post_bus, post_pop, post_temp, post_perm,
                                     pre_net, pre_bus, pre_pop, pre_temp, pre_perm) %>%
  inner_join(msa_zhvi, by = 'MetroShort')

m3 <- lm(post_pct_change ~ pre_pct_change + post_pop + log(density2019) + log(wfh_emp) + log(deaths_capita+1),
             data = zhvi_usps_msa, weights = zhvi_usps_msa$`2019 Population`)


###Create tables###
## Create function to get robust standard errors
rse <- function(reg) { 
  return(as.vector(summary(reg, robust = T)$coefficients[,"Std. Error"]))
} 

## For Word
#https://dmyee.files.wordpress.com/2016/03/table_workshop.pdf
stargazer(m0, m1, m2, m3,
          se = list(rse(m0), rse(m1), rse(m2), rse(m3)),
          omit = c("MetroShort"), omit.stat=c("adj.rsq", "ser","f"),
          type="html", out="~/Documents/zillow/thesis/tables/usps_zillow.doc")

## For Latex
stargazer(m0, m1, m2, m3,
          se = list(rse(m0), rse(m1), rse(m2), rse(m3)),
          omit = c("MetroShort"), omit.stat=c("adj.rsq", "ser","f"),
          out="~/Documents/zillow/thesis/tables/usps_zillow.tex")

