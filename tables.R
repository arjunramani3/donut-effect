###########################################
# tables.R
# This script reads in the cleaned USPS and Zillow 
# files, runs regressions, and creates Tables 1 and 2
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

## Create function to get robust standard errors
rse <- function(reg) { 
  return(as.vector(summary(reg, robust = T)$coefficients[,"Std. Error"]))
} 

## Define top 12 Metros
cities <- c('San Francisco, CA', 'New York, NY', 'Chicago, IL', 'Boston, MA',
            'Los Angeles, CA', 'Washington, DC', 'Atlanta, GA', 'Miami, FL',
            'Philadelphia, PA', 'Dallas, TX', 'Houston, TX', 'Phoenix, AZ')

#set to your working directory
setwd('~/Documents/zillow/thesis/donut-effect/')

################################################
# Create Table 1: Zillow
################################################
## Read in rents file and run ms
df_rent <- read_csv('./data/zori_panel_zips_top12.csv') %>% filter(MetroShort %in% cities) %>%
  rename(rent_pct_change = post_pct_change) %>% filter(!is.na(rent_pct_change))

m0 <- lm(rent_pct_change ~ pre_pct_change + log(density2019) + factor(MetroShort), data = df_rent, weights = df_rent$`2019 Population`)
m1 <- lm(rent_pct_change ~ pre_pct_change + log(dist_to_cbd) + factor(MetroShort), data = df_rent, weights = df_rent$`2019 Population`)
m2 <- lm(rent_pct_change ~ pre_pct_change + log(wfh_emp) + factor(MetroShort), data = df_rent, weights = df_rent$`2019 Population`)
m3 <- lm(rent_pct_change ~ pre_pct_change + log(density2019) + log(dist_to_cbd) + log(wfh_emp) + factor(MetroShort), data = df_rent, weights = df_rent$`2019 Population`)

## Read in home values file and run ms
df6 <- read_csv('./data/zhvi_panel_zips_top12.csv')

m4 <- lm(post_pct_change ~ pre_pct_change + log(density2019) + factor(MetroShort), data = df6, weights = df6$`2019 Population`)
m5 <- lm(post_pct_change ~ pre_pct_change + log(dist_to_cbd) + factor(MetroShort), data = df6, weights = df6$`2019 Population`)
m6 <- lm(post_pct_change ~ pre_pct_change + log(wfh_emp) + factor(MetroShort), data = df6, weights = df6$`2019 Population`)
m7 <- lm(post_pct_change ~ pre_pct_change + log(density2019) + log(dist_to_cbd) + log(wfh_emp) + factor(MetroShort), data = df6, weights = df6$`2019 Population`)

###Create table
#https://dmyee.files.wordpress.com/2016/03/table_workshop.pdf
stargazer(m0, m1, m2, m3, m4, m5, m6, m7, 
          se = list(rse(m1), rse(m2), rse(m3), rse(m4), rse(m5), rse(m6), rse(m7)),
          omit = c("MetroShort", 'pre_pct_change'),
          omit.stat=c("adj.rsq", "ser","f"), type="html", out="./figures-tables/tab1.doc")



################################################
# Create Table 2: USPS
################################################
## Read in usps file and run models
df6 <- read_csv('./data/usps_panel_zips_top12.csv')

m0 <- lm(post_pop ~ pre_pop + log(density2019) + factor(MetroShort), data = df6, weights = df6$`2019 Population`)
m1 <- lm(post_pop ~ pre_pop + log(dist_to_cbd) + factor(MetroShort), data = df6, weights = df6$`2019 Population`)
m2 <- lm(post_pop ~ pre_pop + log(wfh_emp) + factor(MetroShort), data = df6, weights = df6$`2019 Population`)
m3 <- lm(post_pop ~ pre_pop + log(density2019) + log(dist_to_cbd) + log(wfh_emp) + factor(MetroShort), data = df6, weights = df6$`2019 Population`)

## Business regressions
m4 <- lm(post_bus ~ pre_bus + log(density2019) + factor(MetroShort), data = df6, weights = df6$estab_count)
m5 <- lm(post_bus ~ pre_bus + log(dist_to_cbd) + factor(MetroShort), data = df6, weights = df6$estab_count)
m6 <- lm(post_bus ~ pre_bus + log(wfh_emp) + factor(MetroShort), data = df6, weights = df6$estab_count)
m7 <- lm(post_bus ~ pre_bus + log(density2019) + log(dist_to_cbd) + log(wfh_emp) + factor(MetroShort), data = df6, weights = df6$estab_count)

## Create table
#https://dmyee.files.wordpress.com/2016/03/table_workshop.pdf
stargazer(m0, m1, m2, m3, m4, m5, m6, m7, 
          se = list(rse(m1), rse(m2), rse(m3), rse(m4), rse(m5), rse(m6), rse(m7)),
          omit = c("MetroShort", 'pre_pop', 'pre_bus'),
          omit.stat=c("adj.rsq", "ser","f"), type="html", out="./figures-tables/tab2.doc")








