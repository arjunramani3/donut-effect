###########################################
# zillow_tables.R
# This script reads in cleaned pct change files from the Zillow dataset
# and creates tables
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

## Read in rents file and run ms
df_rent <- read_csv('~/Documents/zillow/thesis/data/zori_sfh_panel_zips_top12.csv') %>% filter(MetroShort %in% cities) %>%
  rename(rent_pct_change = post_pct_change) %>% filter(!is.na(rent_pct_change))

m0 <- lm(rent_pct_change ~ pre_pct_change + log(density2019) + factor(MetroShort), data = df_rent, weights = df_rent$`2019 Population`)
m1 <- lm(rent_pct_change ~ pre_pct_change + log(dist_to_cbd) + factor(MetroShort), data = df_rent, weights = df_rent$`2019 Population`)
m2 <- lm(rent_pct_change ~ pre_pct_change + log(wfh_emp) + factor(MetroShort), data = df_rent, weights = df_rent$`2019 Population`)
m3 <- lm(rent_pct_change ~ pre_pct_change + log(density2019) + log(dist_to_cbd) + log(wfh_emp) + factor(MetroShort), data = df_rent, weights = df_rent$`2019 Population`)

df_rent <- df_rent %>% filter(!is.na(deaths_capita))
m0b <- lm(rent_pct_change ~ pre_pct_change + log(density2019) + log(deaths_capita+1) + factor(MetroShort), data = df_rent, weights = df_rent$`2019 Population`)
m1b <- lm(rent_pct_change ~ pre_pct_change + log(dist_to_cbd) + log(deaths_capita+1) + factor(MetroShort), data = df_rent, weights = df_rent$`2019 Population`)
m2b <- lm(rent_pct_change ~ pre_pct_change + log(wfh_emp) + log(deaths_capita+1) + factor(MetroShort), data = df_rent, weights = df_rent$`2019 Population`)
m3b <- lm(rent_pct_change ~ pre_pct_change + log(density2019) + log(dist_to_cbd) + log(wfh_emp) + log(deaths_capita+1) + factor(MetroShort), data = df_rent, weights = df_rent$`2019 Population`)

## Read in home values file and run ms
df6 <- read_csv('~/Documents/zillow/thesis/data/zhvi_sfh_panel_zips_top12.csv')

m4 <- lm(post_pct_change ~ pre_pct_change + log(density2019) + factor(MetroShort), data = df6, weights = df6$`2019 Population`)
m5 <- lm(post_pct_change ~ pre_pct_change + log(dist_to_cbd) + factor(MetroShort), data = df6, weights = df6$`2019 Population`)
m6 <- lm(post_pct_change ~ pre_pct_change + log(wfh_emp) + factor(MetroShort), data = df6, weights = df6$`2019 Population`)
m7 <- lm(post_pct_change ~ pre_pct_change + log(density2019) + log(dist_to_cbd) + log(wfh_emp) + factor(MetroShort), data = df6, weights = df6$`2019 Population`)
df6 <- df6 %>% filter(!is.na(deaths_capita))

m4b <- lm(post_pct_change ~ pre_pct_change + log(density2019) + log(deaths_capita+1) + factor(MetroShort), data = df6, weights = df6$`2019 Population`)
m5b <- lm(post_pct_change ~ pre_pct_change + log(dist_to_cbd) + log(deaths_capita+1) + factor(MetroShort), data = df6, weights = df6$`2019 Population`)
m6b <- lm(post_pct_change ~ pre_pct_change + log(wfh_emp) + log(deaths_capita+1) + factor(MetroShort), data = df6, weights = df6$`2019 Population`)
m7b <- lm(post_pct_change ~ pre_pct_change + log(density2019) + log(dist_to_cbd) + log(wfh_emp) + log(deaths_capita+1) + factor(MetroShort), data = df6, weights = df6$`2019 Population`)

###Create tables###
## Create function to get 
rse <- function(reg) { 
  return(as.vector(summary(reg, robust = T)$coefficients[,"Std. Error"]))
} 

## Word All
#https://dmyee.files.wordpress.com/2016/03/table_workshop.pdf
stargazer(m0, m1, m2, m3, m4, m5, m6, m7, 
          se = list(rse(m1), rse(m2), rse(m3), rse(m4), rse(m5), rse(m6), rse(m7)),
          omit = c("MetroShort", 'pre_pct_change'),
          omit.stat=c("adj.rsq", "ser","f"), type="html", out="~/Documents/zillow/thesis/tables/zori_zhvi.doc")

## Latex Zori
stargazer(m0b, m1b, m2b, m3b,
          se = list(rse(m0b), rse(m1b), rse(m2b), rse(m3b), rse(m3b)),
          omit = c("MetroShort"),
          omit.stat=c("adj.rsq", "ser","f"), out="~/Documents/zillow/thesis/tables/zori.tex")

## Latex Zhvi
stargazer(m4b, m5b, m6b, m7b,
          se = list(rse(m4b), rse(m5b), rse(m6b), rse(m7b), rse(m7b)),
          omit = c("MetroShort"),
          omit.stat=c("adj.rsq", "ser","f"), out="~/Documents/zillow/thesis/tables/zhvi.tex")

