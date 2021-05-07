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

## Read in usps file and run models
df6 <- read_csv('~/Documents/zillow/thesis/data/usps_panel_zips_top12.csv')

m0 <- lm(post_pop ~ pre_pop + log(density2019) + factor(MetroShort), data = df6, weights = df6$`2019 Population`)
m1 <- lm(post_pop ~ pre_pop + log(dist_to_cbd) + factor(MetroShort), data = df6, weights = df6$`2019 Population`)
m2 <- lm(post_pop ~ pre_pop + log(wfh_emp) + factor(MetroShort), data = df6, weights = df6$`2019 Population`)
m3 <- lm(post_pop ~ pre_pop + log(density2019) + log(dist_to_cbd) + log(wfh_emp) + factor(MetroShort), data = df6, weights = df6$`2019 Population`)

m0b <- lm(post_pop ~ pre_pop + log(density2019) + log(deaths_capita+1) + factor(MetroShort), data = df6, weights = df6$`2019 Population`)
m1b <- lm(post_pop ~ pre_pop + log(dist_to_cbd) + log(deaths_capita+1) + factor(MetroShort), data = df6, weights = df6$`2019 Population`)
m2b <- lm(post_pop ~ pre_pop + log(wfh_emp) + log(deaths_capita+1) + factor(MetroShort), data = df6, weights = df6$`2019 Population`)
m3b <- lm(post_pop ~ pre_pop + log(density2019) + log(dist_to_cbd) + log(wfh_emp) + log(deaths_capita+1) + factor(MetroShort), data = df6, weights = df6$`2019 Population`)

## Business regressions
m4 <- lm(post_bus ~ pre_bus + log(density2019) + factor(MetroShort), data = df6, weights = df6$estab_count)
m5 <- lm(post_bus ~ pre_bus + log(dist_to_cbd) + factor(MetroShort), data = df6, weights = df6$estab_count)
m6 <- lm(post_bus ~ pre_bus + log(wfh_emp) + factor(MetroShort), data = df6, weights = df6$estab_count)
m7 <- lm(post_bus ~ pre_bus + log(density2019) + log(dist_to_cbd) + log(wfh_emp) + factor(MetroShort), data = df6, weights = df6$estab_count)

m4b <- lm(post_bus ~ pre_bus + log(density2019) + log(deaths_capita+1) + factor(MetroShort), data = df6, weights = df6$estab_count)
m5b <- lm(post_bus ~ pre_bus + log(dist_to_cbd) + log(deaths_capita+1) + factor(MetroShort), data = df6, weights = df6$estab_count)
m6b <- lm(post_bus ~ pre_bus + log(wfh_emp) + log(deaths_capita+1) + factor(MetroShort), data = df6, weights = df6$estab_count)
m7b <- lm(post_bus ~ pre_bus + log(density2019) + log(dist_to_cbd) + log(wfh_emp) + log(deaths_capita+1) + factor(MetroShort), data = df6, weights = df6$estab_count)

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
          omit.stat=c("adj.rsq", "ser","f"), type="html", out="~/Documents/zillow/thesis/tables/usps_main.doc")

## Latex Zori
stargazer(m0b, m1b, m2b, m3b,
          se = list(rse(m0b), rse(m1b), rse(m2b), rse(m3b), rse(m3b)),
          omit = c("MetroShort", 'pre_pct_change'),
          omit.stat=c("adj.rsq", "ser","f"), out="~/Documents/zillow/thesis/tables/usps_pop.tex")

## Latex Zhvi
stargazer(m4b, m5b, m6b, m7b,
          se = list(rse(m4b), rse(m5b), rse(m6b), rse(m7b), rse(m7b)),
          omit = c("MetroShort", 'pre_pct_change'),
          omit.stat=c("adj.rsq", "ser","f"), out="~/Documents/zillow/thesis/tables/usps_bus.tex")

