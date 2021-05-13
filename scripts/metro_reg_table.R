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

msa_usps <- read_csv("/Users/arjun/Documents/zillow/thesis/data/msa_USPS.csv")
msa_zori <- read_csv('~/Documents/zillow/thesis/data/msa_zori.csv') %>%
  rename(post_rent_change = post_pct_change)
msa_zhvi <- read_csv('~/Documents/zillow/thesis/data/msa_zhvi.csv') %>%
  rename(post_price_change = post_pct_change)

##############################################
#ZORI
m0 <- lm(post_rent_change ~ pre_pct_change + log(density2019),
             data = msa_zori, weights = msa_zori$`2019 Population`)
m1 <- lm(post_rent_change ~ pre_pct_change + log(density2019) + log(wfh_emp),
             data = msa_zori, weights = msa_zori$`2019 Population`)

#ZHVI
m2 <- lm(post_price_change ~ pre_pct_change + log(density2019),
             data = msa_zhvi, weights = msa_zhvi$`2019 Population`)
m3 <- lm(post_price_change ~ pre_pct_change + log(density2019) + log(wfh_emp),
             data = msa_zhvi, weights = msa_zhvi$`2019 Population`)

#USPS pop
m4 <- lm(post_pop ~ pre_pop + log(density2019),
             data = msa_usps, weights = msa_usps$`2019 Population`)
m5 <- lm(post_pop ~ pre_pop + log(density2019) + log(wfh_emp),
             data = msa_usps, weights = msa_usps$`2019 Population`)

#USPS bus
m6 <- lm(post_bus ~ pre_bus + log(density2019),
             data = msa_usps, weights = msa_usps$`2019 Population`)
m7 <- lm(post_bus ~ pre_bus + log(density2019) + log(wfh_emp),
             data = msa_usps, weights = msa_usps$`2019 Population`)

###Create tables###
## Create function to get robust standard errors
rse <- function(reg) { 
  return(as.vector(summary(reg, robust = T)$coefficients[,"Std. Error"]))
} 

## Table
stargazer(m0, m1, m2, m3, m4, m5, m6, m7,
          se = list(rse(m0), rse(m1), rse(m2), rse(m3), rse(m4), rse(m5), rse(m6), rse(m7)), type = "html",
          omit = "MetroShort", omit.stat=c("adj.rsq", "ser","f"),
          out="~/Documents/zillow/thesis/tables/metro_all.doc")
