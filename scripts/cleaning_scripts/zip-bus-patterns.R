#####USPS zip code level data with business counts#####
#####USPS.R#####
rm(list=ls())
install.packages("censusapi")
library(tidyverse); library(censusapi)

#####Get Zip Code business patterns data#####

# Add key to .Renviron
key = '5f93f21dfff971a35463084cc328a3ed1ee4049d'
Sys.setenv(CENSUS_KEY=key)
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")

zbp_2018 <- getCensus(
  name = "zbp",
  vintage = 2018,
  vars = c("EMP", "ESTAB", "EMPSZES"),
  region = "zipcode:*",
  NAICS2017='')

write_csv(zbp_2018, '~/Documents/zillow/thesis/data/zbp_2018.csv')

#####Get WFH data from Dingel and Neiman (2020) and merge#####
wfh <- read_csv('https://raw.githubusercontent.com/jdingel/DingelNeiman-workathome/master/national_measures/output/NAICS3_workfromhome.csv')

zbp <- zbp_2018 %>% filter(nchar(NAICS2017) == 3) %>% rename(NAICS = NAICS2017) %>%
  mutate(NAICS = as.double(NAICS)) %>%
  rename(zip = zip_code) %>%
  select(zip, NAICS, ESTAB) %>%
  inner_join(wfh, by = 'NAICS') %>%
  group_by(zip) %>%
  summarise(estab_count = sum(ESTAB, na.rm = TRUE),
            teleworkable_emp_mean = weighted.mean(teleworkable_emp, ESTAB),
            teleworkable_wage_mean = weighted.mean(teleworkable_wage, ESTAB))

write_csv(zbp, '~/Documents/zillow/thesis/data/zbp_wfh.csv')
