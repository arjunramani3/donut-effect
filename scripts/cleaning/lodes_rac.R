#######################################
# WFH calculations using the LODES data
#######################################
library(tidyverse)


#####load state codes#####
states <- read_csv('https://raw.githubusercontent.com/kjhealy/fips-codes/master/state_fips_master.csv') %>%
  select(state_name, state_abbr) %>% mutate(state_abbr = tolower(state_abbr))

#####load lodes data#####
root <- 'https://lehd.ces.census.gov/data/lodes/LODES7/'
#structure is root/[state]/[state]_rac/S000_JT00_2018.csv.gz

#https://www.huduser.gov/portal/datasets/usps_crosswalk.html
#crosswalk from tract to zip from HUDS
tract_to_zip <- read_csv('~/Documents/zillow/thesis/data/external_data/TRACT_ZIP_092020.csv') %>%
  select(TRACT, ZIP, RES_RATIO) %>%
  rename(tract = TRACT, zip = ZIP, ratio = RES_RATIO)

#WFH exposure at 2-digit NAICS level
wfh_exposure <- read_csv('https://raw.githubusercontent.com/jdingel/DingelNeiman-workathome/master/national_measures/output/NAICS_workfromhome.csv')

#Lodes Codes to NAICS crosswalk
#https://lehd.ces.census.gov/data/lodes/LODES7/LODESTechDoc7.5.pdf
lodes_naics <- read_csv('~/Documents/zillow/thesis/data/LODES-code-NAICS.csv') %>%
  select(Variable, NAICS)

all <- data.frame(zip = double(), wfh_wage = double(), wfh_emp = double())

for (code in states$state_abbr) {
  path = paste(root, code, '/rac/', code, '_rac_S000_JT00_2018.csv.gz', sep = '')
  #get tract, NAICS code panel of address counts
  cur <- read_csv(path) %>% mutate(tract = substr(h_geocode, 0, 11)) %>% 
    select(-c(h_geocode, createdate)) %>%
    group_by(tract) %>% summarise_all(sum) %>% 
    pivot_longer(-tract, names_to = 'Variable', values_to = 'count') %>%
    inner_join(lodes_naics, on = 'Variable')
  #merge with crosswalk and aggregate to zip code level
  cur <- cur %>% left_join(tract_to_zip, on = 'tract') %>%
    mutate(weighted_count = count*ratio) %>%
    group_by(zip, NAICS) %>% summarise(total_count = sum(weighted_count))
  #merge with WFH exposure from Dingel and Neiman (2020) and aggregate to zip code level
  cur <- cur %>% left_join(wfh_exposure, on = 'NAICS') %>%
    mutate(weighted_wage = total_count*teleworkable_wage,
           weighted_emp = total_count*teleworkable_emp) %>%
    group_by(zip) %>% summarise(wfh_wage = sum(weighted_wage, na.rm = TRUE)/sum(total_count, na.rm = TRUE),
                                wfh_emp = sum(weighted_emp, na.rm = TRUE)/sum(total_count, na.rm = TRUE))
  
  all <- rbind(all, cur)
}
all2 <- all %>% filter(!is.na(wfh_wage)) %>%
  distinct(zip, .keep_all = TRUE)
write_csv(all2, '~/Documents/zillow/thesis/data/lodes_rac_wfh_zip_exposure.csv')


