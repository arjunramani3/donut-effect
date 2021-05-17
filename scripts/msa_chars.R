#####Get all MSA characteristics#####
#Msa = full metropolitan statistical area name with state
#MsaName = full metropolitan statistical are name without state
#Metro = shorter metro are name with state

#read in density data
dens <- read_csv('./data/zip_all_chars_cbd.csv')

#WFH Dingel and Neiman 2020
wfh <- read_csv('https://raw.githubusercontent.com/jdingel/DingelNeiman-workathome/master/MSA_measures/output/MSA_workfromhome.csv') %>%
  rename(Msa = AREA_NAME,
         wfh_wage = teleworkable_wage,
         wfh_emp = teleworkable_emp) %>%
  separate(Msa, c("MsaName", "end"), ', ', remove = FALSE) %>% 
  mutate(MetroShort = sub("-.*", "", MsaName),
         MetroState = sub("-.*", "", end),
         MetroShort = paste(MetroShort, MetroState, sep = ', '))


#Zillow Research -> ZHVI at metro level (to get price levels)
df_price <- read_csv('http://files.zillowstatic.com/research/public_v2/zhvi/Metro_zhvi_uc_sfr_tier_0.33_0.67_sm_sa_mon.csv') %>%
  rename(MetroShort = RegionName) %>%
  mutate(MetroShort = sub(",.*", "", MetroShort),
         MetroShort = sub("-.*", "", MetroShort),
         MetroShort = paste(MetroShort, StateName, sep = ', ')) %>% 
  pivot_longer(!c(RegionID, SizeRank, MetroShort, RegionType, StateName),
                  names_to = 'date', values_to = 'zhvi') %>% 
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date('2019-01-01'), date < as.Date('2020-01-01')) %>%
  group_by(MetroShort) %>% summarise(
    price_level = mean(zhvi, na.rm=TRUE))

write_csv(df_price, './data/cbsa_price_levels.csv')

#merge the three datasets
msa_chars <- dens %>% group_by(MetroShort) %>% 
  summarise(`2010 Population` = sum(`2010 Population`, na.rm = TRUE),
            `2019 Population` = sum(`2019 Population`, na.rm = TRUE),
            total_deaths = sum(total_deaths, na.rm = TRUE),
            area = sum(area, na.rm = TRUE),
            land_area = sum(land_area, na.rm = TRUE),
            estab_count = sum(estab_count, na.rm = TRUE)
            ) %>%
  mutate(density = `2010 Population`/land_area,
         density2019 = `2019 Population`/land_area,
         deaths_capita = total_deaths/`2019 Population`) %>%
  inner_join(wfh, by = 'MetroShort') %>%
  inner_join(df_price, by = 'MetroShort')

#export
write_csv(msa_chars, './data/msa_all_chars.csv')

