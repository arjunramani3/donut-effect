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

#merge the three datasets
msa_chars <- dens %>% group_by(MetroShort) %>% 
  summarise(`2010 Population` = sum(`2010 Population`, na.rm = TRUE),
            `2019 Population` = sum(`2019 Population`, na.rm = TRUE),
            area = sum(area, na.rm = TRUE),
            land_area = sum(land_area, na.rm = TRUE),
            estab_count = sum(estab_count, na.rm = TRUE)
            ) %>%
  mutate(density = `2010 Population`/land_area,
         density2019 = `2019 Population`/land_area) %>%
  inner_join(wfh, by = 'MetroShort')

#export
write_csv(msa_chars, './data/msa_all_chars.csv')

