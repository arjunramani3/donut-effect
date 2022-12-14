###########################################
# all_R_figures.R
# This script reads in the cleaned USPS and Zillow 
# files, and creates figures for the paper
# note: creates density groups for each metro individually
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
packages <- c("tidyverse", "zoo", "directlabels", "cowplot", "pracma")
lapply(packages, pkgTest);

cities <- c('San Francisco, CA', 'New York, NY', 'Chicago, IL', 'Boston, MA',
            'Los Angeles, CA', 'Washington, DC', 'Atlanta, GA', 'Miami, FL',
            'Philadelphia, PA', 'Dallas, TX', 'Houston, TX', 'Phoenix, AZ')

#define colors
black <- "#2E2D29"; cardinal <- "#B1040E"; teal <- "#66b2b2"; green <- "#228B22"; marmalade <- "#d16002"
options(repr.plot.width=10, repr.plot.height=8)

#end date for figures
end_date = '2022-12-01'
end_date_long = '2023-05-01'

## start and end date for for cumulations
start_period = '2017-06-01' #start period of cumulation ending in 2020-03-01 exclusive
end_period = '2022-12-01' #end period for cumulation starting in 2020-03-01 inclusive

#set to your working directory
setwd('~/Documents/zillow/thesis/donut-effect/')

###########################################
## Figure 1(a) donut effect in rental market
###########################################
#read in zip code level rental index from Zillow
df <- read_csv('https://files.zillowstatic.com/research/public_csvs/zori/Zip_zori_sm_month.csv?t=1666834047', 
               col_types = cols(RegionName = col_double())) %>% rename(zip = 'RegionName') %>%
  dplyr::select(!c(RegionID, SizeRank, RegionType, StateName, State, City, Metro, CountyName)) %>% 
  pivot_longer(!zip, names_to = 'date', values_to = 'zori') %>%
  mutate(date = as.Date(as.yearmon(date)) + 14) %>%
  filter(date >= as.Date('2018-01-01'), date < as.Date(end_date)) %>%
  group_by(zip) %>% mutate(zori = na.approx(zori, na.rm=FALSE),
                           triple = ifelse(zori/lag(zori, 12) > 3, 1, 0),
                           count_na = sum(is.na(zori)), num_vals=n()) %>%
  group_by(zip) %>% filter(all(triple==0 | is.na(triple)), # filter out rows with 3x growth in rents (happens when there are several consecutive missing observations that zillow imputes)
                           count_na/num_vals<.5) # filter out rows with more than half missing values

#read in all zip code characteristics
chars <- read_csv('./data/zip_all_chars_cbd.csv')

#construct dataset to plot
temp <- chars %>% filter(MetroShort %in% cities, !is.na(`2019 Population`), !is.na(dist_to_cbd)) %>%
  mutate(quantile_rank = 'cbd') %>%
  group_by(MsaName) %>%
  mutate(quantile_rank = replace(quantile_rank, dist_to_cbd >= 2000, ntile(density2019[dist_to_cbd>=2000], 10)),
         category = ifelse(quantile_rank == 'cbd', 'city center',
                           case_when(quantile_rank == 10 ~ 'high density',
                                     quantile_rank %in% c(6:9) ~ 'suburb',
                                     TRUE ~ 'exurb'))) %>%
  inner_join(df, by = 'zip') %>% 
  mutate(date = as.Date(date),
         zori_pop = zori*`2019 Population`) %>%
  group_by(category, date) %>%
  summarise(zori_pop = sum(zori_pop, na.rm = TRUE),
            population = sum(`2019 Population`, na.rm = TRUE)) %>% 
  mutate(zori = zori_pop/population) %>%
  group_by(category) %>% mutate(zori = zori/zori[date == as.Date('2020-02-15')]*100) %>%
  mutate(val = zori, type = category, emph = "b")

#make plot
temp %>% ggplot(
  aes(x=date, y = val, color = type, linetype = as.factor(emph), 
      alpha = as.factor(emph),size = as.factor(emph)), guide='none') +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  geom_line()+
  xlim(as.Date('2018-01-01'), as.Date(end_date_long)) + 
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  geom_dl(aes(label = type), method = list(cex = 1.2,'last.bumpup')) +
  scale_colour_manual(values = c(teal, cardinal, black, green), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "Rental Index (Feb 2020=100)",
       size = 10
  )+
  geom_text(label="Feb 2020", x=as.Date('2019-12-01'), y=95, size = 6, color = 'black',
  ) +
  theme_minimal()+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)
  )

#save file
ggsave('./figures-tables/fig1a.png', plot = last_plot(), width = 10, height = 8)


###########################################
## Figure 1(b) donut effect in purchase market
###########################################
#read zip code level home value index for single family homes from Zillow
df2 <- read_csv('https://files.zillowstatic.com/research/public_csvs/zhvi/Zip_zhvi_uc_sfr_tier_0.33_0.67_sm_sa_month.csv?t=1631634893', 
                col_types = cols(RegionName = col_double())) %>% 
  rename(zip = 'RegionName', MsaShort = 'Metro') %>%
  mutate(MetroShort = sub("-.*", "", MsaShort),
         MetroShort = paste(MetroShort, State, sep = ', ')) %>%
  dplyr::select(!c(RegionID, SizeRank, RegionType, StateName, State, City, CountyName, MsaShort)) %>%
  pivot_longer(!c(zip, MetroShort), names_to = 'date', values_to = 'zhvi') %>%
  filter(date >= as.Date('2018-01-01'), date < as.Date(end_date)) %>%
  group_by(zip) %>% mutate(zhvi = na.approx(zhvi, na.rm=FALSE),
                           triple = ifelse(zhvi/lag(zhvi, 12) > 3, 1, 0),
                           count_na = sum(is.na(zhvi)),
                           num_vals=n()) %>%
  group_by(zip) %>% filter(all(triple==0 | is.na(triple)), 
                           count_na/num_vals<.5) ### filter out rows with 3x growth in rents (happens when there are several consecutive missing observations that zillow imputes)


#read in zip code characteristics
chars <- read_csv('./data/zip_all_chars_cbd.csv')

#construct dataset
temp <- chars %>% filter(MetroShort %in% cities, !is.na(`2019 Population`), !is.na(dist_to_cbd)) %>%
  mutate(quantile_rank = 'cbd') %>%
  group_by(MsaName) %>%
  mutate(quantile_rank = replace(quantile_rank, dist_to_cbd >= 2000, ntile(density2019[dist_to_cbd>=2000], 10)),
         category = ifelse(quantile_rank == 'cbd', 'city center',
                           case_when(quantile_rank == 10 ~ 'high density',
                                     quantile_rank %in% c(6:9) ~ 'suburb',
                                     TRUE ~ 'exurb'))) %>%
  inner_join(df2, by = 'zip') %>% 
  mutate(date = as.Date(date),
         zhvi_pop = zhvi*`2019 Population`) %>%
  group_by(zip) %>% mutate(zhvi_pop = na.approx(zhvi_pop, na.rm=FALSE)) %>% 
  group_by(category, date) %>%
  summarise(zhvi_pop = sum(zhvi_pop, na.rm = TRUE),
            population = sum(`2019 Population`, na.rm = TRUE)) %>% 
  mutate(zhvi = zhvi_pop/population) %>%
  group_by(category) %>% mutate(zhvi = zhvi/zhvi[date == as.Date('2020-02-29')]*100) %>%
  mutate(
    val = zhvi, type = category, emph = "b"
  )

temp %>% ggplot(
  aes(x=date, y = val, color = type, linetype = as.factor(emph), 
      alpha = as.factor(emph),size = as.factor(emph)), guide='none') +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  geom_line()+
  xlim(as.Date('2018-01-01'), as.Date(end_date_long)) + 
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  geom_dl(aes(label = type), method = list(cex = 1.2,'last.bumpup')) +  
  scale_colour_manual(values = c(teal, cardinal, black, green), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "Home Value Index (Feb 2020=100)",
       size = 10
  )+
  geom_text(label="Feb 2020", x=as.Date('2019-12-01'), y=95, size = 6, color = 'black',
  ) +
  theme_minimal()+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)
  )

#save plot
ggsave('./figures-tables/fig1b.png', plot = last_plot(), width = 10, height = 8)

###########################################
## Fig 2 Donut effect for USPS population and business flows
###########################################
#read in zip code characteristics
chars <- read_csv('./data/zip_all_chars_cbd.csv', 
                  col_types = cols('zip' = col_integer())) %>% dplyr::select(!estab_count)
#read in zip code level business establishment counts
bus_chars <- read_csv('./data/zbp_wfh.csv',
                      col_types = cols('zip' = col_integer()))
chars <- chars %>% inner_join(bus_chars, by = 'zip')

#read in zip code level USPS flow data
usps <- read_csv('./data/USPS_zips.csv')

## A. population flows
#construct dataset
temp <- chars %>% filter(MetroShort %in% cities, !is.na(`2019 Population`)) %>%
  mutate(quantile_rank = 'cbd') %>%
  group_by(MsaName) %>%
  mutate(quantile_rank = replace(quantile_rank, dist_to_cbd >= 2000, ntile(density2019[dist_to_cbd>=2000], 10)),
         category = ifelse(quantile_rank == 'cbd', 'city center',
                           case_when(quantile_rank == 10 ~ 'high density',
                                     quantile_rank %in% c(6:9) ~ 'suburb',
                                     TRUE ~ 'exurb'))) %>%
  inner_join(usps, by = 'zip') %>% 
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date('2018-01-01'), date < as.Date(end_date)) %>%
  group_by(zip) %>% mutate(net_pop = na.approx(net_pop, na.rm=FALSE)) %>%
  group_by(category, date) %>%
  summarise(net_pop = sum(net_pop, na.rm = TRUE)/sum(`2019 Population`)*100) %>% 
  group_by(category) %>%
  mutate(net_pop_avg = zoo::rollmean(net_pop, 12, align='right', na.pad=TRUE),
         net_pop = net_pop - net_pop_avg[date == as.Date('2020-02-15')]) %>%
  mutate(emph="b", val = net_pop, name = category)

#make plot
temp %>% ggplot(
  aes(x=date, y = val, color = name, linetype = as.factor(emph), 
      alpha = as.factor(emph),size = as.factor(emph)), guide='none') +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  geom_line()+
  xlim(as.Date('2018-01-01'), as.Date(end_date_long)) + 
  ylim(-1, .25) + #(-2, .3)
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  geom_dl(aes(label = name), method = list(cex = 1.2, 'last.bumpup')) +   
  scale_colour_manual(values = c(teal, cardinal, black, green), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "Devs from pre-pandemic flow (% points)",
       size = 7
  )+
  geom_text(label="Feb 2020", x=as.Date('2019-11-01'), y=-.5, size = 6, color = 'black') +
  theme_minimal()+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)
  )
#save plot
ggsave('./figures-tables/fig2a.png', plot = last_plot(), width = 10, height = 8)

## B. Business flows
#construct dataset
temp <- chars %>% filter(MetroShort %in% cities) %>%
  mutate(quantile_rank = 'cbd') %>%
  group_by(MsaName) %>%
  mutate(quantile_rank = replace(quantile_rank, dist_to_cbd >= 2000, ntile(density2019[dist_to_cbd>=2000], 10)),
         category = ifelse(quantile_rank == 'cbd', 'city center',
                           case_when(quantile_rank == 10 ~ 'high density',
                                     quantile_rank %in% c(6:9) ~ 'suburb',
                                     TRUE ~ 'exurb'))) %>%
  inner_join(usps, by = 'zip') %>% 
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date('2018-01-01'), date < as.Date(end_date_long)) %>%
  group_by(zip) %>% mutate(net_bus = na.approx(net_bus, na.rm=FALSE)) %>%
  group_by(category, date) %>%
  summarise(net_bus = sum(net_bus, na.rm = TRUE)/sum(estab_count)*100) %>% 
  group_by(category) %>%
  mutate(net_bus_avg = zoo::rollmean(net_bus, 12, align='right', na.pad=TRUE),
         net_bus = net_bus - net_bus_avg[date == as.Date('2020-02-15')]) %>%
  mutate(emph="b", val = net_bus, name = category)

#make plot
temp %>% ggplot(
  aes(x=date, y = val, color = name, linetype = as.factor(emph), 
      alpha = as.factor(emph),size = as.factor(emph)), guide='none') +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  geom_line()+
  ylim(-3, .5) +
  xlim(as.Date('2018-01-01'), as.Date(end_date_long)) + 
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  geom_dl(aes(label = name), method = list(cex = 1.2, 'last.bumpup', cex = 1.2)) +   
  scale_colour_manual(values = c(teal, cardinal, black, green), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "Devs from pre-pandemic flow (% points)",
       size = 7
  )+
  geom_text(label="Feb 2020", x=as.Date('2019-11-01'), y=-1, size = 6, color = 'black') +
  theme_minimal()+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)
  )
#save plot
ggsave('./figures-tables/fig2b.png', plot = last_plot(), width = 10, height = 8)

###########################################
## Fig 3 and Appendix A6: USPS heat maps
###########################################

#https://arilamstein.com/documentation/choroplethrZip/reference/zip_choropleth.html
#install_github('arilamstein/choroplethrZip@v1.5.0')
#May require a recent version of R (3.6.2 or greater) to run
library(choroplethrZip)

#create dataset in format required for choroplehtrZip
df5 <- read_csv('./data/usps_panel_zips.csv', col_types = cols(zip = col_character())) %>%
  mutate(value = round(post_pop, digits = 3),
         zip = ifelse(nchar(zip)==4, paste('0', zip, sep=''), zip)) %>%
  dplyr::select(zip, value) %>%
  rename(region = zip) %>%
  filter(!is.na(zip), !is.na(value))


## Create New York map
df_choro <- df5
df_choro$value <- cut(df5$value, breaks = c(-45, -5, -2, -.5, .05, 2, 4, 100))
choro = ZipChoropleth$new(df_choro)
choro$ggplot_scale = scale_fill_brewer(name="Net inflows/pop (%)", palette = 'RdYlGn')
choro$set_zoom_zip(state_zoom=NULL, county_zoom=NULL, msa_zoom='New York-Newark-Jersey City, NY-NJ-PA', zip_zoom=NULL)
choro$render()
ggsave('./figures-tables/fig3a.png', plot = last_plot(), width = 10, height = 8)

## Create San Francisco map
df_choro <- df5
df_choro$value <- cut(df5$value, breaks = c(-35, -10, -5, -2, 0, .5, 2, 10))
choro = ZipChoropleth$new(df_choro)
choro$ggplot_scale = scale_fill_brewer(name="Net inflows/pop (%)", palette = 'RdYlGn')
choro$set_zoom_zip(state_zoom=NULL, county_zoom=NULL, msa_zoom='San Francisco-Oakland-Hayward, CA', zip_zoom=NULL)
choro$render()
ggsave('./figures-tables/fig3b.png', plot = last_plot(), width = 10, height = 8)

## Create Boston map
df_choro <- df5
df_choro$value <- cut(df5$value, breaks = c(-45, -5, -1.5, 0, 1, 2, 4, 10))
choro = ZipChoropleth$new(df_choro)
choro$ggplot_scale = scale_fill_brewer(name="Net inflows/pop (%)", palette = 'RdYlGn')
choro$set_zoom_zip(state_zoom=NULL, county_zoom=NULL, msa_zoom='Boston-Cambridge-Newton, MA-NH', zip_zoom=NULL)
choro$render()
ggsave('./figures-tables/appendixa6_a.png', plot = last_plot(), width = 10, height = 8)

## Create Los Angeles map
df_choro <- df5
df_choro$value <- cut(df5$value, breaks = c(-25, -5, -2, -1, 0, 1, 2, 10))
choro = ZipChoropleth$new(df_choro)
choro$ggplot_scale = scale_fill_brewer(name="Net inflows/pop (%)", palette = 'RdYlGn')
choro$set_zoom_zip(state_zoom=NULL, county_zoom=c('6037', '6059'), msa_zoom=NULL, zip_zoom=NULL)
choro$render()
ggsave('./figures-tables/appendixa6_b.png', plot = last_plot(), width = 10, height = 8)

## Create Philly map
df_choro <- df5
df_choro$value <- cut(df5$value, breaks = c(-30, -5, -2, -1, 0, 1, 2, 3, 15))
choro = ZipChoropleth$new(df_choro)
choro$ggplot_scale = scale_fill_brewer(name="Net inflows/pop (%)", palette = 'RdYlGn')
choro$set_zoom_zip(state_zoom=NULL, county_zoom=NULL, msa_zoom='Philadelphia-Camden-Wilmington, PA-NJ-DE-MD', zip_zoom=NULL)
choro$render()
ggsave('./figures-tables/philly.png', plot = last_plot(), width = 10, height = 8)

## Create Atlanta
df_choro <- df5
df_choro$value <- cut(df5$value, breaks = c(-110, -5, -2, -1, 0, 1, 2, 5, 110))
choro = ZipChoropleth$new(df_choro)
choro$ggplot_scale = scale_fill_brewer(name="Net inflows/pop (%)", palette = 'RdYlGn')
choro$set_zoom_zip(state_zoom=NULL, county_zoom=NULL, msa_zoom='Atlanta-Sandy Springs-Roswell, GA', zip_zoom=NULL)
choro$render()
ggsave('./figures-tables/atl.png', plot = last_plot(), width = 10, height = 8)

###########################################
## Figure 4: heterogeneity in home price index across metros
###########################################
#read in zip code level home value index for single family homes
df3 <- read_csv('https://files.zillowstatic.com/research/public_csvs/zhvi/Zip_zhvi_uc_sfr_tier_0.33_0.67_sm_sa_month.csv?t=1631634893', 
                col_types = cols(RegionName = col_double())) %>% 
  rename(zip = 'RegionName', MsaShort = 'Metro') %>%
  mutate(MetroShort = sub("-.*", "", MsaShort),
         MetroShort = paste(MetroShort, State, sep = ', ')) %>%
  dplyr::select(!c(RegionID, SizeRank, RegionType, StateName, State, City, CountyName, MsaShort)) %>%
  pivot_longer(!c(zip, MetroShort), names_to = 'date', values_to = 'zhvi') %>%
  filter(date >= as.Date('2018-01-01'), date < as.Date(end_date)) %>%
  group_by(zip) %>% mutate(zhvi = na.approx(zhvi, na.rm=FALSE),
                           triple = ifelse(zhvi/lag(zhvi, 12) > 3, 1, 0),
                           count_na = sum(is.na(zhvi)), num_vals=n()) %>%
  group_by(zip) %>% filter(all(triple==0 | is.na(triple)), 
                           count_na/num_vals<.5) ### filter out rows with 3x growth in rents (happens when there are several consecutive missing observations that zillow imputes)

#read in zip code characteristics
chars <- read_csv('./data/zip_all_chars_cbd.csv')

#read in metro level characteristics
msa_chars <- msa_chars <- read_csv('./data/msa_all_chars.csv') %>% filter(!is.na(MsaName)) %>%
  mutate(msa_pop_rank = dense_rank(desc(`2019 Population`)),
         msa_pop_group = case_when(msa_pop_rank %in% c(1:12) ~ 'top12',
                                   msa_pop_rank %in% c(13:50) ~ 'mid',
                                   TRUE ~ 'low'))

## grip_plot is a function that filter metros on the msa_group variable to the msa_level,
## and then plots zip code level series that are aggregated to the zip_group level
#@param: msa_group = which msa-level variable we want to break plot down by
#@param: msa_level = which level of the msa_group variable we want to create the plot for
#@param: zip_group = the zip code level grouping variable to break down time series by

grid_plot <- function (msa_group, msa_level, zip_group) {
  options(repr.plot.width=12, repr.plot.height=5)
  temp <- msa_chars %>% filter(get(msa_group) == msa_level) %>%
    dplyr::select(MetroShort, msa_pop_rank) %>%
    inner_join(chars, by = 'MetroShort') %>% 
    filter(!is.na(`2019 Population`), !is.na(dist_to_cbd)) %>%
    mutate(quantile_rank = 'cbd') %>%
      group_by(MsaName) %>%
      mutate(quantile_rank = replace(quantile_rank, dist_to_cbd >= 2000, ntile(get(zip_group)[dist_to_cbd>=2000], 10)),
             category = ifelse(quantile_rank == 'cbd', 'city center',
                               case_when(quantile_rank == 10 ~ 'high density',
                                         quantile_rank %in% c(6:9) ~ 'suburb',
                                         TRUE ~ 'exurb'))) %>%
    inner_join(df3, by = 'zip') %>% 
    mutate(date = as.Date(date),
           zhvi_pop = zhvi*`2019 Population`) %>%
    group_by(zip) %>% mutate(zhvi_pop = na.approx(zhvi_pop, na.rm=FALSE)) %>% 
    group_by(category, date) %>%
    summarise(zhvi_pop = sum(zhvi_pop, na.rm = TRUE),
              population = sum(`2019 Population`, na.rm = TRUE)) %>% 
    mutate(zhvi = zhvi_pop/population) %>%
    group_by(category) %>% mutate(zhvi = zhvi/zhvi[date == as.Date('2020-02-29')]*100) %>%
    mutate(
      val = zhvi, type = category, emph = "b"
    ) 
  return(temp %>% ggplot(
    aes(x=date, y = val, color = type, linetype = as.factor(emph), 
        alpha = as.factor(emph),size = as.factor(emph)), guide='none') +
      scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
      geom_line()+
      xlim(as.Date('2018-01-01'), as.Date(end_date_long)) + 
      geom_vline(xintercept=as.Date('2020-02-29'), size=.5, color="black") + 
      geom_dl(aes(label = type), method = list(cex = 1,'last.bumpup')) +   
      scale_colour_manual(values = c(teal, cardinal, black, green), guide='none')+
      scale_size_manual(values = c(1.5, 0.8), guide="none")+
      scale_alpha_manual(values = c(1, 0.8), guide="none")+
      scale_linetype(guide='none') +
      labs(title = ifelse(msa_level == 'top12', 'Top 12 metros', ifelse(msa_level=='mid', 'Metros 13-50', 'Metros 51-365')), 
           x  = "", 
           y = ifelse(msa_level=='top12', 'Home Value Index (Feb 2020=100)', '')
      )+
      geom_text(label="Feb 2020", x=as.Date('2019-08-01'), y=105, size = 5, color = 'black') +
      coord_cartesian(clip = "off") + 
      theme_minimal()+
      theme(text = element_text(size=16),
            plot.title= element_text(hjust = 0.5, family = "serif"),
            plot.caption = element_text(size = 8),
            plot.margin = margin(t = 0, r = 10, b = 0, l = 5, unit = "pt"),
            axis.text.x = element_text(size = 16),
            axis.text.y = element_text(size = 16)
      )
  )
}

a <- grid_plot('msa_pop_group', 'top12', 'density2019') #top 12 metros
b <- grid_plot('msa_pop_group', 'mid', 'density2019') #metros 13-50
c <- grid_plot('msa_pop_group', 'low', 'density2019') #metros 51-365

plot_grid(a, NULL, b, NULL, c, NULL, rel_widths = c(1, .1, 1, .1, 1, .13), 
          rel_heights = c(1, .1, 1, .1, 1, .1), nrow = 1, ncol = 6)
ggsave('./figures-tables/fig4.png', plot = last_plot(), width = 15, height = 6)


###########################################
## Figure 5: heterogeneity in population flows
###########################################
#read in zip code characteristics
chars <- read_csv('./data/zip_all_chars_cbd.csv', 
                  col_types = cols('zip' = col_integer())) %>% dplyr::select(!estab_count)
#read in business establishment counts
bus_chars <- read_csv('./data/zbp_wfh.csv',
                      col_types = cols('zip' = col_integer()))
chars <- chars %>% inner_join(bus_chars, by = 'zip')

#read in zip code month level USPS flow data
usps <- read_csv('./data/USPS_zips.csv')

msa_chars <- msa_chars <- read_csv('./data/msa_all_chars.csv') %>% filter(!is.na(MsaName)) %>%
  mutate(msa_pop_rank = dense_rank(desc(`2019 Population`)),
         msa_pop_group = case_when(msa_pop_rank %in% c(1:12) ~ 'top12',
                                   msa_pop_rank %in% c(13:50) ~ 'mid',
                                   TRUE ~ 'low'))
## grip_plot is a function that filter metros on the msa_group variable to the msa_level,
## and then plots zip code level series that are aggregated to the zip_group level
#@param: msa_group = which msa-level variable we want to break plot down by
#@param: msa_level = which level of the msa_group variable we want to create the plot for
#@param: zip_group = the zip code level grouping variable to break down time series by

grid_plot <- function (msa_group, msa_level, zip_group) {
  options(repr.plot.width=12, repr.plot.height=5)
  temp <- msa_chars %>% filter(get(msa_group) == msa_level) %>%
    dplyr::select(MetroShort, msa_pop_rank) %>%
    inner_join(chars, by = 'MetroShort') %>% 
    filter(!is.na(`2019 Population`), !is.na(dist_to_cbd)) %>%
    mutate(quantile_rank = 'cbd') %>%
    group_by(MsaName) %>%
    mutate(quantile_rank = replace(quantile_rank, dist_to_cbd >= 2000, ntile(get(zip_group)[dist_to_cbd>=2000], 10)),
           category = ifelse(quantile_rank == 'cbd', 'city center',
                             case_when(quantile_rank == 10 ~ 'high density',
                                       quantile_rank %in% c(6:9) ~ 'suburb',
                                       TRUE ~ 'exurb'))) %>%
    inner_join(usps, by = 'zip') %>% 
    mutate(date = as.Date(date)) %>%
    filter(date >= as.Date('2018-01-01'), date < as.Date(end_date)) %>%
    group_by(zip) %>% mutate(net_pop = na.approx(net_pop, na.rm=FALSE)) %>%
    group_by(category, date) %>%
    summarise(net_pop = sum(net_pop, na.rm = TRUE)/sum(`2019 Population`)*100) %>% 
    group_by(category) %>%
    mutate(net_pop_avg = zoo::rollmean(net_pop, 12, align='right', na.pad=TRUE),
           net_pop = net_pop - net_pop_avg[date == as.Date('2020-02-15')]) %>%
    mutate(
      val = net_pop, type = category, emph = "b"
    )
  return(temp %>% ggplot(
    aes(x=date, y = val, color = type, linetype = as.factor(emph), 
        alpha = as.factor(emph),size = as.factor(emph)), guide='none') +
      scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
      geom_line()+
      xlim(as.Date('2018-01-01'), as.Date(end_date_long)) + 
      ylim(-1, .25) + 
      geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
      geom_dl(aes(label = type), method = list(cex = 1,'last.bumpup')) +   
      scale_colour_manual(values = c(teal, cardinal, black, green), guide='none')+
      scale_size_manual(values = c(1.5, 0.8), guide="none")+
      scale_alpha_manual(values = c(1, 0.8), guide="none")+
      scale_linetype(guide='none') +
      labs(title = ifelse(msa_level == 'top12', 'Top 12 Metros', ifelse(msa_level=='mid', 'Metros 13-50', 'Metros 51-365')), 
           x  = "", 
           y = ifelse(msa_level=='top12', "Devs from pre-pandemic flow (% points)", '')
      )+
      geom_text(label="Feb 2020", x=as.Date('2019-08-01'), y=105, size = 5, color = 'black') +
      coord_cartesian(clip = "off") + 
      theme_minimal()+
      theme(text = element_text(size=16),
            plot.title= element_text(hjust = 0.5, family = "serif"),
            plot.caption = element_text(size = 8),
            plot.margin = margin(t = 0, r = 10, b = 0, l = 5, unit = "pt"),
            axis.text.x = element_text(size = 16),
            axis.text.y = element_text(size = 16)
      )
  )
}

a <- grid_plot('msa_pop_group', 'top12', 'density2019') #top 12 metros
b <- grid_plot('msa_pop_group', 'mid', 'density2019') #metros 13-50
c <- grid_plot('msa_pop_group', 'low', 'density2019') #metros 51-365

plot_grid(a, NULL, b, NULL, c, NULL, rel_widths = c(1, .1, 1, .1, 1, .13), 
          rel_heights = c(1, .1, 1, .1, 1, .1), nrow = 1, ncol = 6)
ggsave('./figures-tables/fig5.png', plot = last_plot(), width = 15, height = 6)


###########################################
## Figure 7 metro level pop/bus flow charts
###########################################

## Figure 7(a) msa level chart for population flows
msa_chars <- msa_chars <- read_csv('./data/msa_all_chars.csv') %>% filter(!is.na(MsaName)) %>%
  mutate(msa_pop_rank = dense_rank(desc(`2019 Population`)),
         msa_pop_group = case_when(msa_pop_rank %in% c(1:12) ~ 'top12',
                                   msa_pop_rank %in% c(13:50) ~ '13-50',
                                   TRUE ~ '51+')) %>%
  dplyr::select(MetroShort, msa_pop_group)

#read in all zip code characteristics
chars <- read_csv('./data/zip_all_chars_cbd.csv') %>% filter(!is.na(`2019 Population`))

#combine chars
chars_all <- chars %>% filter(!is.na(`2019 Population`)) %>% left_join(msa_chars, by='MetroShort') %>%
  mutate(msa_pop_group=replace_na(msa_pop_group, 'rural'))
#read in zip code level USPS flow data
usps <- read_csv('./data/USPS_zips.csv')

## A. population flows
#construct dataset
temp <- chars_all %>% inner_join(usps, by = 'zip') %>% 
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date('2018-01-01'), date < as.Date(end_date)) %>%
  group_by(zip) %>% mutate(net_pop = na.approx(net_pop, na.rm=FALSE)) %>%
  group_by(msa_pop_group, date) %>%
  summarise(net_pop = sum(net_pop, na.rm = TRUE)/sum(`2019 Population`)*100) %>%
  group_by(msa_pop_group) %>% 
         mutate(net_pop_avg = zoo::rollmean(net_pop, 12, align='right', na.rm=TRUE, na.pad=TRUE),
                net_pop = cumsum(net_pop - net_pop_avg[date == as.Date('2020-02-15')]),
                net_pop = net_pop - net_pop[date==as.Date('2020-02-15')]) %>%
  mutate(emph="b", val = net_pop, name = msa_pop_group)

#make plot
temp %>% ggplot(
  aes(x=date, y = val, color = name, linetype = as.factor(emph), 
      alpha = as.factor(emph),size = as.factor(emph)), guide='none') +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  geom_line()+
  xlim(as.Date('2018-01-01'), as.Date(end_date_long)) + 
  ylim(-1, 2) + #(-.2, 2)
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  geom_dl(aes(label = name), method = list('last.bumpup', cex = 1.2)) +   
  scale_colour_manual(values = c(teal, cardinal, black, green, marmalade), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "Cumulative devs from pre-pandemic flow (% points)",
       size = 7
  )+
  geom_text(label="Feb 2020", x=as.Date('2020-03-01'), y=1.5, size = 6, color = 'black') +
  theme_minimal()+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)
  )
#save plot
ggsave('./figures-tables/fig7a.png', plot = last_plot(), width = 10, height = 8)

##B. Business flows
#construct dataset
temp <- chars_all %>% inner_join(usps, by = 'zip') %>% 
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date('2018-01-01'), date < as.Date(end_date)) %>%
  group_by(zip) %>% mutate(net_bus = na.approx(net_bus, na.rm=FALSE)) %>%
  group_by(msa_pop_group, date) %>%
  summarise(net_bus = sum(net_bus, na.rm = TRUE)/sum(`2019 Population`)*100) %>% 
  group_by(msa_pop_group) %>% 
  mutate(net_bus_avg = zoo::rollmean(net_bus, 12, align='right', na.pad=TRUE),
         net_bus = cumsum(net_bus - net_bus_avg[date == as.Date('2020-02-15')]),
         net_bus = net_bus - net_bus[date==as.Date('2020-02-15')]) %>%
  mutate(emph="b", val = net_bus, name = msa_pop_group)

#make plot
temp %>% ggplot(
  aes(x=date, y = val, color = name, linetype = as.factor(emph), 
      alpha = as.factor(emph),size = as.factor(emph)), guide='none') +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  geom_line()+
  xlim(as.Date('2018-01-01'), as.Date(end_date_long)) + 
  ylim(-.05, .05) + #(-.2, 2)
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  geom_dl(aes(label = name), method = list('last.bumpup', cex = 1.2)) +   
  scale_colour_manual(values = c(teal, cardinal, black, green, marmalade), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "Cumulative devs from pre-pandemic flow (% points)",
       size = 7
  )+
  geom_text(label="Feb 2020", x=as.Date('2019-11-01'), y=0.025, size = 6, color = 'black') +
  theme_minimal()+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)
  )
#save plot
ggsave('./figures-tables/fig7b.png', plot = last_plot(), width = 10, height = 8)


###########################################
## Figure 8 metro level dispersion charts | SF, NY breakout
###########################################

msa_chars <- msa_chars <- read_csv('./data/msa_all_chars.csv') %>% filter(!is.na(MsaName)) %>%
  mutate(msa_pop_rank = dense_rank(desc(`2019 Population`)),
         msa_pop_group = case_when(msa_pop_rank %in% c(1:12) ~ 'top12',
                                   msa_pop_rank %in% c(13:50) ~ '13-50',
                                   TRUE ~ '51+'),
         msa_pop_group = ifelse(MetroShort %in% c('New York, NY', 'San Francisco, CA'), 'NY_SF', msa_pop_group)) %>%
  dplyr::select(MetroShort, msa_pop_group)

#read in all zip code characteristics
chars <- read_csv('./data/zip_all_chars_cbd.csv') %>% filter(!is.na(`2019 Population`))

#combine chars
chars_all <- chars %>% filter(!is.na(`2019 Population`)) %>% left_join(msa_chars, by='MetroShort') %>%
  mutate(msa_pop_group=replace_na(msa_pop_group, 'rural'))

## Figure 8(a) msa level chart for housing rents
#read in zip code level rental index from Zillow
df <- read_csv('https://files.zillowstatic.com/research/public_csvs/zori/Zip_zori_sm_month.csv?t=1666834047', 
               col_types = cols(RegionName = col_double())) %>% rename(zip = 'RegionName') %>%
  dplyr::select(!c(RegionID, SizeRank, RegionType, StateName, State, City, Metro, CountyName)) %>% 
  pivot_longer(!zip, names_to = 'date', values_to = 'zori') %>%
  mutate(date = as.Date(as.yearmon(date)) + 14) %>%
  filter(date >= as.Date('2018-01-01'), date < as.Date(end_date)) %>%
  group_by(zip) %>% mutate(zori = na.approx(zori, na.rm=FALSE, rule = 2),
                           triple = ifelse(zori/lag(zori, 12) > 3, 1, 0),
                           count_na = sum(is.na(zori)), num_vals=n()) %>%
  group_by(zip) %>% filter(all(triple==0 | is.na(triple)), # filter out rows with 3x growth in rents (happens when there are several consecutive missing observations that zillow imputes)
                           count_na/num_vals<.5) # filter out rows with more than half missing values

#construct dataset to plot
temp <- chars_all %>% inner_join(df, by = 'zip') %>% 
  mutate(date = as.Date(date),
         zori_pop = zori*`2019 Population`) %>%
  filter(date >= as.Date('2018-01-01'), date < as.Date(end_date)) %>%
  group_by(msa_pop_group, date) %>%
  summarise(zori_pop = sum(zori_pop, na.rm = TRUE),
            population = sum(`2019 Population`, na.rm = TRUE)) %>% 
  mutate(zori = zori_pop/population) %>%
  group_by(msa_pop_group) %>% mutate(zori = zori/zori[date == as.Date('2020-02-15')]*100) %>%
  mutate(
    val = zori, name = msa_pop_group,
    type = case_when(str_detect(name, 'NY_SF') ~ 'NY/SF',
                     str_detect(name, 'top12') ~ 'Other top 12',
                     TRUE ~ name), emph = "b")

#make plot
temp %>% ggplot(
  aes(x=date, y = val, color = type, linetype = as.factor(emph), 
      alpha = as.factor(emph),size = as.factor(emph)), guide='none') +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  geom_line()+
  xlim(as.Date('2018-01-01'), as.Date(end_date_long)) + 
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  geom_dl(aes(label = type), method = list(cex = 1.2,'last.bumpup')) +  
  scale_colour_manual(values = c(teal, cardinal, black, green, marmalade), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "Rental Index (Feb 2020=100)",
       size = 10
  )+
  geom_text(label="Feb 2020", x=as.Date('2019-12-01'), y=95, size = 6, color = 'black',
  ) +
  theme_minimal()+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)
  )

#save file
ggsave('./figures-tables/fig8a.png', plot = last_plot(), width = 10, height = 8)

## Figure 8(b) msa level chart for housing prices

#read zip code level home value index for single family homes from Zillow
df2 <- read_csv('https://files.zillowstatic.com/research/public_csvs/zhvi/Zip_zhvi_uc_sfr_tier_0.33_0.67_sm_sa_month.csv?t=1631634893', 
                col_types = cols(RegionName = col_double())) %>% 
  rename(zip = 'RegionName', MsaShort = 'Metro') %>%
  mutate(MetroShort = sub("-.*", "", MsaShort),
         MetroShort = paste(MetroShort, State, sep = ', ')) %>%
  dplyr::select(!c(RegionID, SizeRank, RegionType, StateName, State, City, CountyName, MsaShort)) %>%
  pivot_longer(!c(zip, MetroShort), names_to = 'date', values_to = 'zhvi') %>%
  filter(date >= as.Date('2018-01-01'), date < as.Date(end_date)) %>%
  group_by(zip) %>% mutate(zhvi = na.approx(zhvi, na.rm=FALSE),
                           triple = ifelse(zhvi/lag(zhvi, 12) > 3, 1, 0),
                           count_na = sum(is.na(zhvi)),
                           num_vals=n()) %>%
  group_by(zip) %>% filter(all(triple==0 | is.na(triple)), 
                           count_na/num_vals<.5) ### filter out rows with 3x growth in rents (happens when there are several consecutive missing observations that zillow imputes)

#construct dataset
temp <- chars_all %>% inner_join(df2, by = 'zip') %>% 
  mutate(date = as.Date(date),
         zhvi_pop = zhvi*`2019 Population`) %>%
  group_by(zip) %>% mutate(zhvi_pop = na.approx(zhvi_pop, na.rm=FALSE)) %>% 
  group_by(msa_pop_group, date) %>%
  summarise(zhvi_pop = sum(zhvi_pop, na.rm = TRUE),
            population = sum(`2019 Population`, na.rm = TRUE)) %>% 
  mutate(zhvi = zhvi_pop/population) %>%
  group_by(msa_pop_group) %>% mutate(zhvi = zhvi/zhvi[date == as.Date('2020-02-29')]*100) %>%
  mutate(
    val = zhvi, name = msa_pop_group,
    type = case_when(str_detect(name, 'NY_SF') ~ 'NY/SF',
                     str_detect(name, 'top12') ~ 'Other top 12',
                     TRUE ~ name), emph = "b")

temp %>% ggplot(
  aes(x=date, y = val, color = type, linetype = as.factor(emph), 
      alpha = as.factor(emph),size = as.factor(emph)), guide='none') +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  geom_line()+
  xlim(as.Date('2018-01-01'), as.Date(end_date_long)) + 
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  geom_dl(aes(label = type), method = list(cex = 1.2,'last.bumpup')) +   
  scale_colour_manual(values = c(teal, cardinal, black, green, marmalade), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "Home Value Index (Feb 2020=100)",
       size = 10
  )+
  geom_text(label="Feb 2020", x=as.Date('2019-12-01'), y=95, size = 6, color = 'black',
  ) +
  theme_minimal()+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)
  )

#save plot
ggsave('./figures-tables/fig8b.png', plot = last_plot(), width = 10, height = 8)

## Figure 8(c) msa level chart for population flows
#read in zip code level USPS flow data
usps <- read_csv('./data/USPS_zips.csv')

#construct dataset
temp <- chars_all %>% inner_join(usps, by = 'zip') %>% 
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date('2018-01-01'), date < as.Date(end_date)) %>%
  group_by(zip) %>% mutate(net_pop = na.approx(net_pop, na.rm=FALSE)) %>%
  group_by(msa_pop_group, date) %>%
  summarise(net_pop = sum(net_pop, na.rm = TRUE)/sum(`2019 Population`)*100) %>% 
  group_by(msa_pop_group) %>% 
  mutate(net_pop_avg = zoo::rollmean(net_pop, 12, align='right', na.pad=TRUE),
         net_pop = cumsum(net_pop - net_pop_avg[date == as.Date('2020-02-15')]),
         net_pop = net_pop - net_pop[date==as.Date('2020-02-15')]) %>%
  mutate(emph="b", val = net_pop, type = msa_pop_group)

#make plot
temp %>% ggplot(
  aes(x=date, y = val, color = type, linetype = as.factor(emph), 
      alpha = as.factor(emph), size = as.factor(emph)), guide='none') +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  geom_line()+
  xlim(as.Date('2018-01-01'), as.Date(end_date_long)) + 
  ylim(-2, 2) + #(-.2, 2)
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  geom_dl(aes(label = type), method = list(cex = 1.2,'last.bumpup')) + 
  scale_colour_manual(values = c(teal, cardinal, black, green, marmalade), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "Cumulative devs from pre-pandemic flow (% points)",
       size = 7
  )+
  geom_text(label="Feb 2020", x=as.Date('2019-11-01'), y=-1.5, size = 6, color = 'black') +
  theme_minimal()+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)
  )
#save plot
ggsave('./figures-tables/fig8c.png', plot = last_plot(), width = 10, height = 8)

###########################################
## Appendix A1: Heterogeneity in rents
###########################################
#read in Zillow rental index data
df <- read_csv('https://files.zillowstatic.com/research/public_csvs/zori/Zip_zori_sm_month.csv?t=1666834047', 
               col_types = cols(RegionName = col_double())) %>% rename(zip = 'RegionName') %>%
  dplyr::select(!c(RegionID, SizeRank, RegionType, StateName, State, City, Metro, CountyName)) %>% 
  pivot_longer(!zip, names_to = 'date', values_to = 'zori') %>%
  mutate(date = as.Date(as.yearmon(date)) + 14) %>%
  filter(date >= as.Date('2018-01-01'), date < as.Date(end_date)) %>%
  group_by(zip) %>% mutate(zori = na.approx(zori, na.rm=FALSE),
                           triple = ifelse(zori/lag(zori, 12) > 3, 1, 0),
                           count_na = sum(is.na(zori)),
                           num_vals=n()) %>%
  group_by(zip) %>% filter(all(triple==0 | is.na(triple)), # filter out rows with 3x growth in rents (happens when there are several consecutive missing observations that zillow imputes)
                           count_na/num_vals<.5) # filter out rows with more than half missing values

#read in zip code characteristics
chars <- read_csv('./data/zip_all_chars_cbd.csv')

#read in metro level characteristics
msa_chars <- msa_chars <- read_csv('./data/msa_all_chars.csv') %>% filter(!is.na(MsaName)) %>%
  mutate(msa_pop_rank = dense_rank(desc(`2019 Population`)),
         msa_pop_group = case_when(msa_pop_rank %in% c(1:12) ~ 'top12',
                                   msa_pop_rank %in% c(13:50) ~ 'mid',
                                   TRUE ~ 'low'))

## grip_plot is a function that filter metros on the msa_group variable to the msa_level,
## and then plots zip code level series that are aggregated to the zip_group level
#@param: msa_group = which msa-level variable we want to break plot down by
#@param: msa_level = which level of the msa_group variable we want to create the plot for
#@param: zip_group = the zip code level grouping variable to break down time series by

grid_plot <- function (msa_group, msa_level, zip_group) {
  options(repr.plot.width=12, repr.plot.height=5)
  temp <- msa_chars %>% filter(get(msa_group) == msa_level) %>%
    dplyr::select(MetroShort, msa_pop_rank) %>%
    inner_join(chars, by = 'MetroShort') %>% 
    filter(!is.na(`2019 Population`), !is.na(dist_to_cbd)) %>%
    mutate(quantile_rank = 'cbd') %>%
    group_by(MsaName) %>%
    mutate(quantile_rank = replace(quantile_rank, dist_to_cbd >= 2000, ntile(get(zip_group)[dist_to_cbd>=2000], 10)),
           category = ifelse(quantile_rank == 'cbd', 'city center',
                             case_when(quantile_rank == 10 ~ 'high density',
                                       quantile_rank %in% c(6:9) ~ 'suburb',
                                       TRUE ~ 'exurb'))) %>%
    inner_join(df, by = 'zip') %>% 
    mutate(date = as.Date(date),
           zori_pop = zori*`2019 Population`) %>%
    group_by(category, date) %>%
    summarise(zori_pop = sum(zori_pop, na.rm = TRUE),
              population = sum(`2019 Population`, na.rm = TRUE)) %>% 
    mutate(zori = zori_pop/population) %>%
    group_by(category) %>% mutate(zori = zori/zori[date == as.Date('2020-02-15')]*100) %>%
    mutate(
      val = zori, type = category, emph = "b"
    )
  return(temp %>% ggplot(
    aes(x=date, y = val, color = type, linetype = as.factor(emph), 
        alpha = as.factor(emph),size = as.factor(emph)), guide='none') +
      scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
      geom_line()+
      xlim(as.Date('2018-01-01'), as.Date(end_date_long)) + 
      geom_vline(xintercept=as.Date('2020-02-29'), size=.5, color="black") + 
      geom_dl(aes(label = type), method = list(cex = 1,'last.bumpup')) +   
      scale_colour_manual(values = c(teal, cardinal, black, green), guide='none')+
      scale_size_manual(values = c(1.5, 0.8), guide="none")+
      scale_alpha_manual(values = c(1, 0.8), guide="none")+
      scale_linetype(guide='none') +
      labs(title = ifelse(msa_level == 'top12', 'Top 12 metros ', ifelse(msa_level=='mid', 'Metros 13-50', 'Metros 51-100')), 
           x  ="", 
           y = ifelse(msa_level=='top12', 'Rental Index (Feb 2020=100)', '')
      )+
      geom_text(label="Feb 2020", x=as.Date('2019-06-01'), y=105, size = 5, color = 'black') +
      coord_cartesian(clip = "off") + 
      theme_minimal()+
      theme(text = element_text(size=16),
            plot.title= element_text(hjust = 0.5, family = "serif"),
            plot.caption = element_text(size = 8),
            plot.margin = margin(t = 0, r = 10, b = 0, l = 5, unit = "pt"),
            axis.text.x = element_text(size = 16),
            axis.text.y = element_text(size = 16)
      )
  )
}

a <- grid_plot('msa_pop_group', 'top12', 'density2019') #top 12 metros
b <- grid_plot('msa_pop_group', 'mid', 'density2019') #metros 13-50
c <- grid_plot('msa_pop_group', 'low', 'density2019') #metros 51-100

plot_grid(a, NULL, b, NULL, c, NULL, rel_widths = c(1, .1, 1, .1, 1, .13), 
          rel_heights = c(1, .1, 1, .1, 1, .1), nrow = 1, ncol = 6)
ggsave('./figures-tables/appendix_a1.png', plot = last_plot(), width = 15, height = 6)

###########################################
## Appendix A3: equal weighting
###########################################
## Part A. rental index
#read in Zillow rental index data
df <- read_csv('https://files.zillowstatic.com/research/public_csvs/zori/Zip_zori_sm_month.csv?t=1666834047', 
               col_types = cols(RegionName = col_double())) %>% rename(zip = 'RegionName') %>%
  dplyr::select(!c(RegionID, SizeRank, RegionType, StateName, State, City, Metro, CountyName)) %>% 
  pivot_longer(!zip, names_to = 'date', values_to = 'zori') %>%
  mutate(date = as.Date(as.yearmon(date)) + 14) %>%
  filter(date >= as.Date('2018-01-01'), date < as.Date(end_date)) %>%
  group_by(zip) %>% mutate(zori = na.approx(zori, na.rm=FALSE),
                           triple = ifelse(zori/lag(zori, 12) > 3, 1, 0),
                           count_na = sum(is.na(zori)),
                           num_vals=n()) %>%
  group_by(zip) %>% filter(all(triple==0 | is.na(triple)), # filter out rows with 3x growth in rents (happens when there are several consecutive missing observations that zillow imputes)
                           count_na/num_vals<.5) # filter out rows with more than half missing values

#read in zip code characteristics
chars <- read_csv('./data/zip_all_chars_cbd.csv')

#create dataset
temp <- chars %>% filter(MetroShort %in% cities, !is.na(dist_to_cbd)) %>%
  mutate(quantile_rank = 'cbd') %>%
  group_by(MsaName) %>%
  mutate(quantile_rank = replace(quantile_rank, dist_to_cbd >= 2000, ntile(density2019[dist_to_cbd>=2000], 10)),
         category = ifelse(quantile_rank == 'cbd', 'city center',
                           case_when(quantile_rank == 10 ~ 'high density',
                                     quantile_rank %in% c(6:9) ~ 'suburb',
                                     TRUE ~ 'exurb'))) %>%
  inner_join(df, by = 'zip') %>% 
  mutate(date = as.Date(date)) %>%
  group_by(zip) %>% mutate(zori = zori/zori[date == as.Date('2020-02-15')]*100) %>%
  mutate(zori_pop = zori*`2019 Population`) %>%
  group_by(category, date) %>%
  summarise(zori_pop = sum(zori_pop, na.rm = TRUE),
            population = sum(`2019 Population`, na.rm = TRUE)) %>% 
  mutate(val = zori_pop/population,
         type = category, emph = "b")

temp %>% ggplot(
  aes(x=date, y = val, color = type, linetype = as.factor(emph), 
      alpha = as.factor(emph), size = as.factor(emph)), guide='none') +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  geom_line()+
  xlim(as.Date('2018-01-01'), as.Date(end_date_long)) + 
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  geom_dl(aes(label = type), method = list(cex = 1.2,'last.bumpup')) +   
  scale_colour_manual(values = c(teal, cardinal, black, green), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "Rental Index (Feb 2020=100)",
       size = 7
  )+
  geom_text(label="Feb 2020", x=as.Date('2019-12-01'), y=104, size = 6, color = 'black',
  ) +
  theme_minimal()+
  theme(text = element_text(size=24),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 24)
  )
#save plot
ggsave('./figures-tables/appendix_a3a.png', plot = last_plot(), width = 10, height = 8)


## Part B. home value index
#read in zip code level Zillow home value index data for single family homes
df2 <- read_csv('https://files.zillowstatic.com/research/public_csvs/zhvi/Zip_zhvi_uc_sfr_tier_0.33_0.67_sm_sa_month.csv', 
                col_types = cols(RegionName = col_double())) %>% 
  rename(zip = 'RegionName', MsaShort = 'Metro') %>%
  mutate(MetroShort = sub("-.*", "", MsaShort),
         MetroShort = paste(MetroShort, State, sep = ', ')) %>%
  dplyr::select(!c(RegionID, SizeRank, RegionType, StateName, State, City, CountyName, MsaShort)) %>%
  pivot_longer(!c(zip, MetroShort), names_to = 'date', values_to = 'zhvi') %>%
  filter(date >= as.Date('2018-01-01'), date < as.Date(end_date)) %>%
  group_by(zip) %>% mutate(zhvi = na.approx(zhvi, na.rm=FALSE),
                           triple = ifelse(zhvi/lag(zhvi, 12) > 3, 1, 0),
                           count_na = sum(is.na(zhvi)),
                           num_vals=n()) %>%
  group_by(zip) %>% filter(all(triple==0 | is.na(triple)), 
                           count_na/num_vals<.5) ### filter out rows with 3x growth in rents (happens when there are several consecutive missing observations that zillow imputes)

#read in zip code characteristics
chars <- read_csv('./data/zip_all_chars_cbd.csv')

#create dataset
temp <- chars %>% filter(MetroShort %in% cities, !is.na(density2019), !is.na(dist_to_cbd), !is.na(`2019 Population`)) %>%
  mutate(quantile_rank = 'cbd') %>%
  group_by(MsaName) %>%
  mutate(quantile_rank = replace(quantile_rank, dist_to_cbd >= 2000, ntile(density2019[dist_to_cbd>=2000], 10)),
         category = ifelse(quantile_rank == 'cbd', 'city center',
                           case_when(quantile_rank == 10 ~ 'high density',
                                     quantile_rank %in% c(6:9) ~ 'suburb',
                                     TRUE ~ 'exurb'))) %>%
  inner_join(df2, by = 'zip') %>% 
  mutate(date = as.Date(date)) %>%
  group_by(zip) %>% mutate(zhvi = na.approx(zhvi, na.rm=FALSE)) %>%
  group_by(zip) %>% mutate(zhvi = zhvi/zhvi[date == as.Date('2020-02-29')]*100) %>%
  mutate(zhvi_pop = zhvi*`2019 Population`) %>%
  group_by(category, date) %>%
  summarise(zhvi_pop = sum(zhvi_pop, na.rm = TRUE),
            population = sum(`2019 Population`, na.rm = TRUE)) %>% 
  mutate(val = zhvi_pop/population,
         type = category, emph = "b")
#make plot
temp %>% ggplot(
  aes(x=date, y = val, color = type, linetype = as.factor(emph), 
      alpha = as.factor(emph),size = as.factor(emph)), guide='none') +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  geom_line()+
  xlim(as.Date('2018-01-01'), as.Date(end_date_long)) + 
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  geom_dl(aes(label = type), method = list(cex = 1.2,'last.bumpup')) +
  scale_colour_manual(values = c(teal, cardinal, black, green), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "Home Value Index (Feb 2020=100)",
       size = 7
  )+
  geom_text(label="Feb 2020", x=as.Date('2020-03-01'), y=104, size = 6, color = 'black') +
  theme_minimal()+
  theme(text = element_text(size=24),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 24)
  )
#save plot
ggsave('./figures-tables/appendix_a3b.png', plot = last_plot(), width = 10, height = 8)


###########################################
## Appendix A4: Past macro shocks
###########################################
## A. 9/11
#read in Zillow home value index
df2 <- read_csv('https://files.zillowstatic.com/research/public_csvs/zhvi/Zip_zhvi_uc_sfr_tier_0.33_0.67_sm_sa_month.csv?t=1635201481', 
                col_types = cols(RegionName = col_double())) %>% 
  rename(zip = 'RegionName', MsaShort = 'Metro') %>%
  mutate(MetroShort = sub("-.*", "", MsaShort),
         MetroShort = paste(MetroShort, State, sep = ', ')) %>%
  dplyr::select(!c(RegionID, SizeRank, RegionType, StateName, State, City, CountyName, MsaShort)) %>%
  pivot_longer(!c(zip, MetroShort), names_to = 'date', values_to = 'zhvi') %>%
  filter(date >= as.Date('2000-07-01'), date < as.Date('2002-07-01')) %>%
  group_by(zip) %>% mutate(zhvi = na.approx(zhvi, na.rm=FALSE),
                           triple = ifelse(zhvi/lag(zhvi, 12) > 3, 1, 0),
                           count_na = sum(is.na(zhvi)),
                           num_vals=n()) %>%
  group_by(zip) %>% filter(all(triple==0 | is.na(triple)), 
                           count_na/num_vals<.5) ### filter out rows with 3x growth in rents (happens when there are several consecutive missing observations that zillow imputes)
#read in zip code characteristics
chars <- read_csv('./data/zip_all_chars_cbd.csv')

#create dataset
temp <- chars %>% filter(MetroShort %in% cities, !is.na(`2010 Population`), !is.na(dist_to_cbd)) %>%
  mutate(quantile_rank = 'cbd') %>%
  group_by(MsaName) %>%
  mutate(quantile_rank = replace(quantile_rank, dist_to_cbd >= 2000, ntile(density2019[dist_to_cbd>=2000], 10)),
         category = ifelse(quantile_rank == 'cbd', 'city center',
                           case_when(quantile_rank == 10 ~ 'high density',
                                     quantile_rank %in% c(6:9) ~ 'suburb',
                                     TRUE ~ 'exurb'))) %>%
  inner_join(df2, by = 'zip') %>% 
  mutate(date = as.Date(date),
         zhvi_pop = zhvi*`2010 Population`) %>%
  group_by(zip) %>% mutate(zhvi_pop = na.approx(zhvi_pop, na.rm=FALSE)) %>% 
  group_by(category, date) %>%
  summarise(zhvi_pop = sum(zhvi_pop, na.rm = TRUE),
            population = sum(`2010 Population`, na.rm = TRUE)) %>% 
  mutate(zhvi = zhvi_pop/population) %>%
  group_by(category) %>% mutate(zhvi = zhvi/zhvi[date == as.Date('2001-06-30')]*100) %>%
  mutate(
    val = zhvi, type = category, emph = "b"
  )

#make plot
temp %>% ggplot(
  aes(x=date, y = val, color = type, linetype = as.factor(emph), 
      alpha = as.factor(emph), size = as.factor(emph)), guide='none') +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  geom_line()+
  xlim(as.Date('2000-07-01'), as.Date('2002-12-30')) + 
  geom_vline(xintercept=as.Date('2001-06-30'), size=.5, color="black") + 
  geom_dl(aes(label = type), method = list(cex = 1, 'last.bumpup', cex = 1.2)) +   
  scale_colour_manual(values = c(teal, cardinal, black, green), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "Home Value Index (Aug 2001=100)",
       size = 7
  )+    geom_text(label="Aug 2001", x=as.Date('2001-06-01'), y=104, size = 6, color = 'black',
  ) +
  theme_minimal()+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)
  )
#save plot
ggsave('./figures-tables/appendix_a4a.png', plot = last_plot(), width = 10, height = 8)

## B. Great Recession
df2 <- read_csv('https://files.zillowstatic.com/research/public_csvs/zhvi/Zip_zhvi_uc_sfr_tier_0.33_0.67_sm_sa_month.csv?t=1635201481', 
                col_types = cols(RegionName = col_double())) %>% 
  rename(zip = 'RegionName', MsaShort = 'Metro') %>%
  mutate(MetroShort = sub("-.*", "", MsaShort),
         MetroShort = paste(MetroShort, State, sep = ', ')) %>%
  dplyr::select(!c(RegionID, SizeRank, RegionType, StateName, State, City, CountyName, MsaShort)) %>%
  pivot_longer(!c(zip, MetroShort), names_to = 'date', values_to = 'zhvi') %>%
  filter(date >= as.Date('2006-01-01'), date < as.Date('2009-01-01')) %>%
  group_by(zip) %>% mutate(zhvi = na.approx(zhvi, na.rm=FALSE),
                           triple = ifelse(zhvi/lag(zhvi, 12) > 3, 1, 0),
                           count_na = sum(is.na(zhvi)),
                           num_vals=n()) %>%
  group_by(zip) %>% filter(all(triple==0 | is.na(triple)), 
                           count_na/num_vals<.5) ### filter out rows with 3x growth in rents (happens when there are several consecutive missing observations that zillow imputes)
#create dataset
temp <- chars %>% filter(MetroShort %in% cities, !is.na(`2010 Population`), !is.na(dist_to_cbd)) %>%
  mutate(quantile_rank = 'cbd') %>%
  group_by(MsaName) %>%
  mutate(quantile_rank = replace(quantile_rank, dist_to_cbd >= 2000, ntile(density2019[dist_to_cbd>=2000], 10)),
         category = ifelse(quantile_rank == 'cbd', 'city center',
                           case_when(quantile_rank == 10 ~ 'high density',
                                     quantile_rank %in% c(6:9) ~ 'suburb',
                                     TRUE ~ 'exurb'))) %>%
  inner_join(df2, by = 'zip') %>% 
  mutate(date = as.Date(date),
         zhvi_pop = zhvi*`2010 Population`) %>%
  group_by(zip) %>% mutate(zhvi_pop = na.approx(zhvi_pop, na.rm=FALSE)) %>% 
  group_by(category, date) %>%
  summarise(zhvi_pop = sum(zhvi_pop, na.rm = TRUE),
            population = sum(`2010 Population`, na.rm = TRUE)) %>% 
  mutate(zhvi = zhvi_pop/population) %>%
  group_by(category) %>% mutate(zhvi = zhvi/zhvi[date == as.Date('2007-12-31')]*100) %>%
  mutate(
    val = zhvi, type = category, emph = "b"
  )
#create plot
temp %>% ggplot(
  aes(x=date, y = val, color = type, linetype = as.factor(emph), 
      alpha = as.factor(emph),size = as.factor(emph)), guide='none') +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  geom_line()+
  xlim(as.Date('2006-01-01'), as.Date('2009-06-01')) + 
  geom_vline(xintercept=as.Date('2007-12-31'), size=.5, color="black") + 
  geom_dl(aes(label = type), method = list('last.bumpup', cex = 1.2)) +   
  scale_colour_manual(values = c(teal, cardinal, black, green), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "Home Value Index (Dec 2007=100)",
       size = 7
  )+    geom_text(label="Dec 2007", x=as.Date('2007-10-01'), y=95, size = 6, color = 'black',
  ) +
  theme_minimal()+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)
  )
#save plot
ggsave('./figures-tables/appendix_a4b.png', plot = last_plot(), width = 10, height = 8)


###########################################
## Appendix A5: Cumulative flows
###########################################
#read in zip code characteristics
chars <- read_csv('./data/zip_all_chars_cbd.csv', 
                  col_types = cols('zip' = col_integer())) %>% dplyr::select(!estab_count)
#read in business establishment counts
bus_chars <- read_csv('./data/zbp_wfh.csv',
                      col_types = cols('zip' = col_integer()))
chars <- chars %>% inner_join(bus_chars, by = 'zip')

#read in USPS zip code month level flow data
usps <- read_csv('./data/USPS_zips.csv')

## A. population flows
#create dataset
temp <- chars %>% filter(MetroShort %in% cities, !is.na(`2019 Population`)) %>%
  mutate(quantile_rank = 'cbd') %>%
  group_by(MsaName) %>%
  mutate(quantile_rank = replace(quantile_rank, dist_to_cbd >= 2000, ntile(density2019[dist_to_cbd>=2000], 10)),
         category = ifelse(quantile_rank == 'cbd', 'city center',
                           case_when(quantile_rank == 10 ~ 'high density',
                                     quantile_rank %in% c(6:9) ~ 'suburb',
                                     TRUE ~ 'exurb'))) %>%
  inner_join(usps, by = 'zip') %>% 
  mutate(date = as.Date(date)) %>%
  group_by(category, date) %>%
  summarise(net_pop = sum(net_pop, na.rm = TRUE)/sum(`2019 Population`)*100) %>% 
  filter(date >= as.Date('2018-01-01')) %>%
  group_by(category) %>%
  mutate(net_pop_avg = zoo::rollmean(net_pop, 12, align='right', na.pad=TRUE),
         net_pop = cumsum(net_pop - net_pop_avg[date == as.Date('2020-02-15')]),
         net_pop = net_pop - net_pop[date==as.Date('2020-02-15')]) %>%
  mutate(emph = "b", val=net_pop, name=category)

#create plot
temp %>% ggplot(
  aes(x=date, y = val, color = name, linetype = as.factor(emph), 
      alpha = as.factor(emph),size = as.factor(emph)), guide='none') +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  geom_line()+
  xlim(as.Date('2018-01-01'), as.Date(end_date_long)) + 
  ylim(-8, 1) + #(-2, .3)
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  geom_dl(aes(label = name), method = list(cex = 1.2, 'last.bumpup')) +   
  scale_colour_manual(values = c(teal, cardinal, black, green), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "Cumulative devs from pre-pandemic flow (% points)",
       size = 7
  )+
  geom_text(label="Feb 2020", x=as.Date('2019-11-01'), y=-.5, size = 6, color = 'black') +
  theme_minimal()+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)
  )

#save plot
ggsave('./figures-tables/appendix_a5a.png', plot = last_plot(), width = 10, height = 8)

## B. business flows
#create dataset
temp <- chars %>% filter(MetroShort %in% cities, !is.na(`2019 Population`)) %>%
  mutate(quantile_rank = 'cbd') %>%
  group_by(MsaName) %>%
  mutate(quantile_rank = replace(quantile_rank, dist_to_cbd >= 2000, ntile(density2019[dist_to_cbd>=2000], 10)),
         category = ifelse(quantile_rank == 'cbd', 'city center',
                           case_when(quantile_rank == 10 ~ 'high density',
                                     quantile_rank %in% c(6:9) ~ 'suburb',
                                     TRUE ~ 'exurb'))) %>%
  inner_join(usps, by = 'zip') %>% 
  mutate(date = as.Date(date)) %>%
  group_by(category, date) %>%
  summarise(net_bus = sum(net_bus, na.rm = TRUE)/sum(`2019 Population`)*100) %>% 
  filter(date >= as.Date('2018-01-01')) %>%
  group_by(category) %>%
  mutate(net_bus_avg = zoo::rollmean(net_bus, 12, align='right', na.pad=TRUE),
         net_bus = cumsum(net_bus - net_bus_avg[date == as.Date('2020-02-15')]),
         net_bus = net_bus - net_bus[date==as.Date('2020-02-15')]) %>%
  mutate(emph = "b", val=net_bus, name=category)

#create plot
temp %>% ggplot(
  aes(x=date, y = val, color = name, linetype = as.factor(emph), 
      alpha = as.factor(emph),size = as.factor(emph)), guide='none') +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  geom_line()+
  xlim(as.Date('2018-01-01'), as.Date(end_date_long)) + 
  ylim(-4.5, .5) + #(-2, .3)
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  geom_dl(aes(label = name), method = list(cex = 1.2, 'last.bumpup')) +   
  scale_colour_manual(values = c(teal, cardinal, black, green), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "Cumulative devs from pre-pandemic flow (% points)",
       size = 7
  )+
  geom_text(label="Feb 2020", x=as.Date('2019-11-01'), y=-.5, size = 6, color = 'black') +
  theme_minimal()+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)
  )
#save plot
ggsave('./figures-tables/appendix_a5b.png', plot = last_plot(), width = 10, height = 8)


###########################################
## Footnote: cumulative flows alternate calculation
## We calculate cumulative population outflows as a percent of stock
## and cumulative establishment outflows as a percent of stock
## from Feb 2019-20 and 2020-21 (unadjusted for prior flow)
###########################################
#read in zip code characteristics
chars <- read_csv('./data/zip_all_chars_cbd.csv', 
                  col_types = cols('zip' = col_integer())) %>% dplyr::select(!estab_count)
#read in business establishment counts
bus_chars <- read_csv('./data/zbp_wfh.csv',
                      col_types = cols('zip' = col_integer()))
chars <- chars %>% inner_join(bus_chars, by = 'zip')

#read in zip code month level USPS flow data
usps <- read_csv('./data/USPS_zips.csv')

#Calulate outflows from CBDs for post-Covid period (Feb 2020 - Dec 2021)
inflow_post = chars %>% filter(MetroShort %in% cities, dist_to_cbd < 2000) %>%
  inner_join(usps, by = 'zip') %>% filter(date >= as.Date('2020-03-01'), date < as.Date(end_period)) %>% 
  summarise(net_inflow = sum(net_pop, na.rm = TRUE))
#Calulate outflows from CBDs for pre-Covid period (Apr 2018 - Feb 2020)
inflow_pre = chars %>% filter(MetroShort %in% cities, dist_to_cbd < 2000) %>%
  inner_join(usps, by = 'zip') %>% filter(date >= as.Date(start_period), date < as.Date('2020-03-01')) %>% 
  summarise(net_inflow = sum(net_pop, na.rm = TRUE))
pop = chars %>% filter(MetroShort %in% cities, dist_to_cbd < 2000) %>% summarise(pop = sum(`2019 Population`, na.rm = TRUE))
print(c(inflow_pre, pop, inflow_pre/pop))
print(c(inflow_post, pop, inflow_post/pop))

#Calulate outflows from low density zips for post-Covid period (Feb 2020 - Dec 2021)
inflow_post = chars %>% filter(MetroShort %in% cities, ) %>%
  inner_join(usps, by = 'zip') %>% filter(date >= as.Date('2020-03-01'), date < as.Date(end_period)) %>% 
  summarise(net_inflow = sum(net_pop, na.rm = TRUE))
#Calulate outflows from low density for pre-Covid period (Apr 2018 - Feb 2020)
inflow_pre = chars %>% filter(MetroShort %in% cities, dist_to_cbd < 2000) %>%
  inner_join(usps, by = 'zip') %>% filter(date >= as.Date(start_period), date < as.Date('2020-03-01')) %>% 
  summarise(net_inflow = sum(net_pop, na.rm = TRUE))
pop = chars %>% filter(MetroShort %in% cities, dist_to_cbd < 2000) %>% summarise(pop = sum(`2019 Population`, na.rm = TRUE))
print(c(inflow_pre, pop, inflow_pre/pop))
print(c(inflow_post, pop, inflow_post/pop))