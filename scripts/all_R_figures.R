###########################################
# R_figures.R
# This script reads in the cleaned USPS and Zillow 
# files, and creates figures
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
black <- "#2E2D29"; cardinal <- "#B1040E"; teal <- "#66b2b2"; green <- "#228B22"
options(repr.plot.width=10, repr.plot.height=8)

#define current directory
dir <- '~/Documents/zillow/thesis/donut-effect/'

###########################################
## Figure 1(a) donut effect in rental market
###########################################
df <- read_csv('https://files.zillowstatic.com/research/public_v2/zori/Zip_ZORI_AllHomesPlusMultifamily_Smoothed.csv', 
               col_types = cols(RegionName = col_double())) %>% rename(zip = 'RegionName') %>%
  select(!c(RegionID, SizeRank, MsaName)) %>% 
  pivot_longer(!zip, names_to = 'date', values_to = 'zhvi') %>%
  mutate(date = as.Date(as.yearmon(date)) + 14)
chars <- read_csv(dir + 'data/zip_all_chars_cbd.csv')

chars %>% filter(MetroShort %in% cities, !is.na(`2019 Population`), !is.na(dist_to_cbd)) %>%
  mutate(quantile_rank = ntile(density2019, 10),
         category = ifelse(dist_to_cbd < 2000, 'cbd', 
                           case_when(quantile_rank == 10 ~ 'high',
                                     quantile_rank %in% c(6:9) ~ 'mid',
                                     TRUE ~ 'low'))) %>%
  inner_join(df, by = 'zip') %>% 
  mutate(date = as.Date(date),
         zhvi_pop = zhvi*`2019 Population`) %>%
  filter(date >= as.Date('2018-01-01')) %>%
  group_by(zip) %>% mutate(zhvi_pop = na.approx(zhvi_pop, na.rm=FALSE)) %>% 
  group_by(category, date) %>%
  summarise(zhvi_pop = sum(zhvi_pop, na.rm = TRUE),
            population = sum(`2019 Population`, na.rm = TRUE)) %>% 
  mutate(zhvi = zhvi_pop/population) %>%
  group_by(category) %>% mutate(zhvi = zhvi/zhvi[date == as.Date('2020-02-15')]*100) %>%
  mutate(
    val = zhvi, name = category,
    type = case_when(str_detect(name, 'high') ~ 'high density',
                     str_detect(name, 'mid') ~ 'mid density',
                     str_detect(name, 'cbd') ~ 'CBD',
                     TRUE ~ 'low density'), emph = "b"
  ) %>% ggplot(
    aes(x=date, y = val, color = type, linetype = as.factor(emph), 
        alpha = as.factor(emph),size = as.factor(emph)), guide='none') +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  geom_line()+
  xlim(as.Date('2018-01-01'), as.Date('2021-04-15')) + 
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  geom_dl(aes(label = type), method = list(dl.trans(x=x+1.5, y = y-.7),'last.bumpup', cex = 1.5, hjust=1)) +   
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
#legend.position=c(.85, .5)
ggsave(dir +'figures/fig1a.png', plot = last_plot(), width = 10, height = 8)


###########################################
## Figure 1(b) donut effect in purchase market
###########################################
df2 <- read_csv('http://files.zillowstatic.com/research/public_v2/zhvi/Zip_zhvi_uc_sfr_tier_0.33_0.67_sm_sa_mon.csv', 
                col_types = cols(RegionName = col_double())) %>% 
  rename(zip = 'RegionName', MsaShort = 'Metro') %>%
  mutate(MetroShort = sub("-.*", "", MsaShort),
         MetroShort = paste(MetroShort, State, sep = ', ')) %>%
  select(!c(RegionID, SizeRank, RegionType, StateName, State, City, CountyName, MsaShort)) %>%
  pivot_longer(!c(zip, MetroShort), names_to = 'date', values_to = 'zhvi')
chars <- read_csv(dir + 'data/zip_all_chars_cbd.csv')

chars %>% filter(MetroShort %in% cities, !is.na(`2019 Population`), !is.na(dist_to_cbd)) %>%
  mutate(quantile_rank = ntile(density2019, 10),
         category = ifelse(dist_to_cbd < 2000, 'cbd', 
                           case_when(quantile_rank == 10 ~ 'high',
                                     quantile_rank %in% c(6:9) ~ 'mid',
                                     TRUE ~ 'low'))) %>%
  inner_join(df2, by = 'zip') %>% 
  mutate(date = as.Date(date),
         zhvi_pop = zhvi*`2019 Population`) %>%
  filter(date >= as.Date('2018-01-01')) %>%
  group_by(zip) %>% mutate(zhvi_pop = na.approx(zhvi_pop, na.rm=FALSE)) %>% 
  group_by(category, date) %>%
  summarise(zhvi_pop = sum(zhvi_pop, na.rm = TRUE),
            population = sum(`2019 Population`, na.rm = TRUE)) %>% 
  mutate(zhvi = zhvi_pop/population) %>%
  group_by(category) %>% mutate(zhvi = zhvi/zhvi[date == as.Date('2020-02-29')]*100) %>%
  mutate(
    val = zhvi, name = category,
    type = case_when(str_detect(name, 'high') ~ 'high density',
                     str_detect(name, 'mid') ~ 'mid density',
                     str_detect(name, 'cbd') ~ 'CBD',
                     TRUE ~ 'low density'), emph = "b"
  ) %>% ggplot(
    aes(x=date, y = val, color = type, linetype = as.factor(emph), 
        alpha = as.factor(emph),size = as.factor(emph)), guide='none') +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  geom_line()+
  xlim(as.Date('2018-01-01'), as.Date('2021-04-15')) + 
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  scale_colour_manual(values = c(teal, cardinal, black, green), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "Home Value Index (Feb 2020=100)",
       size = 7
  )+    geom_text(label="Feb 2020", x=as.Date('2019-12-01'), y=104, size = 6, color = 'black',
  ) +
  geom_text(label="low density", x=as.Date('2020-11-01'), y=109, size = 6, color = black,
  ) +
  geom_text(label="mid density", x=as.Date('2021-02-15'), y=105.5, size = 6, color = green,
  ) +
  geom_text(label="high density", x=as.Date('2021-02-15'), y=102, size = 6, color = cardinal,
  ) +
  geom_text(label="CBD", x=as.Date('2020-12-01'), y=98, size = 6, color = teal,
  ) +
  
  theme_minimal()+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)
  )
#legend.position=c(.85, .5)
ggsave(dir + 'figures/fig1b.png', plot = last_plot(), width = 10, height = 8)

###########################################
## Fig 2 Donut effect for USPS population and business flows
###########################################

chars <- read_csv(dir + 'data/zip_all_chars_cbd.csv', 
                  col_types = cols('zip' = col_integer())) %>% select(!estab_count)
bus_chars <- read_csv(dir + 'zbp_wfh.csv',
                      col_types = cols('zip' = col_integer()))
chars <- chars %>% inner_join(bus_chars, by = 'zip')

usps <- read_csv(dir + 'data/USPS_zips.csv')

## A. population flows

chars %>% filter(MetroShort %in% cities, !is.na(`2019 Population`)) %>%
  mutate(quantile_rank = ntile(density2019, 10),
         category = if_else(dist_to_cbd <2000, 'cbd', 
                            case_when(quantile_rank == 10 ~ 'high',
                                      quantile_rank %in% c(6:9) ~ 'mid',
                                      TRUE ~ 'low'))) %>%
  inner_join(usps, by = 'zip') %>% 
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date('2018-01-01')) %>%
  group_by(zip) %>% mutate(net_pop = na.approx(net_pop, na.rm=FALSE)) %>%
  group_by(category, date) %>%
  summarise(net_pop = sum(net_pop, na.rm = TRUE)/sum(`2019 Population`)*100) %>% 
  group_by(category) %>% mutate(net_pop = net_pop - net_pop[date == as.Date('2020-02-15')]) %>%
  mutate(emph="b", val = net_pop, name = category) %>%
  ggplot(
    aes(x=date, y = val, color = name, linetype = as.factor(emph), 
        alpha = as.factor(emph),size = as.factor(emph)), guide='none') +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  geom_line()+
  xlim(as.Date('2018-01-01'), as.Date('2021-04-01')) + 
  ylim(-2, .25) +
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  scale_colour_manual(values = c(teal, cardinal, black, green), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "Deviations from Feb 2020 (% points)",
       size = 7
  )+
  geom_text(label="Feb 2020", x=as.Date('2019-12-01'), y=-1, size = 6, color = 'black',
  ) +
  geom_text(label="low density", x=as.Date('2020-10-15'), y=.05, size = 6, color = black,
  ) +
  geom_text(label="mid density", x=as.Date('2020-10-15'), y=-.15, size = 6, color = green,
  ) +
  geom_text(label="high density", x=as.Date('2020-10-15'), y=-.3, size = 6, color = cardinal,
  ) +
  geom_text(label="CBD", x=as.Date('2020-11-15'), y=-.6, size = 6, color = teal,
  ) +
  theme_minimal()+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)
  )
#legend.position=c(.85, .5)
ggsave(dir + 'figures/fig2a.png', plot = last_plot(), width = 10, height = 8)

## B. Business flows

chars %>% filter(MetroShort %in% cities) %>%
  mutate(quantile_rank = ntile(density2019, 10),
         category = if_else(dist_to_cbd <2000, 'cbd', 
                            case_when(quantile_rank == 10 ~ 'high',
                                      quantile_rank %in% c(6:9) ~ 'mid',
                                      TRUE ~ 'low'))) %>%
  inner_join(usps, by = 'zip') %>% 
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date('2018-01-01')) %>%
  group_by(zip) %>% mutate(net_bus = na.approx(net_bus, na.rm=FALSE)) %>%
  group_by(category, date) %>%
  summarise(net_bus = sum(net_bus, na.rm = TRUE)/sum(estab_count)*100) %>% 
  group_by(category) %>% mutate(net_bus = net_bus - net_bus[date == as.Date('2020-02-15')]) %>%
  mutate(emph="b", val = net_bus, name = category) %>%
  ggplot(
    aes(x=date, y = val, color = name, linetype = as.factor(emph), 
        alpha = as.factor(emph),size = as.factor(emph)), guide='none') +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  geom_line()+
  xlim(as.Date('2018-01-01'), as.Date('2021-04-01')) + 
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  scale_colour_manual(values = c(teal, cardinal, black, green), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "Deviations from Feb 2020 (% points)",
       size = 7
  )+
  geom_text(label="Feb 2020", x=as.Date('2019-12-01'), y=-1, size = 6, color = 'black',
  ) +
  geom_text(label="low density", x=as.Date('2020-11-15'), y=.2, size = 6, color = black,
  ) +
  geom_text(label="mid density", x=as.Date('2020-11-15'), y=-.1, size = 6, color = green,
  ) +
  geom_text(label="high density", x=as.Date('2020-08-15'), y=-.47, size = 6, color = cardinal,
  ) +
  geom_text(label="CBD", x=as.Date('2021-01-15'), y=-.6, size = 6, color = teal,
  ) +
  theme_minimal()+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)
  )
#legend.position=c(.85, .5)
ggsave(dir + 'figures/fig2b.png', plot = last_plot(), width = 10, height = 8)

###########################################
## Fig 3 and Appendix A7: USPS heat maps
###########################################

#https://arilamstein.com/documentation/choroplethrZip/reference/zip_choropleth.html
#install_github('arilamstein/choroplethrZip@v1.5.0')
library(choroplethrZip)

df5 <- read_csv('~/Documents/zillow/thesis/data/usps_panel_zips.csv', col_types = cols(zip = col_character())) %>%
  mutate(value = round(post_pop, digits = 3),
         zip = ifelse(nchar(zip) ==4, paste('0', zip, sep=''), zip)) %>%
  select(zip, value) %>%
  rename(region = zip) %>%
  filter(!is.na(zip), !is.na(value))

## New York
df_choro <- df5
df_choro$value <- cut(df5$value, breaks = c(-45, -5, -2, -.5, .05, 2, 4, 100))
choro = ZipChoropleth$new(df_choro)
choro$ggplot_scale = scale_fill_brewer(name="Net inflows/pop (%)", palette = 'RdYlGn')
choro$set_zoom_zip(state_zoom=NULL, county_zoom=NULL, msa_zoom='New York-Newark-Jersey City, NY-NJ-PA', zip_zoom=NULL)
choro$render()
ggsave(dir + 'figures/fig3a.png', plot = last_plot(), width = 10, height = 8)

## San Frnacisco
df_choro <- df5
df_choro$value <- cut(df5$value, breaks = c(-35, -10, -5, -2, 0, .5, 2, 10))
choro = ZipChoropleth$new(df_choro)
choro$ggplot_scale = scale_fill_brewer(name="Net inflows/pop (%)", palette = 'RdYlGn')
choro$set_zoom_zip(state_zoom=NULL, county_zoom=NULL, msa_zoom='San Francisco-Oakland-Hayward, CA', zip_zoom=NULL)
choro$render()
ggsave(dir + 'figures/fig3b.png', plot = last_plot(), width = 10, height = 8)

## Boston
df_choro <- df5
df_choro$value <- cut(df5$value, breaks = c(-45, -5, -1.5, 0, 1, 2, 4, 10))
choro = ZipChoropleth$new(df_choro)
choro$ggplot_scale = scale_fill_brewer(name="Net inflows/pop (%)", palette = 'RdYlGn')
choro$set_zoom_zip(state_zoom=NULL, county_zoom=NULL, msa_zoom='Boston-Cambridge-Newton, MA-NH', zip_zoom=NULL)
choro$render()
ggsave(dir + 'figures/appendixa7_a.png', plot = last_plot(), width = 10, height = 8)

## Los Angeles
df_choro <- df5
df_choro$value <- cut(df5$value, breaks = c(-25, -5, -2, -1, 0, 1, 2, 10))
choro = ZipChoropleth$new(df_choro)
choro$ggplot_scale = scale_fill_brewer(name="Net inflows/pop (%)", palette = 'RdYlGn')
choro$set_zoom_zip(state_zoom=NULL, county_zoom=c('6037', '6059'), msa_zoom=NULL, zip_zoom=NULL)
choro$render()
ggsave(dir + 'figures/appendixa7_b.png', plot = last_plot(), width = 10, height = 8)


###########################################
## Figure 4: heterogeneity in home price index across metros
###########################################

df3 <- read_csv('http://files.zillowstatic.com/research/public_v2/zhvi/Zip_zhvi_uc_sfr_tier_0.33_0.67_sm_sa_mon.csv', 
                col_types = cols(RegionName = col_double())) %>% 
  rename(zip = 'RegionName', MsaShort = 'Metro') %>%
  mutate(MetroShort = sub("-.*", "", MsaShort),
         MetroShort = paste(MetroShort, State, sep = ', ')) %>%
  select(!c(RegionID, SizeRank, RegionType, StateName, State, City, CountyName, MsaShort)) %>%
  pivot_longer(!c(zip, MetroShort), names_to = 'date', values_to = 'zhvi')

chars <- read_csv(dir + 'data/zip_all_chars_cbd.csv')

msa_chars <- msa_chars <- read_csv(dir + 'data/msa_all_chars.csv') %>% filter(!is.na(MsaName)) %>%
  mutate(msa_pop_rank = dense_rank(desc(`2019 Population`)),
         msa_pop_group = case_when(msa_pop_rank %in% c(1:12) ~ 'top12',
                                   msa_pop_rank %in% c(13:50) ~ 'mid',
                                   TRUE ~ 'low'))

#@param: msa_group = which msa-level variable we want to break plot down by
#@param: msa_level = which level of the msa_group variable we want to create the plot for
#@param: zip_group = the zip code level grouping variable to break down time series by

grid_plot <- function (msa_group, msa_level, zip_group) {
  options(repr.plot.width=12, repr.plot.height=5)
  return (msa_chars %>% filter(get(msa_group) == msa_level) %>%
            select(MetroShort, msa_pop_rank) %>%
            inner_join(chars, by = 'MetroShort') %>% 
            filter(!is.na(`2019 Population`), !is.na(dist_to_cbd)) %>%
            mutate(quantile_rank = ntile(get(zip_group), 10),
                   category = ifelse(dist_to_cbd < 2000, 'cbd', 
                                     case_when(quantile_rank == 10 ~ 'high',
                                               quantile_rank %in% c(6:9) ~ 'mid',
                                               TRUE ~ 'low'))) %>%
            inner_join(df3, by = 'zip') %>% 
            mutate(date = as.Date(date),
                   zhvi_pop = zhvi*`2019 Population`) %>%
            filter(date >= as.Date('2018-01-01')) %>%
            group_by(zip) %>% mutate(zhvi_pop = na.approx(zhvi_pop, na.rm=FALSE)) %>% 
            group_by(category, date) %>%
            summarise(zhvi_pop = sum(zhvi_pop, na.rm = TRUE),
                      population = sum(`2019 Population`, na.rm = TRUE)) %>% 
            mutate(zhvi = zhvi_pop/population) %>%
            group_by(category) %>% mutate(zhvi = zhvi/zhvi[date == as.Date('2020-02-29')]*100) %>%
            mutate(
              val = zhvi, name = category,
              type = case_when(str_detect(name, 'high') ~ 'high dens',
                               str_detect(name, 'mid') ~ 'mid dens',
                               str_detect(name, 'cbd') ~ 'CBD',
                               TRUE ~ 'low dens'), emph = "b"
            ) %>% ggplot(
              aes(x=date, y = val, color = type, linetype = as.factor(emph), 
                  alpha = as.factor(emph),size = as.factor(emph)), guide='none') +
            scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
            geom_line()+
            xlim(as.Date('2018-01-01'), as.Date('2021-04-15')) + 
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

a <- grid_plot('msa_pop_group', 'top12', 'density2019')
b <- grid_plot('msa_pop_group', 'mid', 'density2019')
c <- grid_plot('msa_pop_group', 'low', 'density2019')

plot_grid(a, NULL, b, NULL, c, NULL, rel_widths = c(1, .1, 1, .1, 1, .13), 
          rel_heights = c(1, .1, 1, .1, 1, .1), nrow = 1, ncol = 6)
ggsave(dir + 'figures/fig4.png', plot = last_plot(), width = 15, height = 6)

###########################################
## Appendix A1: Heterogeneity in rents
###########################################

df <- read_csv('https://files.zillowstatic.com/research/public_v2/zori/Zip_ZORI_AllHomesPlusMultifamily_Smoothed.csv', 
               col_types = cols(RegionName = col_double())) %>% rename(zip = 'RegionName') %>%
  select(!c(RegionID, SizeRank, MsaName)) %>% 
  pivot_longer(!zip, names_to = 'date', values_to = 'zhvi') %>%
  mutate(date = as.Date(as.yearmon(date)) + 14)

chars <- read_csv(dir + 'data/zip_all_chars_cbd.csv')

msa_chars <- msa_chars <- read_csv(dir + 'data/msa_all_chars.csv') %>% filter(!is.na(MsaName)) %>%
  mutate(msa_pop_rank = dense_rank(desc(`2019 Population`)),
         msa_pop_group = case_when(msa_pop_rank %in% c(1:12) ~ 'top12',
                                   msa_pop_rank %in% c(13:50) ~ 'mid',
                                   TRUE ~ 'low'))

#@param: msa_group = which msa-level variable we want to break plot down by
#@param: msa_level = which level of the msa_group variable we want to create the plot for
#@param: zip_group = the zip code level grouping variable to break down time series by

grid_plot <- function (msa_group, msa_level, zip_group) {
  options(repr.plot.width=12, repr.plot.height=5)
  return (msa_chars %>% filter(get(msa_group) == msa_level) %>%
            select(MetroShort, msa_pop_rank) %>%
            inner_join(chars, by = 'MetroShort') %>% 
            filter(!is.na(`2019 Population`), !is.na(dist_to_cbd)) %>%
            mutate(quantile_rank = ntile(get(zip_group), 10),
                   category = ifelse(dist_to_cbd < 2000, 'cbd', 
                                     case_when(quantile_rank == 10 ~ 'high',
                                               quantile_rank %in% c(6:9) ~ 'mid',
                                               TRUE ~ 'low'))) %>%
            inner_join(df, by = 'zip') %>% 
            mutate(date = as.Date(date),
                   zhvi_pop = zhvi*`2019 Population`) %>%
            filter(date >= as.Date('2018-01-01')) %>%
            group_by(zip) %>% mutate(zhvi_pop = na.approx(zhvi_pop, na.rm=FALSE)) %>% 
            group_by(category, date) %>%
            summarise(zhvi_pop = sum(zhvi_pop, na.rm = TRUE),
                      population = sum(`2019 Population`, na.rm = TRUE)) %>% 
            mutate(zhvi = zhvi_pop/population) %>%
            group_by(category) %>% mutate(zhvi = zhvi/zhvi[date == as.Date('2020-02-15')]*100) %>%
            mutate(
              val = zhvi, name = category,
              type = case_when(str_detect(name, 'high') ~ 'high dens',
                               str_detect(name, 'mid') ~ 'mid dens',
                               str_detect(name, 'cbd') ~ 'CBD',
                               TRUE ~ 'low dens'), emph = "b"
            ) %>% ggplot(
              aes(x=date, y = val, color = type, linetype = as.factor(emph), 
                  alpha = as.factor(emph),size = as.factor(emph)), guide='none') +
            scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
            geom_line()+
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

a <- grid_plot('msa_pop_group', 'top12', 'density2019')
b <- grid_plot('msa_pop_group', 'mid', 'density2019')
c <- grid_plot('msa_pop_group', 'low', 'density2019')

plot_grid(a, NULL, b, NULL, c, NULL, rel_widths = c(1, .1, 1, .1, 1, .13), 
          rel_heights = c(1, .1, 1, .1, 1, .1), nrow = 1, ncol = 6)
ggsave(dir + 'figures/appendix_a1.png', plot = last_plot(), width = 15, height = 6)


###########################################
## Appendix A2: heterogeneity in population flows
###########################################
chars <- read_csv(dir + 'data/zip_all_chars_cbd.csv', 
                  col_types = cols('zip' = col_integer())) %>% select(!estab_count)
bus_chars <- read_csv(dir + 'data/zbp_wfh.csv',
                      col_types = cols('zip' = col_integer()))
chars <- chars %>% inner_join(bus_chars, by = 'zip')

usps <- read_csv(dir + 'data/USPS_zips.csv')

msa_chars <- msa_chars <- read_csv(dir + 'data/msa_all_chars.csv') %>% filter(!is.na(MsaName)) %>%
  mutate(msa_pop_rank = dense_rank(desc(`2019 Population`)),
         msa_pop_group = case_when(msa_pop_rank %in% c(1:12) ~ 'top12',
                                   msa_pop_rank %in% c(13:50) ~ 'mid',
                                   TRUE ~ 'low'))
#@param: msa_group = which msa-level variable we want to break plot down by
#@param: msa_level = which level of the msa_group variable we want to create the plot for
#@param: zip_group = the zip code level grouping variable to break down time series by

grid_plot <- function (msa_group, msa_level, zip_group) {
  options(repr.plot.width=12, repr.plot.height=5)
  return (msa_chars %>% filter(get(msa_group) == msa_level) %>%
            select(MetroShort, msa_pop_rank) %>%
            inner_join(chars, by = 'MetroShort') %>% 
            filter(!is.na(`2019 Population`), !is.na(dist_to_cbd)) %>%
            mutate(quantile_rank = ntile(get(zip_group), 10),
                   category = if_else(dist_to_cbd <2000, 'cbd', 
                                      case_when(quantile_rank == 10 ~ 'high',
                                                quantile_rank %in% c(6:9) ~ 'mid',
                                                TRUE ~ 'low'))) %>%
            inner_join(usps, by = 'zip') %>% 
            mutate(date = as.Date(date)) %>%
            filter(date >= as.Date('2018-01-01')) %>%
            group_by(zip) %>% mutate(net_pop = na.approx(net_pop, na.rm=FALSE)) %>%
            group_by(category, date) %>%
            summarise(net_pop = sum(net_pop, na.rm = TRUE)/sum(`2019 Population`)*100) %>% 
            group_by(category) %>% mutate(net_pop = net_pop - net_pop[date == as.Date('2020-02-15')]) %>%
            mutate(emph="b", val = net_pop, name = category) %>%
            ggplot(
              aes(x=date, y = val, color = name, linetype = as.factor(emph), 
                  alpha = as.factor(emph),size = as.factor(emph)), guide='none') +
            scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
            geom_line()+
            xlim(as.Date('2018-01-01'), as.Date('2021-04-15')) + 
            ylim(-1.75, .25) + 
            geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
            geom_dl(aes(label = name), method = list(cex = 1,'last.bumpup')) +   
            scale_colour_manual(values = c(teal, cardinal, black, green), guide='none')+
            scale_size_manual(values = c(1.5, 0.8), guide="none")+
            scale_alpha_manual(values = c(1, 0.8), guide="none")+
            scale_linetype(guide='none') +
            labs(title = ifelse(msa_level == 'top12', 'Top 12 Metros', ifelse(msa_level=='mid', 'Metros 13-50', 'Metros 51-365')), 
                 x  = "", 
                 y = ifelse(msa_level=='top12', 'Deviations from Feb 2020 (% points)', '')
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

a <- grid_plot('msa_pop_group', 'top12', 'density2019')
b <- grid_plot('msa_pop_group', 'mid', 'density2019')
c <- grid_plot('msa_pop_group', 'low', 'density2019')

plot_grid(a, NULL, b, NULL, c, NULL, rel_widths = c(1, .1, 1, .1, 1, .13), 
          rel_heights = c(1, .1, 1, .1, 1, .1), nrow = 1, ncol = 6)
ggsave(dir + 'figures/appendix_a2.png', plot = last_plot(), width = 15, height = 6)


###########################################
## Appendix A4: equal weighting
###########################################
## Part A. rental index
df <- read_csv('https://files.zillowstatic.com/research/public_v2/zori/Zip_ZORI_AllHomesPlusMultifamily_Smoothed.csv', 
               col_types = cols(RegionName = col_double())) %>% rename(zip = 'RegionName') %>%
  select(!c(RegionID, SizeRank, MsaName)) %>% 
  pivot_longer(!zip, names_to = 'date', values_to = 'zhvi') %>%
  mutate(date = as.Date(as.yearmon(date)) + 14)
chars <- read_csv(dir + 'data/zip_all_chars_cbd.csv')

chars %>% filter(MetroShort %in% cities, !is.na(dist_to_cbd)) %>%
  mutate(quantile_rank = ntile(density2019, 10),
         category = ifelse(dist_to_cbd < 2000, 'cbd', 
                           case_when(quantile_rank == 10 ~ 'high',
                                     quantile_rank %in% c(6:9) ~ 'mid',
                                     TRUE ~ 'low'))) %>%
  inner_join(df, by = 'zip') %>% 
  mutate(date = as.Date(date)) %>%
  group_by(zip) %>% mutate(zhvi = na.approx(zhvi, na.rm=FALSE)) %>%
  group_by(zip) %>% mutate(zhvi = zhvi/zhvi[date == as.Date('2020-02-15')]*100) %>%
  mutate(zhvi_pop = zhvi*`2019 Population`) %>%
  group_by(category, date) %>%
  summarise(zhvi_pop = sum(zhvi_pop, na.rm = TRUE),
            population = sum(`2019 Population`, na.rm = TRUE)) %>% 
  mutate(val = zhvi_pop/population,
         name = category) %>%
  filter(date > as.Date('2018-01-01')) %>% 
  mutate(
    type = case_when(str_detect(name, 'high') ~ 'high density',
                     str_detect(name, 'mid') ~ 'mid density',
                     str_detect(name, 'cbd') ~ 'CBD',
                     TRUE ~ 'low density'), emph = "b"
  )%>% ggplot(
    aes(x=date, y = val, color = type, linetype = as.factor(emph), 
        alpha = as.factor(emph),size = as.factor(emph)), guide='none') +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  geom_line()+
  xlim(as.Date('2018-01-01'), as.Date('2021-04-01')) + 
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  geom_dl(aes(label = type), method = list(dl.trans(x=x+1.3, y = y-.8),'last.bumpup', cex = 1.5, hjust=1)) +   
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
#legend.position=c(.85, .5)
ggsave(dir + 'figures/appendix_a4a.png', plot = last_plot(), width = 10, height = 8)


## Part B. home value index
df2 <- read_csv('http://files.zillowstatic.com/research/public_v2/zhvi/Zip_zhvi_uc_sfr_tier_0.33_0.67_sm_sa_mon.csv', 
                col_types = cols(RegionName = col_double())) %>% 
  rename(zip = 'RegionName', MsaShort = 'Metro') %>%
  mutate(MetroShort = sub("-.*", "", MsaShort),
         MetroShort = paste(MetroShort, State, sep = ', ')) %>%
  select(!c(RegionID, SizeRank, RegionType, StateName, State, City, CountyName, MsaShort)) %>%
  pivot_longer(!c(zip, MetroShort), names_to = 'date', values_to = 'zhvi')
chars <- read_csv(dir + 'data/zip_all_chars_cbd.csv')

chars %>% filter(MetroShort %in% cities, !is.na(density2019), !is.na(dist_to_cbd), !is.na(`2019 Population`)) %>%
  mutate(quantile_rank = ntile(density2019, 10),
         category = ifelse(dist_to_cbd < 2000, 'cbd', 
                           case_when(quantile_rank == 10 ~ 'high',
                                     quantile_rank %in% c(6:9) ~ 'mid',
                                     TRUE ~ 'low'))) %>%
  inner_join(df2, by = 'zip') %>% 
  mutate(date = as.Date(date)) %>%
  group_by(zip) %>% mutate(zhvi = na.approx(zhvi, na.rm=FALSE)) %>%
  group_by(zip) %>% mutate(zhvi = zhvi/zhvi[date == as.Date('2020-02-29')]*100) %>%
  mutate(zhvi_pop = zhvi*`2019 Population`) %>%
  group_by(category, date) %>%
  summarise(zhvi_pop = sum(zhvi_pop, na.rm = TRUE),
            population = sum(`2019 Population`, na.rm = TRUE)) %>% 
  mutate(val = zhvi_pop/population,
         name = category) %>%
  filter(date > as.Date('2018-01-01')) %>% 
  mutate(
    type = case_when(str_detect(name, 'high') ~ 'high density',
                     str_detect(name, 'mid') ~ 'mid density',
                     str_detect(name, 'cbd') ~ 'city center',
                     TRUE ~ 'low density'), emph = "b"
  )%>% ggplot(
    aes(x=date, y = val, color = type, linetype = as.factor(emph), 
        alpha = as.factor(emph),size = as.factor(emph)), guide='none') +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  geom_line()+
  xlim(as.Date('2018-01-01'), as.Date('2021-04-01')) + 
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  scale_colour_manual(values = c(teal, cardinal, black, green), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "Home Value Index (Feb 2020=100)",
       size = 7
  )+
  geom_text(label="Feb 2020", x=as.Date('2019-12-01'), y=104, size = 6, color = 'black',
  ) +
  geom_text(label="low density", x=as.Date('2021-04-01'), y=107, size = 6, color = black,
  ) +
  geom_text(label="mid density", x=as.Date('2020-10-01'), y=108, size = 6, color = green,
  ) +
  geom_text(label="high density", x=as.Date('2020-12-25'), y=102.5, size = 6, color = cardinal,
  ) +
  geom_text(label="CBD", x=as.Date('2020-11-01'), y=100, size = 6, color = teal,
  ) +
  theme_minimal()+
  theme(text = element_text(size=24),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 24)
  )
#legend.position=c(.85, .5)
ggsave(dir + 'figures/appendix_a4b.png', plot = last_plot(), width = 10, height = 8)


###########################################
## Appendix A5: Past macro shocks
###########################################
## A. 9/11
df2 <- read_csv('https://files.zillowstatic.com/research/public_v2/zhvi/Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_mon.csv?t=1620068499', 
                col_types = cols(RegionName = col_double())) %>% 
  rename(zip = 'RegionName', MsaShort = 'Metro') %>%
  mutate(MetroShort = sub("-.*", "", MsaShort),
         MetroShort = paste(MetroShort, State, sep = ', ')) %>%
  select(!c(RegionID, SizeRank, RegionType, StateName, State, City, CountyName, MsaShort)) %>%
  pivot_longer(!c(zip, MetroShort), names_to = 'date', values_to = 'zhvi')
chars <- read_csv('~/Documents/zillow/thesis/data/zip_all_chars_cbd.csv')

chars %>% filter(MetroShort %in% cities, !is.na(`2010 Population`), !is.na(dist_to_cbd)) %>%
  mutate(quantile_rank = ntile(density2019, 10),
         category = ifelse(dist_to_cbd < 2000, 'cbd', 
                           case_when(quantile_rank == 10 ~ 'high',
                                     quantile_rank %in% c(6:9) ~ 'mid',
                                     TRUE ~ 'low'))) %>%
  inner_join(df2, by = 'zip') %>% 
  mutate(date = as.Date(date),
         zhvi_pop = zhvi*`2010 Population`) %>%
  filter(date >= as.Date('2000-07-01'), date < as.Date('2002-07-01')) %>%
  group_by(zip) %>% mutate(zhvi_pop = na.approx(zhvi_pop, na.rm=FALSE)) %>% 
  group_by(category, date) %>%
  summarise(zhvi_pop = sum(zhvi_pop, na.rm = TRUE),
            population = sum(`2010 Population`, na.rm = TRUE)) %>% 
  mutate(zhvi = zhvi_pop/population) %>%
  group_by(category) %>% mutate(zhvi = zhvi/zhvi[date == as.Date('2001-08-31')]*100) %>%
  mutate(
    val = zhvi, name = category,
    type = case_when(str_detect(name, 'high') ~ 'high density',
                     str_detect(name, 'mid') ~ 'mid density',
                     str_detect(name, 'cbd') ~ 'CBD',
                     TRUE ~ 'low density'), emph = "b"
  ) %>% ggplot(
    aes(x=date, y = val, color = type, linetype = as.factor(emph), 
        alpha = as.factor(emph),size = as.factor(emph)), guide='none') +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  geom_line()+
  xlim(as.Date('2000-07-01'), as.Date('2002-12-30')) + 
  geom_vline(xintercept=as.Date('2001-08-31'), size=.5, color="black") + 
  scale_colour_manual(values = c(teal, cardinal, black, green), guide='none')+
  geom_dl(aes(label = type), method = list(cex = 1, 'last.bumpup', cex = 1.5)) +   
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
#legend.position=c(.85, .5)
ggsave(dir + 'figures/appendix_a5a.png', plot = last_plot(), width = 10, height = 8)

## B. Great Recession
chars %>% filter(MetroShort %in% cities, !is.na(`2010 Population`), !is.na(dist_to_cbd)) %>%
  mutate(quantile_rank = ntile(density2019, 10),
         category = ifelse(dist_to_cbd < 2000, 'cbd', 
                           case_when(quantile_rank == 10 ~ 'high',
                                     quantile_rank %in% c(6:9) ~ 'mid',
                                     TRUE ~ 'low'))) %>%
  inner_join(df2, by = 'zip') %>% 
  mutate(date = as.Date(date),
         zhvi_pop = zhvi*`2010 Population`) %>%
  filter(date >= as.Date('2007-01-01'), date < as.Date('2009-01-01')) %>%
  group_by(zip) %>% mutate(zhvi_pop = na.approx(zhvi_pop, na.rm=FALSE)) %>% 
  group_by(category, date) %>%
  summarise(zhvi_pop = sum(zhvi_pop, na.rm = TRUE),
            population = sum(`2010 Population`, na.rm = TRUE)) %>% 
  mutate(zhvi = zhvi_pop/population) %>%
  group_by(category) %>% mutate(zhvi = zhvi/zhvi[date == as.Date('2007-12-31')]*100) %>%
  mutate(
    val = zhvi, name = category,
    type = case_when(str_detect(name, 'high') ~ 'high density',
                     str_detect(name, 'mid') ~ 'mid density',
                     str_detect(name, 'cbd') ~ 'CBD',
                     TRUE ~ 'low density'), emph = "b"
  ) %>% ggplot(
    aes(x=date, y = val, color = type, linetype = as.factor(emph), 
        alpha = as.factor(emph),size = as.factor(emph)), guide='none') +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  geom_line()+
  xlim(as.Date('2007-01-01'), as.Date('2009-06-01')) + 
  geom_vline(xintercept=as.Date('2007-12-31'), size=.5, color="black") + 
  geom_dl(aes(label = type), method = list(cex = 1.5, 'last.bumpup', cex = 1.5)) +   
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
#legend.position=c(.85, .5)
ggsave(dir + 'figures/appendix_a5b.png', plot = last_plot(), width = 10, height = 8)


###########################################
## Appendix A6: Cumulative flows
###########################################
chars <- read_csv(dir + 'data/zip_all_chars_cbd.csv', 
                  col_types = cols('zip' = col_integer())) %>% select(!estab_count)
bus_chars <- read_csv(dir + 'zbp_wfh.csv',
                      col_types = cols('zip' = col_integer()))
chars <- chars %>% inner_join(bus_chars, by = 'zip')

usps <- read_csv(dir + 'data/USPS_zips.csv')

## A. population flows
chars %>% filter(MetroShort %in% cities, !is.na(`2019 Population`)) %>%
  mutate(quantile_rank = ntile(density, 10),
         category = if_else(dist_to_cbd <2000, 'cbd', 
                            case_when(quantile_rank == 10 ~ 'high',
                                      quantile_rank %in% c(6:9) ~ 'mid',
                                      TRUE ~ 'low'), case_when(quantile_rank == 10 ~ 'high', 
                                                               quantile_rank %in% c(6:9) ~ 'mid', TRUE ~ 'low'))) %>%
  inner_join(usps, by = 'zip') %>% 
  mutate(date = as.Date(date)) %>%
  group_by(category, date) %>%
  summarise(net = sum(net, na.rm = TRUE)/sum(`2019 Population`)*100,
            net_bus = sum(net_bus, na.rm = TRUE)/sum(`2019 Population`)*100,
            net_pop = sum(net_pop, na.rm = TRUE)/sum(`2019 Population`)*100,
  ) %>% 
  pivot_wider(id_cols = 'date', names_from = 'category', values_from = 'net_pop') %>% 
  select(date, low, mid, high, cbd) %>% 
  filter(date > as.Date('2018-01-01')) %>%
  mutate (
    
    low = cumsum(low - low[date == as.Date('2020-02-15')]),
    mid = cumsum(mid - mid[date == as.Date('2020-02-15')]),
    high = cumsum(high - high[date == as.Date('2020-02-15')]),
    cbd = cumsum(cbd - cbd[date == as.Date('2020-02-15')]),
  ) %>%
  pivot_longer(c(low, mid, high, cbd),
               names_to = "name", values_to = "val") %>% 
  mutate(emph = "b"
  ) %>% ggplot(
    aes(x=date, y = val, color = name, linet_permype = as.factor(emph), 
        alpha = as.factor(emph),size = as.factor(emph)), guide='none') +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  geom_line()+
  xlim(as.Date('2018-01-01'), as.Date('2021-03-01')) + 
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  scale_colour_manual(values = c(teal, cardinal, black, green), guide='none')+
  scale_size_manual(values = c(1.5, 0.8), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "Deviations from Jan 2018 (% points)"
  )+
  geom_text(label="Feb 2020", x=as.Date('2019-12-01'), y=-5, size = 6, color = 'black',
  ) +
  geom_text(label="low density", x=as.Date('2020-12-15'), y=.8, size = 6, color = black,
  ) +
  geom_text(label="mid density", x=as.Date('2020-12-15'), y=-1.5, size = 6, color = green,
  ) +
  geom_text(label="high density", x=as.Date('2020-12-15'), y=-7.5, size = 6, color = cardinal,
  ) +
  geom_text(label="CBD", x=as.Date('2021-01-15'), y=-13, size = 6, color = teal,
  ) +
  theme_minimal()+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)
  )
#legend.position=c(.85, .5)
ggsave(dir + 'figures/appendix_a7a.png', plot = last_plot(), width = 10, height = 8)

## B. business flows
chars %>% filter(MetroShort %in% cities, !is.na(`2019 Population`)) %>%
  mutate(quantile_rank = ntile(density, 10),
         category = if_else(dist_to_cbd <2000, 'cbd', 
                            case_when(quantile_rank == 10 ~ 'high',
                                      quantile_rank %in% c(6:9) ~ 'mid',
                                      TRUE ~ 'low'), case_when(quantile_rank == 10 ~ 'high', 
                                                               quantile_rank %in% c(6:9) ~ 'mid', TRUE ~ 'low'))) %>%
  inner_join(usps, by = 'zip') %>% 
  mutate(date = as.Date(date)) %>%
  group_by(category, date) %>%
  summarise(net_bus = sum(net_bus, na.rm = TRUE)/sum(estab_count)*100) %>% 
  pivot_wider(id_cols = 'date', names_from = 'category', values_from = 'net_bus') %>% 
  select(date, low, mid, high, cbd) %>% 
  filter(date > as.Date('2018-01-01')) %>%
  mutate (
    
    low = cumsum(low - low[date == as.Date('2020-02-15')]),
    mid = cumsum(mid - mid[date == as.Date('2020-02-15')]),
    high = cumsum(high - high[date == as.Date('2020-02-15')]),
    cbd = cumsum(cbd - cbd[date == as.Date('2020-02-15')]),
  ) %>%
  pivot_longer(c(low, mid, high, cbd),
               names_to = "name", values_to = "val") %>% 
  mutate(emph = "b"
  ) %>% ggplot(
    aes(x=date, y = val, color = name, linet_permype = as.factor(emph), 
        alpha = as.factor(emph),size = as.factor(emph)), guide='none') +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  geom_line()+
  xlim(as.Date('2018-01-01'), as.Date('2021-03-01')) + 
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  scale_colour_manual(values = c(teal, cardinal, black, green), guide='none')+
  scale_size_manual(values = c(1.5, 0.8), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "Deviations from Jan 2018 (% points)"
  )+
  geom_text(label="Feb 2020", x=as.Date('2019-12-01'), y=-5, size = 6, color = 'black',
  ) +
  geom_text(label="low density", x=as.Date('2020-12-15'), y=1, size = 6, color = black,
  ) +
  geom_text(label="mid density", x=as.Date('2020-12-15'), y=-.8, size = 6, color = green,
  ) +
  geom_text(label="high density", x=as.Date('2021-01-15'), y=-3.5, size = 6, color = cardinal,
  ) +
  geom_text(label="CBD", x=as.Date('2021-02-15'), y=-7, size = 6, color = teal,
  ) +
  theme_minimal()+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)
  )
#legend.position=c(.85, .5)
ggsave(dir + 'figures/appendix_a7b.png', plot = last_plot(), width = 10, height = 8)



