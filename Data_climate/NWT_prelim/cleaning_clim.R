

library(vegan)
library(tidyverse)
library(nlme)
library(emmeans)
library(MuMIn)
library(visreg)
library(car)
library(piecewiseSEM)


## this code reads and cleans temperature and precip data for the saddle on niwot ridge

temp_data <- read.csv("Data_climate/niwot_clim/saddle_clim.csv") %>% 
  rename_all(tolower) %>% 
  select(lter_site, date, year, airtemp_max, airtemp_min,soiltemp_5cm_max, soiltemp_5cm_min) %>% 
  separate(date, c("year","month", "day"), "-") %>% 
  select(-c(day, month)) %>% 
  group_by(lter_site, year) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)


precip_data <- read.csv("Data_climate/niwot_clim/saddle_precip_mm.csv") %>% 
  rename_all(tolower) %>% 
  select(lter_site, ppt_tot, date) %>% 
  separate(date, c("year","month", "day"), "-") %>% 
  select(-day) %>% 
  group_by(lter_site, year) %>% 
  summarise(precip=sum(ppt_tot, na.rm=T))



nwt_temp_precip <- precip_data %>% 
  left_join(temp_data) %>% 
  mutate(year=as.numeric(year))


rm(precip_data, temp_data)


temp_data_winter <- read.csv("Data_climate/niwot_clim/saddle_clim.csv") %>% 
  rename_all(tolower) %>% 
  select(lter_site, date, year, airtemp_max, airtemp_min,soiltemp_5cm_max, soiltemp_5cm_min) %>% 
  separate(date, c("year","month", "day"), "-") %>% 
  mutate(year=as.numeric(year)) %>% 
  mutate(month=as.numeric(month),
         growing=ifelse(month%in%c(10:12), year+1, year)) %>% 
  filter(!month%in%c(7:9)) %>% 
  select(-c(day, month)) %>% 
  group_by(lter_site, year) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

precip_data_winter <- read.csv("Data_climate/niwot_clim/saddle_precip_mm.csv") %>% 
  rename_all(tolower) %>% 
  select(lter_site, ppt_tot, date) %>% 
  separate(date, c("year","month", "day"), "-") %>% 
  mutate(year=as.numeric(year)) %>% 
  mutate(month=as.numeric(month),
         growing=ifelse(month%in%c(10:12), year+1, year)) %>% 
  filter(!month%in%c(7:9)) %>% 
  select(-day) %>% 
  group_by(lter_site, year) %>% 
  summarise(precip=sum(ppt_tot, na.rm=T))

nwt_temp_precip_winter <- precip_data_winter %>% 
  left_join(temp_data_winter)


rm(precip_data_winter, temp_data_winter)


## summer months

temp_data_summer <- read.csv("Data_climate/niwot_clim/saddle_clim.csv") %>% 
  rename_all(tolower) %>% 
  select(lter_site, date, year, airtemp_max, airtemp_min,soiltemp_5cm_max, soiltemp_5cm_min) %>% 
  separate(date, c("year","month", "day"), "-") %>% 
  mutate(month=as.numeric(month)) %>% 
  mutate(year=as.numeric(year)) %>% 
  filter(month%in%c(6:9)) %>% 
  select(-c(day, month)) %>% 
  group_by(lter_site, year) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

precip_data_summer <- read.csv("Data_climate/niwot_clim/saddle_precip_mm.csv") %>% 
  rename_all(tolower) %>% 
  select(lter_site, ppt_tot, date) %>% 
  separate(date, c("year","month", "day"), "-") %>% 
  mutate(month=as.numeric(month)) %>% 
  mutate(year=as.numeric(year)) %>% 
  filter(month%in%c(6:9)) %>% 
  select(-day) %>% 
  group_by(lter_site, year) %>% 
  summarise(precip=sum(ppt_tot, na.rm=T))

nwt_temp_precip_summer <- precip_data_summer %>% 
  left_join(temp_data_summer)


rm(precip_data_summer, temp_data_summer)



