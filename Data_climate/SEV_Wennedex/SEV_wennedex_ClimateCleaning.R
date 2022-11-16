#This script cleans the SEV_Wennedex Climate Data

library(here)
library(vegan)
library(tidyverse)
library(googledrive)

#Create folder
dir.create(file.path("Data_climate","SEV_Wennedex"), showWarnings = FALSE)

#List files in google drive, filter desired *climate data* csv file and download it locally
drive_ls(as_id("https://drive.google.com/drive/folders/1I_RFbh_YkkYHapP7H0J3gXXkUf6-nmqL")) %>%
  dplyr::filter(name == "SEV_wennedex_met_monthly_gap_filled.csv") %>%
  googledrive::drive_download(path=file.path("Data_climate","SEV_Wennedex", "SEV_wennedex_met_monthly_gap_filled.csv"),
                              overwrite = T)

#Read in climate data
wenn.clim<-read.csv(here::here("Data_climate","SEV_Wennedex", "SEV_wennedex_met_monthly_gap_filled.csv"))


#Calculate annual ppt
wenn.clim %>% 
  filter(sta=="40") %>% #Subset data to only include station 40 (Per Anny's expertise)
  mutate(ppt=as.numeric(ppt)) %>%
  select(year, ppt) %>%
  group_by(year) %>%
  summarise(annualpcp = sum(ppt, na.rm=T)) ->
  annual_pcp
  

#Calculate mean max temp
wenn.clim %>% 
  filter(sta=="40") %>% #Subset data to only include station 40 (Per Anny's expertise)
  mutate(maxair=as.numeric(maxair)) %>%
  select(year, maxair) %>%
  group_by(year) %>%
  summarise(mean_maxairt = mean(maxair, na.rm=T)) ->
  mean_max_temp


#Join annual ppt and mean max temp data by year
annual_pcp %>% left_join(mean_max_temp, by = "year") ->
  final_sev_wenn_clim

rm(wenn.clim, annual_pcp, mean_max_temp)



