### SEV WENNDEx Data Cleaning ###


## Data manipulation packages
library(tidyverse)
library(googledrive)#read this tutorial to setup the Google account permissions https://nceas.github.io/scicomp.github.io/tutorials.html#using-the-googledrive-r-package

#Create directory for file download 
# I put this directory in to the .gitignore so it does not affect the repository when changes are pushed
dir.create("raw_data_GDrive", showWarnings = F)

#Create a tibble with all of the files in the LTER transitions directory
#Don't need to change this since this is where all of the raw files are located
files_ls <- drive_ls(as_id("https://drive.google.com/drive/folders/1I_RFbh_YkkYHapP7H0J3gXXkUf6-nmqL"))

#Google asks for access to the pre-authorized Google account 
#my account is under "1" yours may be different 


#Download the dataset based on the file name in the directory
drive_download(file = subset(files_ls,name=="SEV_wenndex_biomass_2021.csv"),
               path = "raw_data_GDrive/SEV_wenndex_biomass_2021.csv",
               overwrite = TRUE) #Overwrite is in included so you can replace older versions


#load data from the directory
sevwen<-read.table("raw_data_GDrive/SEV_wenndex_biomass_2021.csv", sep=",",header = T)
## Working data frame

unique(sevwen$site)
# only 1 site--warming site

unique(sevwen$year)
#2006-2021 (15 years of data)

unique(sevwen$season)
#Twice a year sampling

sevwen$seas_num<-as.numeric(as.character(recode_factor(sevwen$season,fall="0.5",spring="0")))
sevwen$year_seas<-sevwen$year+sevwen$seas_num

### Add ExpYear
df <- data.frame(year_seas= seq(from=min(sevwen$year_seas),to=max(sevwen$year_seas), by=0.5),
                 ExpYear= seq(from=1,to=(max(sevwen$year_seas)-min(sevwen$year_seas)+1), by=0.5))
sevwen <- merge.data.frame(sevwen, df, by="year_seas" )

## Split up the different types of treatments into separate columns
unique(sevwen$treatment)
# 8 unique trts
#T=night time warming
#P=winter precip addition
#N= N addition
#C=control
treat.df<-data.frame(treatment=unique(sevwen$treatment),
                     warm=c("T","C","C","C","T","C","T","T"),
                     rainfall=c("C","P","C","P","C","C","P","P"),
                     Nadd=c("C","C","C","N","N","N","C","N"))

sevwen <- merge.data.frame(sevwen, treat.df, by="treatment")

## Clean up SEV WENNDEX DATA!

sev_wen_clean <- sevwen %>%
  as_tibble() %>%
  dplyr::select(-web, -transect, -block, -subplot) %>%  # Remove unwanted columns
  as_tibble() %>%
  ## format column names to match the rest of the datasets
  dplyr::mutate(site = "sev",
                project = "wen",
                year = year_seas, #to deal with fall/spring,
                expyear = ExpYear,
                plot = plot, 
                subplot = quad,   # check if this does make sense (this is correct)
                abundance = biomass.BM, #biomass based on best model
                unitAbund = "biomass.BM",
                scaleAbund = "g/m2",   # check metadata
                cover = cover/100, #cover was entered as % of 1m^2 
                unitcover = "cover",
                scalecover = "m2",
                trt = treatment,
                carbon = "0",
                nadd = Nadd,
                ncess = "0",
                fence = "0",
                burn = "0",
                rainfall = rainfall, 
                warm = warm,
                species = paste(genus, sp.epithet, sep =" "),
                fungroup = FunctionalGroup,
                photopath = PhotoPath,
                season.ppt = season.precip, #check which months this includes from meta data--not stated ask Anny or Jenn
                spei = SPEI.comp,
                uniqueID = paste(site, project, plot, subplot, sep = "_")) %>%
  dplyr::select(year, expyear, site, project, plot, subplot, uniqueID, photopath,
                carbon, nadd, ncess, fence, burn, rainfall, warm,
                species, fungroup, abundance, unitAbund, scaleAbund, cover, unitcover, scalecover, season.ppt, spei)

rm(files_ls, df, treat.df, sevwen)

#write.csv(sev_wen_clean, "../data/sev_wen_clean.csv") #Note: need to update the csv file with the new cleaned sev_wen data from this script (link is currently broken)