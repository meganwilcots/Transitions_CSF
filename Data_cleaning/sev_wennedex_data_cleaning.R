### SEV WENNDEX Data Cleaning ###


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
               overwrite = TRUE)#Overwrite is in included so you can replace older versions


#load data from the directory

sevwen<-read.table("raw_data_GDrive/SEV_wenndex_biomass_2021.csv", sep=",",header = T)

## Working data frame

unique(sevwen$site)
# only 1 site

### Add ExpYear
unique(sevwen$year)
#only include up to 2019 because precip treatments changed in 2020
sevwen<-subset(sevwen,year<2020)
df <- data.frame(year= 2006:2019,
                 ExpYear= 1:14)
sevwen <- merge.data.frame(sevwen, df, by="year" )

## Split up the different types of treatments into separate columns
unique(sevwen$treatment)
# 8 unique trts
#T=night time warming
#P=winter precip addition
#N= N addition
#C=control
treat.df<-data.frame(treatment=unique(sevwen$treatment),
                     warm=c("T","T","C","C","T","T","C","C"),
                     rainfall=c("C","P","P","C","P","C","P","C"),
                     Nadd=c("C","N","C","N","C","N","N","C"))
sevwen <- merge.data.frame(sevwen, treat.df, by="treatment" )

## Clean up data SEV WENNDEX!

sev_wen_clean <- sevwen %>%
  as_tibble() %>%
  dplyr::select(-web, -transect, -block, -subplot) %>%  # Remove unwanted columns
  as_tibble() %>%
  ## format column names to match the rest of the datasets
  dplyr::mutate(site = "sev",
                project = "wen",
                field = project,
                year = year,
                expyear = ExpYear,
                plot = plot, 
                subplot = quad,   # check if this does make sense
                abundance = biomass.BM, #biomass based on best model
                trt = treatment,
                carbon = "0",
                nadd = Nadd,
                ncess = "0",
                fence = "0",
                burn = "0",
                rainfall = rainfall, 
                warm = warm,
                unitAbund = "biomass",
                scaleAbund = "g/m2",   # check metadata
                species = kartez,
                fungroup = FunctionalGroup,
                uniqueID = paste(site, field, project, plot, subplot, sep = "_")) %>%
  dplyr::select(year, site, field, project, plot, subplot, uniqueID,
                carbon, nadd, ncess, fence, burn, rainfall, warm,
                species, fungroup, abundance, unitAbund, scaleAbund) 

rm(files_ls, df, treat.df, sevwen)

#write.csv(sev_wen_clean, "../data/sev_wen_clean.csv")