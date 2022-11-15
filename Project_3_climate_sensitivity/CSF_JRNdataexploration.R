#JRN CSF code using JRN data from data cleaning/harmonized coed
#Fit CSFs for Jornada Precip Variability Data Cleaning experiment

library(here)
library(vegan)
library(tidyverse)
library(nlme)
library(emmeans)
library(MuMIn)
library(visreg)
library(car)
library(piecewiseSEM)
library(googledrive)

#List files in google drive, filter desired *experiment data* csv file and download it locally
drive_ls(as_id("https://drive.google.com/drive/folders/1I_RFbh_YkkYHapP7H0J3gXXkUf6-nmqL")) %>%
  dplyr::filter(name == "JRN_pvar_2018.csv") %>%
  googledrive::drive_download(path=file.path("Project_3_climate_sensitivity","JRN data", "JRN_pvar_2018.csv"),
                              overwrite = T)

#List files in google drive, filter desired *climate data* csv file and download it locally
drive_ls(as_id("https://drive.google.com/drive/folders/1I_RFbh_YkkYHapP7H0J3gXXkUf6-nmqL")) %>%
  dplyr::filter(name == "JRN_pvar_clim.csv") %>%
  googledrive::drive_download(path=file.path("Project_3_climate_sensitivity","JRN data", "JRN_pvar_clim.csv"),
                              overwrite = T)

#Read in cleaned experiment data and climate data
exp.dat<-read.csv(here::here("Project_3_climate_sensitivity","JRN data", "JRN_pvar_2018.csv"))
#clim.dat<-read.csv(here::here("Project_3_climate_sensitivity","JRN data", "JRN_pvar_clim.csv")) 



## Data reshaping and combining----------------------

#Reshape exp data into wide form
exp.wide<- exp.dat %>%
  pivot_wider(names_from = plant_fun_type, values_from = anpp,
              values_fill = 0)

#Add climate data
exp.clim <- merge.data.frame(exp.wide, #clim.dat,by.x="year",by.y="year_seas")

#Model selection----------------
#Candidate model set
#L=linear, Q=quadratic, C=cubic, a=additive, i=interaction
m.null<-lme(total~year*nadd,data=exp.clim,random=~1|uniqueID,method="ML")
m.La<-lme(total~SPEI.comp+nadd,data=exp.clim,random=~1|uniqueID,method="ML")
m.Li<-lme(total~SPEI.comp*nadd,data=exp.clim,random=~1|uniqueID,method="ML")
m.Qa<-lme(total~SPEI.comp+nadd+I(SPEI.comp^2),data=exp.clim,random=~1|uniqueID,method="ML")
m.Qi<-lme(total~SPEI.comp*nadd+I(SPEI.comp^2)*nadd,data=exp.clim,random=~1|uniqueID,method="ML")
m.Ca<-lme(total~SPEI.comp+nadd+I(SPEI.comp^2)+I(SPEI.comp^3),data=exp.clim,random=~1|uniqueID,method="ML")
m.Ci<-lme(total~SPEI.comp*nadd+I(SPEI.comp^2)*nadd+I(SPEI.comp^3)*nadd,data=exp.clim,random=~1|uniqueID,method="ML")

# model selection
AICc(m.null, m.La,m.Li,m.Qa,m.Qi,m.Ca,m.Ci)
min(AICc(m.null, m.La,m.Li,m.Qa,m.Qi,m.Ca,m.Ci)[,2])
#Best model is .......



