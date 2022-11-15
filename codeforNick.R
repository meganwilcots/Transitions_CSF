
library(vegan)
library(tidyverse)
library(nlme)
library(emmeans)
library(MuMIn)
library(visreg)
library(car)
library(piecewiseSEM)

## niwot climate data
source("Data_climate/NWT_prelim/cleaning_clim.R")

## niwot exp data
source("Data_cleaning/nwt_fert_data_cleaning.R")

##=================================================================================================================
##                      ANNUAL CLIMATE DATA
##==================================================================================================================
exp.clim <- nwt_fert_clean %>% 
  filter(species == "Deschampsia cespitosa") %>% 
  group_by(year,field, site,nadd, plot, uniqueID) %>% 
  summarize(tot.cover = sum(abundance, na.rm=T)) %>% 
  left_join(nwt_temp_precip) %>%
  na.omit()

## climate model selection
## Annual
m.null<-lme(tot.cover~airtemp_max + precip, data=exp.clim,random=~1|uniqueID,method="ML")

summary(m.null)

