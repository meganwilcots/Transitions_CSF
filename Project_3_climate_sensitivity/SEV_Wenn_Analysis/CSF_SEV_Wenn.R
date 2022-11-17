## Fit CSFs for SEV Wennedex Experiment

#Run SEV_wennedex_ClimateCleaning and sev_wennedex_data_cleaning code prior to running this code
library(here)
library(vegan)
library(tidyverse)
library(nlme)
library(emmeans)
library(MuMIn)
library(visreg)
library(car)
library(piecewiseSEM)

#Create SEV_Wenn_Analysis folder
dir.create(file.path("Project_3_climate_sensitivity","SEV_Wenn_Analysis"), showWarnings = FALSE)

#Reshape exp data into wide form
exp.wide<- sev_wen_clean %>%
  pivot_wider(names_from = species, values_from = abundance,
              values_fill = 0)

#Calculate total cover
Var_1 <- "HYFIC" #First plant
Var_2 <- "TECO" #Last plant
exp.tot<- exp.wide %>%
  mutate(tot.cover = select(., all_of(Var_1):all_of(Var_2)) %>% rowSums(na.rm = TRUE))

#Filter only fall year data by only including years that end in .5 (fall)
exp.fall<-subset(exp.tot,substring(exp.tot$year,6,6)=="5")

#add new year column that drops the .5
exp.fall$substring_year = str_sub(exp.fall$year,1,4)

#Add climate data
exp.clim <- merge.data.frame(exp.fall,sev_wenn_clim, by.x="substring_year",by.y="year")


#Clean up data frames
rm(exp.fall, exp.tot, exp.wide, sev_wen_clean, sev_wenn_clim)

#Wednesday's Log-Log Models 
m1<-lme(log(tot.cover)~ log(annualpcp) + log(mean_maxairt), random = ~ 1|plot, data=exp.clim)
summary(m1)

m2<-lme(log(tot.cover)~ log(annualpcp) + log(mean_airt), random = ~ 1|plot, data=exp.clim)
summary(m2)


## ------------------------------ ##
# Statistics Extraction
## ------------------------------ ##
# Written by Nick J Lyon

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, nlme, NCEAS/scicomptools)

# Extract model components
est1 <- scicomptools::nlme_extract(fit = m1) 
est2 <- scicomptools::nlme_extract(fit = m2) 

# Take a look
est1
est2

# Can now export that as a CSV / to Google Drive as desired!


## ------------------------------ ##
# Site Summary Statistics
## ------------------------------ ##
sev_wenn_clim %>%
  summarize(mean_temp = mean(mean_airt),
            range_mean_temp = range(mean_airt),
            mean_max_temp = mean(mean_maxairt),
            range_max_temp = range(mean_maxairt),
            mean_precip = mean(annualpcp),
            range_precip = range(annualpcp)) ->
  sumstats


## ------------------------------ ##
# Determine Site Dominant Species
## ------------------------------ ##

