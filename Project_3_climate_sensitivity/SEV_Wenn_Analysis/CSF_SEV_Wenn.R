## Fit CSFs for SEV WENNDEx Experiment

#Run SEV_wenndex_ClimateCleaning and sev_wenndex_data_cleaning code prior to running this code
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
Var_1 <- "Hymenopappus filifolius" #First plant
Var_2 <- "Tetraclea coulteri" #Last plant
exp.tot<- exp.wide %>%
  mutate(tot.cover = select(., all_of(Var_1):all_of(Var_2)) %>% rowSums(na.rm = TRUE))

#Filter only fall year data by only including years that end in .5 (fall)
exp.fall<-subset(exp.tot,substring(exp.tot$year,6,6)=="5") 

#add new year column that drops the .5
exp.fall$substring_year = str_sub(exp.fall$year,1,4)

#Add climate data
exp.clim <- merge.data.frame(exp.fall,sev_wenn_clim, by.x="substring_year",by.y="year")

#Clean up data frames
rm(exp.fall, exp.tot, exp.wide, sev_wen_clean)


#Model selection for Fall----------------
#Candidate model set
#L=linear, Q=quadratic, C=cubic
#a=additive, i=interaction
m.null<-lme(tot.cover~year*nadd,data=exp.clim,random=~1|uniqueID,method="ML")
m.La<-lme(tot.cover~spei+nadd,data=exp.clim,random=~1|uniqueID,method="ML")
m.Li<-lme(tot.cover~spei*nadd,data=exp.clim,random=~1|uniqueID,method="ML")
m.Qa<-lme(tot.cover~spei+nadd+I(spei^2),data=exp.clim,random=~1|uniqueID,method="ML")
m.Qi<-lme(tot.cover~spei*nadd+I(spei^2)*nadd,data=exp.clim,random=~1|uniqueID,method="ML")
m.Ca<-lme(tot.cover~spei+nadd+I(spei^2)+I(spei^3),data=exp.clim,random=~1|uniqueID,method="ML")
m.Ci<-lme(tot.cover~spei*nadd+I(spei^2)*nadd+I(spei^3)*nadd,data=exp.clim,random=~1|uniqueID,method="ML")

# model selection
AICc(m.null, m.La,m.Li,m.Qa,m.Qi,m.Ca,m.Ci)
min(AICc(m.null, m.La,m.Li,m.Qa,m.Qi,m.Ca,m.Ci)[,2])
#Note: Best model is m.Ca, the next best fit model is m.Ci


##Pick up here! 


# AR1 - autocorrelation 1, AR2 - autocorrelation 2 to best model from above
#This is only necessary if expyear includes non-integer numbers
#int.year <- exp.clim$expyear*2-1 #convert to integer
#exp.clim$expyear <- int.year

#Fit temporal autocorrelation models
m.AR1<-update(m.Ca,correlation=corAR1(form=~expyear))
m.AR2<-update(m.Ca,correlation=corARMA(form=~expyear,p=2))

# model selection
AICc(m.Ca,m.AR1,m.AR2)
# best model m.AR2
rsquared(m.AR2)
#Marginal R2:  the proportion of variance explained by the fixed factor(s) alone
#Conditional R2: he proportion of variance explained by both the fixed and random factors

#Evaluate model assumptions
plot(m.AR2)#a bit funnel shaped
qqPlot(residuals(m.AR2))
hist(residuals(m.AR2))

#Do sketchy frequentist tests on best model
anova(m.AR2,type="marginal")#F test
Anova(m.AR2,type=2)# Chisq test. Mostly similar except for significance of "SPEI.comp"

#Param estimates and post-hoc pairwise comparisons
emtrends(m.AR2,~ nadd | degree, "SPEI.comp", max.degree = 3)
pairs(emtrends(m.AR2,~ nadd | degree , "SPEI.comp", max.degree = 3))
#The quadratic slope is the most different

#Visualize CSF results---------------------
# get a plot of estimated values from the model, by each depth
# visreg with ggplot graphics
visreg(fit=m.AR2,"SPEI.comp",type="conditional",by="nadd",gg=TRUE,partial=F,rug=F)+ 
  geom_point(aes(x=SPEI.comp,y=tot.cover,col=year),alpha=0.2,data=exp.clim)+
  theme_bw()+
  labs(x="SPEI",
       y="Aboveground total cover")

#Alternative visualization code if the above doesn't work
exp.clim$predicted<-predict(m.AR2, exp.clim)
ggplot(exp.clim, aes(x=SPEI.comp, y=predicted)) +
  facet_wrap(~nadd)+
  geom_point(aes(x=SPEI.comp, y=tot.cover), color="gray60", size=0.5) +
  geom_smooth(aes(y=predicted), color="gray20")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  labs(x="SPEI",
       y="Aboveground total cover")










## ------------------------------ ##
#Work from the November meeting 
## ------------------------------ ##
#Wednesday's Log-Log Models 
m1<-lme(log(tot.cover)~ log(annualpcp) + log(mean_maxairt), random = ~ 1|uniqueID, data=exp.clim)
summary(m1)
##P-value for log(annualpcp) is zero. This is weird! 

m2<-lme(log(tot.cover)~ log(annualpcp) + log(mean_airt), random = ~ 1|uniqueID, data=exp.clim)
summary(m2)
##P-value for intercept and log(annualpcp) is zero. This is also weird! 

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
  climate_sumstats


## ------------------------------ ##
# Determine Site Dominant Species
# by determining 90th percentile of
# abundance
## ------------------------------ ##


#load data from the directory
exp.dat<-read.table("raw_data_GDrive/SEV_wenndex_biomass_2021.csv", sep=",",header = T)

#Visualize count data of species cover
ggplot(exp.dat, aes(x=biomass.BM, color=kartez)) + 
  geom_histogram()

#Get species totals over time and all plots
exp.dat %>%
  group_by(kartez) %>% #kartez is the species ID
  summarise(tot.biomass= sum(biomass.BM))->
  tot_biomass_data

#Get most abundant species
new<-exp.dat[exp.dat$kartez 
             %in% subset(tot_biomass_data, tot.biomass>=
                           quantile(tot_biomass_data$tot.biomass, probs=0.90))$kartez,] #brackets used for indexing


#plot most abundant species
x <-ggplot(new, aes(x=biomass.BM, color=kartez)) + 
  geom_histogram()

## ------------------------------ ##
# General Analytical Models-Friday
## ------------------------------ ##

m3<-lme(log(tot.cover)~ log(annualpcp) * log(mean_airt), random = ~ 1|uniqueID, data=exp.clim)
summary(m3)


