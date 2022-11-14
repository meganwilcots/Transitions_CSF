

library(vegan)
library(tidyverse)
library(nlme)
library(emmeans)
library(MuMIn)
library(visreg)
library(car)
library(piecewiseSEM)


## niwot cliamte data
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
m.null<-lme(tot.cover~year*nadd + soiltemp_5cm_max + precip, data=exp.clim,random=~1|uniqueID,method="ML")
m1<-lme(tot.cover~year*nadd + soiltemp_5cm_min + precip, data=exp.clim,random=~1|uniqueID,method="ML")
m2<-lme(tot.cover~year*nadd +  airtemp_min + precip, data=exp.clim,random=~1|uniqueID,method="ML")
m3<-lme(tot.cover~year*nadd +  airtemp_max + precip, data=exp.clim,random=~1|uniqueID,method="ML")

AIC(m.null, m1, m2, m3)
min(AICc(m.null, m1, m2, m3)[,2])

##=================================================================================================================
##                      WINTER CLIMATE DATA
##==================================================================================================================
## Excluding summer months July-September
exp.clim_winter <- nwt_fert_clean %>% 
  filter(species == "Deschampsia cespitosa") %>% 
  group_by(year,field, site,nadd, plot, uniqueID) %>% 
  summarize(tot.cover = sum(abundance, na.rm=T)) %>% 
  left_join(nwt_temp_precip_winter) %>% 
  na.omit()

## slightly better than annual
m.null<-lme(tot.cover~year*nadd + soiltemp_5cm_max + precip, data=exp.clim_winter,random=~1|uniqueID,method="ML")
m1<-lme(tot.cover~year*nadd + soiltemp_5cm_min + precip, data=exp.clim_winter,random=~1|uniqueID,method="ML")
m2<-lme(tot.cover~year*nadd +  airtemp_min + precip, data=exp.clim_winter,random=~1|uniqueID,method="ML")
m3<-lme(tot.cover~year*nadd +  airtemp_max + precip, data=exp.clim_winter,random=~1|uniqueID,method="ML")

AIC(m.null, m1, m2, m3)
min(AICc(m.null, m1, m2, m3)[,2])


##=================================================================================================================
##                    SUMMER CLIMATE DATA  
##==================================================================================================================
## Only summer months June - Sept
exp.clim_summer <- nwt_fert_clean %>% 
  filter(species == "Deschampsia cespitosa") %>% 
  group_by(year,field, site,nadd, plot, uniqueID) %>% 
  summarize(tot.cover = sum(abundance, na.rm=T)) %>% 
  left_join(nwt_temp_precip_summer) %>% 
  na.omit()

## slightly better
m.null<-lme(tot.cover~year*nadd + soiltemp_5cm_max + precip, data=exp.clim_summer,random=~1|uniqueID,method="ML")
m1<-lme(tot.cover~year*nadd + soiltemp_5cm_min + precip, data=exp.clim_summer,random=~1|uniqueID,method="ML")
m2<-lme(tot.cover~year*nadd +  airtemp_min + precip, data=exp.clim_summer,random=~1|uniqueID,method="ML")
m3<-lme(tot.cover~year*nadd +  airtemp_max + precip, data=exp.clim_summer,random=~1|uniqueID,method="ML")

AIC(m.null, m1, m2, m3)
min(AICc(m.null, m1, m2, m3)[,2])


##=================================================================================================================
##    WINTER MONTHS ARE SLIGHTLY BETTER THAN ANNUAL; SUMMER IS LESS PREDICTIVE; SOIL TEMP MAX IS THE BEST  
##==================================================================================================================


#Model selection----------------
#Candidate model set
#L=linear, Q=quadratic, C=cubic, a=additive, i=interaction
m.null<-lme(tot.cover~year*nadd, data=exp.clim_winter,random=~1|uniqueID,method="ML")
m.La<-lme(tot.cover~soiltemp_5cm_max+nadd,data=exp.clim_winter,random=~1|uniqueID,method="ML")
m.Li<-lme(tot.cover~soiltemp_5cm_max*nadd,data=exp.clim_winter,random=~1|uniqueID,method="ML")
m.Qa<-lme(tot.cover~soiltemp_5cm_max+nadd+I(soiltemp_5cm_max^2),data=exp.clim_winter,random=~1|uniqueID,method="ML")
m.Qi<-lme(tot.cover~soiltemp_5cm_max*nadd+I(soiltemp_5cm_max^2)*nadd,data=exp.clim_winter,random=~1|uniqueID,method="ML")
m.Ca<-lme(tot.cover~soiltemp_5cm_max+nadd+I(soiltemp_5cm_max^2)+I(soiltemp_5cm_max^3),data=exp.clim_winter,random=~1|uniqueID,method="ML")
m.Ci<-lme(tot.cover~soiltemp_5cm_max*nadd+I(soiltemp_5cm_max^2)*nadd+I(soiltemp_5cm_max^3)*nadd,data=exp.clim_winter,random=~1|uniqueID,method="ML")


# model selection
AICc(m.null, m.La,m.Li,m.Qa,m.Qi,m.Ca,m.Ci)
min(AICc(m.null, m.La,m.Li,m.Qa,m.Qi,m.Ca,m.Ci)[,2])


m.precip <- lme(tot.cover~year*nadd+precip, data=exp.clim_winter,random=~1|uniqueID,method="ML")
m.noprecip <- lme(tot.cover~year*nadd+soiltemp_5cm_max, data=exp.clim_winter,random=~1|uniqueID,method="ML")
m.both <- lme(tot.cover~year*nadd+soiltemp_5cm_max+precip, data=exp.clim_winter,random=~1|uniqueID,method="ML")
m.both.int <- lme(tot.cover~year*nadd+soiltemp_5cm_max*precip, data=exp.clim_winter,random=~1|uniqueID,method="ML")
m.both.quad <- lme(tot.cover~year*nadd+I(soiltemp_5cm_max^2)+I(precip^2), data=exp.clim_winter,random=~1|uniqueID,method="ML")

  
AICc(m.null, m.La,m.Li,m.Qa,m.Qi,m.Ca,m.Ci, m.precip, m.noprecip, m.both, m.both.int, m.both.quad)
min(AICc(m.null, m.La,m.Li,m.Qa,m.Qi,m.Ca,m.Ci, m.precip, m.noprecip, m.both, m.both.int, m.both.quad)[,2])

## summary of model selection
## interaction with precip and soil temps leads to best results
## quadratic terms not saying much

summary(m.both.int)




#Best model is m.Ci, but within 2 of m.Qi

# AR1 - autocorrelation 1, AR1 - autocorrelation 2 to best model from above
#This is only necessary if expyear includes non-integer numbers
int.year <- exp.clim_winter$year*2-1 #convert to integer
exp.clim_winter$year <- int.year

#Fit temporal autocorrelation models
## quad model
# m.AR1<-update(m.both.quad,correlation=corAR1(form=~year))
# m.AR2<-update(m.both.quad,correlation=corARMA(form=~year,p=2))

## interaction models
m.AR1<-update(m.both.int,correlation=corAR1(form=~year))
m.AR2<-update(m.both.int,correlation=corARMA(form=~year,p=2))
# model selection
AICc(m.Ci,m.AR1,m.AR2)
# best model m.AR2
rsquared(m.AR2)

#Marginal R2:  the proportion of variance explained by the fixed factor(s) alone
#Conditional R2: he proportion of variance explained by both the fixed and random factors

#Evaluate model assumptions
plot(m.AR2)
qqPlot(residuals(m.AR2))
hist(residuals(m.AR2))

#Do sketchy frequentist tests on best model
anova(m.AR2,type="marginal")#F test
Anova(m.AR2,type=2)# Chisq test. 

#Param estimates and post-hoc pairwise comparisons
emtrends(m.AR2,~ nadd | degree, "precip", max.degree = 3)
pairs(emtrends(m.AR2,~ nadd | degree , "precip", max.degree = 3))


#Visualize CSF results---------------------
# get a plot of estimated values from the model, by each depth
# visreg with ggplot graphics
visreg(fit=m.AR2,"precip",type="conditional",by="nadd",gg=TRUE,partial=F,rug=F)+ 
  geom_point(aes(x=precip,y=tot.cover,col=year),alpha=0.2,data=exp.clim_winter)+
  theme_bw()+
  labs(x="precip",
       y="Aboveground total cover")

#Alternative visualization code if the above doesn't work
exp.clim_winter$predicted<-predict(m.AR2, exp.clim_winter)
ggplot(exp.clim_winter, aes(x=precip, y=predicted)) +
  facet_wrap(~nadd)+
  geom_point(aes(x=precip, y=tot.cover), color="gray60", size=0.5) +
  geom_smooth(aes(y=predicted), color="gray20")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  labs(x="Precipitation (winter months, mm)",
       y="Aboveground total cover Desch")

ggplot(exp.clim_winter, aes(x=soiltemp_5cm_max, y=predicted)) +
  facet_wrap(~nadd)+
  geom_point(aes(x=soiltemp_5cm_max, y=tot.cover), color="gray60", size=0.5) +
  geom_smooth(aes(y=predicted), color="gray20")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  labs(x="Soil temp. (winter months)",
       y="Aboveground total cover Desch")



##=================================================================================================================
##    GLMMS                  
##==================================================================================================================
library(lme4)
library(sjPlot)
library(ggeffects)

m.both.int <- lmer(tot.cover~year*nadd+soiltemp_5cm_max*precip+(1|uniqueID), data=exp.clim_winter)
m.both.quad <- lmer(tot.cover~year*nadd+I(soiltemp_5cm_max^2)+I(precip^2) +(1|uniqueID), data=exp.clim_winter)
m.both <- lmer(tot.cover~year*nadd+soiltemp_5cm_max+precip +(1|uniqueID), data=exp.clim_winter)
AIC(m.both.int, m.both.quad, m.both)

tab_model(m.both, m.both.int, m.both.quad)


predplot=ggpredict(m.both, terms=c( "soiltemp_5cm_max"))
plot(predplot)+geom_point()

predplot=ggpredict(m.both.int, terms=c( "soiltemp_5cm_max"))
plot(predplot)



##=================================================================================================================
##                      
##    
##    RICHNESS DATA
##    
##==================================================================================================================

richness <- nwt_fert_clean %>% 
  filter(!species%in%c("Bare Ground", "Soil", "Litter", "Moss")) %>% 
  group_by(year,field, site,nadd, plot, uniqueID) %>% 
  #summarize(rich = paste(unique(species), collapse = ","))
  summarize(rich= length(unique(species))) %>% 
  left_join(nwt_temp_precip_winter) %>% 
  na.omit()

m.null_rich <- lme(rich~year*nadd, data=richness,random=~1|uniqueID,method="ML")
m.precip_rich <- lme(rich~year*nadd+precip, data=richness,random=~1|uniqueID,method="ML")
m.noprecip_rich <- lme(rich~year*nadd+soiltemp_5cm_max, data=richness,random=~1|uniqueID,method="ML")
m.both_rich <- lme(rich~year*nadd+soiltemp_5cm_max+precip, data=richness,random=~1|uniqueID,method="ML")
m.both.int_rich <- lme(rich~year*nadd+soiltemp_5cm_max*precip, data=richness,random=~1|uniqueID,method="ML")
m.both.quad_rich <- lme(rich~year*nadd+I(soiltemp_5cm_max^2)+I(precip^2), data=richness,random=~1|uniqueID,method="ML")

AICc(m.null_rich,m.precip_rich, m.noprecip_rich, m.both_rich, m.both.int_rich, m.both.quad_rich)


tab_model(m.null_rich,m.both.int_rich, m.both.int)
