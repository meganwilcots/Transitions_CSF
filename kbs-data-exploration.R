### KBS Early Successional Microplot ANPP — Main Cropping System Experiment (MCSE) Data Cleaning ###

## Data manipulation packages
library(tidyverse)
library(SPEI)
library(ggpp)
library(nlme)
library(emmeans)
library(MuMIn)
library(visreg)
library(car)
library(piecewiseSEM)
library(cowplot)
#https://lter.kbs.msu.edu/datatables/448
kbs_monthly_MET= read.table(here::here("Project_3_climate_sensitivity","KBS_data","448-monthly+precipitation+and+air+temperature+1666860249.csv"),
                            sep = ",", header = T)
head(kbs_monthly_MET)
dim(kbs_monthly_MET)
#417   6

kbs_daily_MET= read.table(here::here("Project_3_climate_sensitivity","KBS_data","12-lter+weather+station+daily+weather+all+variates+1668639113.csv"),
                            sep = ",", header = T)
head(kbs_daily_MET)
dim(kbs_daily_MET)
#12728    26


#Calculate balances

kbs_monthly_MET$PET <- thornthwaite(kbs_monthly_MET$air_temp_mean, 42.415329)
kbs_monthly_MET$BAL <- kbs_monthly_MET$precipitation-kbs_monthly_MET$PET
summary(kbs_monthly_MET)
ggplot(kbs_monthly_MET,aes(x=year, y=BAL))+geom_point(aes(color=month))
kbs_monthly_MET_ts=ts(kbs_monthly_MET[,-c(1,2)],end = c(2022,9), frequency=12)

#6 month spei 
spei_KBS6=spei(kbs_monthly_MET_ts[,"BAL"], 6)
spei_KBS6_df=try_data_frame(spei_KBS6$fitted)
colnames(spei_KBS6_df)=c("time","SPEI_6m")
head(spei_KBS6_df)
spei_KBS6_df$year=as.numeric(format(as.Date(spei_KBS6_df$time),"%Y"))
spei_KBS6_df$month=as.numeric(format(as.Date(spei_KBS6_df$time),"%m"))
spei_KBS6_df$time=NULL
head(spei_KBS6_df)

#Annual precip and max temp
head(kbs_monthly_MET)
kbs_monthly_MET_annual=kbs_monthly_MET|>group_by(year)|>
  summarise(air_temp_max_max=max(air_temp_max), annual_precip=sum(precipitation),
            air_temp_max_mean=mean(air_temp_max), air_temp_mean_mean=mean(air_temp_mean))


ggplot(kbs_monthly_MET,aes(x=year,y=air_temp_max))+geom_point()
ggplot(kbs_monthly_MET_annual,aes(x=year,y=air_temp_max_mean))+geom_point()
#https://lter.kbs.msu.edu/datatables/686
source("Data_cleaning/kbs_early_succ_microplot_msce_cleaning.R") # extra column is disturbance treatment



head(KBS_spp_clean)
dim(KBS_spp_clean)
#3991   19
colnames(KBS_spp_clean)
unique(KBS_spp_clean$nadd)

#Remove nfert

KBS_spp_clean_control=subset(KBS_spp_clean, nadd=="unfertilized")
dim(KBS_spp_clean_control)
#2187   19

#Summarize abundance (pers spp biomass) to ANPP 

KBS_spp_clean_control_tot=KBS_spp_clean_control|>group_by(year,month,plot,nadd,uniqueID)|>
  summarise(ANPP=sum(abundance))
dim(KBS_spp_clean_control_tot)
summary(as.numeric(KBS_spp_clean_control_tot$year))

#Annual temp and precip

KBS_spp_clean_control_tot_temp_precip=merge(KBS_spp_clean_control_tot, kbs_monthly_MET_annual,
                                            by="year" )
head(KBS_spp_clean_control_tot_temp_precip)
KBS_spp_clean_control_tot_temp_precip=KBS_spp_clean_control_tot_temp_precip|>
  mutate(ANPP_scale=scale(ANPP), air_temp_max_mean_scale=scale(air_temp_max_mean), annual_precip_scale=scale(annual_precip))

dim(KBS_spp_clean_control_tot_temp_precip)


#Instantaneous max temp 
kbs_temp_precip_mod=lme(ANPP_scale~air_temp_max_mean_scale+annual_precip_scale,
                    data=KBS_spp_clean_control_tot_temp_precip,
                    random=~1|plot,method="ML")

summary(kbs_temp_precip_mod)

summary(KBS_spp_clean_control_tot_temp_precip[,c("annual_precip","air_temp_max_mean")])

# Extract model components
kbs_temp_precip_mod_est <- scicomptools::nlme_extract(fit = kbs_temp_precip_mod)

# Take a look
kbs_temp_precip_mod_est


KBS_rain_temp_sub=KBS_spp_clean_control_tot_temp_precip[!duplicated(KBS_spp_clean_control_tot_temp_precip$year),]
ggplot(KBS_rain_temp_sub|>pivot_longer(cols = c(annual_precip,air_temp_max_mean),names_to = "measure",values_to = "value"), aes(x=value))+geom_density()+facet_wrap(~measure,scales = "free")


kbs_temp_precip_LOG_mod=lme(log(ANPP)~log(air_temp_max_mean)+log(annual_precip),
                        data=KBS_spp_clean_control_tot_temp_precip,
                        random=~1|plot,method="ML")

summary(kbs_temp_precip_LOG_mod)

summary(KBS_spp_clean_control_tot_temp_precip[,c("annual_precip","air_temp_max_mean")])

# Extract model components
kbs_temp_precip_LOG_mod_est <- scicomptools::nlme_extract(fit = kbs_temp_precip_LOG_mod)

# Take a look
kbs_temp_precip_LOG_mod_est



#Identify the anomalies 
quantile(KBS_rain_temp_sub$air_temp_max_mean, c(0.05, 0.95)) ## find 5th and 9th percentile
quantile(KBS_rain_temp_sub$annual_precip, c(0.05, 0.95))
KBS_spei_sub$anomalies <- ifelse(KBS_spei_sub$SPEI_6m >= 1.763899, "anomaly",
                                 ifelse(KBS_spei_sub$SPEI_6m <= -1.384875, "anomaly", "normal")) ## classify 


ggplot(KBS_spei_sub, aes(x=SPEI_6m))+geom_density()+geom_dotplot(aes(color=anomalies,fill=anomalies))


#Instantaneous max temp 
kbs_temp_mean_precip_mod=lme(ANPP_scale~air_temp_mean_mean+annual_precip_scale,
                        data=KBS_spp_clean_control_tot_temp_precip,
                        random=~1|plot,method="ML")

summary(kbs_temp_mean_precip_mod)

summary(KBS_spp_clean_control_tot_temp_precip[,c("annual_precip","air_temp_mean_mean")])

# Extract model components
kbs_temp_mean_precip_mod_est <- scicomptools::nlme_extract(fit = kbs_temp_mean_precip_mod)

# Take a look
kbs_temp_mean_precip_mod_est


KBS_rain_temp_sub=KBS_spp_clean_control_tot_temp_precip[!duplicated(KBS_spp_clean_control_tot_temp_precip$year),]
ggplot(KBS_rain_temp_sub|>pivot_longer(cols = c(annual_precip,air_temp_max_mean),names_to = "measure",values_to = "value"), aes(x=value))+geom_density()+facet_wrap(~measure,scales = "free")


kbs_temp_mean_precip_LOG_mod=lme(log(ANPP)~log(air_temp_mean_mean)+log(annual_precip),
                            data=KBS_spp_clean_control_tot_temp_precip,
                            random=~1|plot,method="ML")

summary(kbs_temp_mean_precip_LOG_mod)

summary(KBS_spp_clean_control_tot_temp_precip[,c("annual_precip","air_temp_mean_mean")])

# Extract model components
kbs_temp_mean_precip_LOG_mod_est <- scicomptools::nlme_extract(fit = kbs_temp_mean_precip_LOG_mod)

# Take a look
kbs_temp_mean_precip_LOG_mod_est





#SPEI time
head(spei_KBS6_df)
KBS_spp_clean_control_spei=merge(KBS_spp_clean_control_tot, spei_KBS6_df,
                                            by=c("year","month") )
head(KBS_spp_clean_control_spei)
KBS_spp_clean_control_spei=KBS_spp_clean_control_spei|>mutate(ANPP_scale=scale(ANPP),
                                                              SPEI_6m_scale=scale(SPEI_6m))
dim(KBS_spp_clean_control_spei)

kbs_spei_mod=lme(ANPP_scale~SPEI_6m,
                    data=KBS_spp_clean_control_spei,
                    random=~1|plot,method="ML")
summary(KBS_spp_clean_control_spei$SPEI_6m)
summary(KBS_spp_clean_control_spei)
summary(kbs_spei_mod)

# Extract model components
kbs_spei_mod_est <- scicomptools::nlme_extract(fit = kbs_spei_mod)

# Take a look
kbs_spei_mod_est

ggplot(KBS_spp_clean_control_spei, aes(x=SPEI_6m,y=ANPP_scale))+geom_point()+
  geom_abline(slope = kbs_spei_mod_est[kbs_spei_mod_est$Term=="SPEI_6m",]$Value,
              intercept = kbs_spei_mod_est[kbs_spei_mod_est$Term=="(Intercept)",]$Value)+
  geom_smooth(color="red")

KBS_spei_sub=KBS_spp_clean_control_spei[!duplicated(KBS_spp_clean_control_spei[,c("month","year")]),]
ggplot(KBS_spei_sub, aes(x=SPEI_6m))+geom_density()

#Identify the anomalies 
quantile(KBS_spei_sub$SPEI_6m, c(0.05, 0.95)) ## find 5th and 9th percentile
KBS_spei_sub$anomalies <- ifelse(KBS_spei_sub$SPEI_6m >= 1.763899, "anomaly",
                             ifelse(KBS_spei_sub$SPEI_6m <= -1.384875, "anomaly", "normal")) ## classify 


ggplot(KBS_spei_sub, aes(x=SPEI_6m))+geom_density()+geom_dotplot(aes(color=anomalies,fill=anomalies))

#Log log model


#####Top five taxa####
colnames(KBS_spp_clean_control)

#total spp biomass

kbs_spp_biomass_tot<-KBS_spp_clean_control|>
  group_by(species)|>
  summarise(spp_bio_tot=sum(abundance))

ggplot(kbs_spp_biomass_tot,aes(x=spp_bio_tot))+
  geom_histogram()+geom_vline(xintercept = mean(kbs_spp_biomass_tot$spp_bio_tot))

kbs_spp_biomass_tot_year<-KBS_spp_clean_control|>
  group_by(species,year)|>
  summarise(spp_bio_tot=mean(abundance))

ggplot(kbs_spp_biomass_tot_year[kbs_spp_biomass_tot_year$species%in%
                                  subset(kbs_spp_biomass_tot,spp_bio_tot>=mean(kbs_spp_biomass_tot$spp_bio_tot))$species,],aes(x=year,y=spp_bio_tot, 
                                                                                                                               group=species, color=species))+geom_line()+theme(legend.position = "none")


plot_grid(ggplot(kbs_spp_biomass_tot_year|>group_by(year)|>summarise(bio_tot=sum(spp_bio_tot)),
                 aes(x=year,y=bio_tot))+geom_line()+theme(legend.position = "none"),
          ggplot(kbs_spp_biomass_tot_year[kbs_spp_biomass_tot_year$species%in%
                                            subset(kbs_spp_biomass_tot,spp_bio_tot>=quantile(kbs_spp_biomass_tot$spp_bio_tot, probs = 0.95))$species,]|>
                   group_by(year)|>summarise(dom_biomass=sum(spp_bio_tot)),aes(x=year,y=dom_biomass))+geom_line()+theme(legend.position = "none"),
          ggplot(kbs_spp_biomass_tot_year[kbs_spp_biomass_tot_year$species%in%
                                            subset(kbs_spp_biomass_tot,spp_bio_tot>=quantile(kbs_spp_biomass_tot$spp_bio_tot, probs = 0.90))$species,]|>
                   group_by(year)|>summarise(dom_biomass=sum(spp_bio_tot)),aes(x=year,y=dom_biomass))+geom_line()+theme(legend.position = "none"),nrow = 3)

length(subset(kbs_spp_biomass_tot,spp_bio_tot>=quantile(kbs_spp_biomass_tot$spp_bio_tot, probs = 0.90))$species)
#14
#Functional groups 
subset(kbs_spp_biomass_tot,spp_bio_tot>=quantile(kbs_spp_biomass_tot$spp_bio_tot, probs = 0.90))$species


KBS_spp_clean_control_90=KBS_spp_clean_control[KBS_spp_clean_control$species%in%
                                                 subset(kbs_spp_biomass_tot,spp_bio_tot>=
                                                          quantile(kbs_spp_biomass_tot$spp_bio_tot, probs = 0.90))$species,]

dim(KBS_spp_clean_control_90)
grass=c("Elymus repens (L.) Gould","Phleum pratense L.",
        "Poa pratensis L.")

herb_legume=c("Trifolium pratense L.")
herbaceous_perennial=
  c("Apocynum cannabinum L.","Aster pilosus Willd.",
    "Centaurea stoebe L. ssp. micranthos (Gugler) Hayek",
    "Euthamia graminifolia (L.) Nutt.",
    "Rubus allegheniensis T.C. Porter (*)")
herbaceous_annual=c("Conyza canadensis (L.) Cronq.","Daucus carota L.",
                    "Erigeron annuus (L.) Pers." ,"Solidago canadensis L."
                    )
woody_nfix=c("Robinia pseudoacacia L.")

KBS_spp_clean_control_90=KBS_spp_clean_control_90|>
  mutate(plant_funct=case_when(species%in%grass~"grass",
                               species%in%herb_legume~"herb_legume",
                               species%in%herbaceous_perennial~"herbaceous_perennial",
                               species%in%herbaceous_annual~"herbaceous_annual",
                               species%in%woody_nfix~"woody_nfix"))



KBS_spp_clean_control_90_funct=
  KBS_spp_clean_control_90|>group_by(plant_funct,year)|>
  summarise(funct_abun=sum(abundance))


plot_grid(ggplot(KBS_spp_clean_control|>group_by(year)|>summarise(bio_tot=sum(abundance)),
                 aes(x=year,y=bio_tot))+geom_line()+theme(legend.position = "none"),
          ggplot(KBS_spp_clean_control_90_funct,aes(x=year,y=funct_abun,color=plant_funct))+geom_line(),
          align = "v",axis = "r",nrow = 2)



kbs_spp_biomass_tot$species
kbs_spp_biomass_tot[order(kbs_spp_biomass_tot$spp_bio_tot,decreasing = T),][1:5,]$species

KBS_spp_clean_control_tops<-
  KBS_spp_clean_control[KBS_spp_clean_control$species%in%kbs_spp_biomass_tot[order(kbs_spp_biomass_tot$spp_bio_tot,decreasing = T),][1:6,]$species,]

ggplot(KBS_spp_clean_control_tops,aes(x=as.numeric(year),y=abundance))+
  geom_point()+facet_wrap(~species,scales = "free_y")



