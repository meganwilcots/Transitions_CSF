### cleaning konza data and data exploration ###
rm(list = ls())
setwd("~/Desktop/lter transitions data/")
library(tidyverse)
knz_biom <- read.csv("konza_biomass.csv")
knz_weather <- read.csv("konza_weather.csv")

names(knz_biom)
knz_biom <- select(knz_biom, -CUYRDEAD, -PRYRDEAD, -WOODY, -COMMENTS)
uniqueID <- unite(knz_biom, col = "uniqueID", c("WATERSHED", "SOILTYPE", "TRANSECT", "PLOTNUM"), sep = "-")
knz_biom$uniqueID <- uniqueID$uniqueID ##make a unique ID column and put it in the big df
knz_biom$biomass <- (knz_biom$LVGRASS*10) + (knz_biom$FORBS*10) ## multiply by 10 to get to g/m2 (found units on knz website)

### what do we need: annual biomass per unique ID

knz_biom1 <- knz_biom %>%
  group_by(RECYEAR, uniqueID) %>%
  summarize(ANPP = sum(biomass))
knz_biom1 <- na.omit(knz_biom1)
names(knz_biom1) <- c("year", "uniqueID", "ANPP")

ggplot(knz_biom1, aes(x = year, y = ANPP)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method= "loess", se = F) +
  theme_bw()

ppt <- knz_weather$DPPT
knz_weather$ppt <- ifelse(knz_weather$DPPT == ".", 0, knz_weather$DPPT)
knz_weather$ppt <- as.numeric(knz_weather$ppt)
knz_weather$TMAX <- as.numeric(knz_weather$TMAX)
knz_weather1 <- knz_weather %>%
  group_by(RECYEAR, RECMONTH) %>%
  summarize(max_temp_c = mean(TMAX),
            total_precip = sum(ppt))

knz_weather1 <- na.omit(knz_weather1) ## lose 13 rows
names(knz_weather1) <- c("year", "month", "max_temp_c", "MAP_mm")
library(SPEI)

#Calculate balances

knz_weather1$PET <- thornthwaite(knz_weather1$max_temp_c, 39.1069)
knz_weather1$BAL <- knz_weather1$MAP_mm - knz_weather1$PET
summary(knz_weather1)
ggplot(knz_weather1,aes(x=year, y=BAL))+geom_point(aes(color=month))
knz_weather1$month <- as.numeric(knz_weather1$month)
knz_weather1$year <- as.numeric(knz_weather1$year)

knz_weather1_ts = ts(knz_weather1[,-c(1,2)],end = c(2021,12), frequency=12)

library(ggpp)
#6 month spei 
spei_knz6=spei(knz_weather1_ts[,"BAL"], 6)
spei_knz6_df = try_data_frame(spei_knz6$fitted)
colnames(spei_knz6_df)=c("time","SPEI_6m")
head(spei_knz6_df)
spei_knz6_df$year=format(as.Date(spei_knz6_df$time),"%Y")
spei_knz6_df$month=format(as.Date(spei_knz6_df$time),"%m")
spei_knz6_df$time=NULL

august_spei <- filter(spei_knz6_df, month == "08")
august_spei$year <- as.numeric(august_spei$year)
august_spei$month <- as.numeric(august_spei$month)
knz_weather2 <- knz_weather1 %>%
  group_by(year) %>%
  summarize(MAP_mm = sum(MAP_mm),
            max_t = mean(max_temp_c))

knz_all <- left_join(knz_weather2, august_spei, by = "year")
knz_all1 <- left_join(knz_all, knz_biom1, by = "year")

knz_final <- na.omit(knz_all1)

knz_standardized <- knz_final %>%
  mutate(MAP_stand = scale(MAP_mm),
         temp_stand = scale(max_t),
         NPP_stand = scale(ANPP))

library(nlme)
mod_tp <- lme(NPP_stand ~ MAP_stand + temp_stand, random = ~1|uniqueID, data = knz_standardized)
mod_spei <- lme(NPP_stand ~ SPEI_6m, random = ~1|uniqueID, data = knz_standardized)

summary(mod_tp)
summary(mod_spei)
library(vegan)

sumstats <- knz_final %>%
  summarize(mean_max_temp = mean(max_t),
            range_max_temp = range(max_t),
            mean_precip = mean(MAP_mm),
            range_precip = range(MAP_mm),
            mean_spei= mean(SPEI_6m),
            range_spei = range(SPEI_6m))

##visualizing
ggplot(knz_standardized, aes(x = SPEI_6m, y = NPP_stand)) +
  geom_point() + 
  geom_smooth(method = "lm", se = T) +
  theme_bw()

ggplot(knz_standardized, aes(x = MAP_stand, y = NPP_stand)) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  theme_bw()


### test w/ data in the google sheet

test_data <- tibble(site = c("CDR", "KBS", "KNZ"),
                    coef_temp = c(-0.033, -0.102, 0.090),
                    se_temp = c(0.031, 0.066, 0.014),
                    coef_precip = c(0.115, 0.036, 0.186),
                    se_precip = c(0.031, 0.066, 0.013),
                    coef_spei = c(-0.098, -0.044, 0.083),
                    se_spei = c(0.033, 0.066, 0.013),
                    MAP = c(831.052, 905.7, 796.1),
                    max_temp = c(12.5, 34.8, 19.3),
                    mean_spei = c(-0.616, 0.207, 0.015))

ggplot(test_data, aes(x = MAP, y = coef_precip, color = site)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = coef_precip - se_precip, ymax = coef_precip + se_precip), width = 1.4) +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = 2) +
  ylab("Coef. standardized(NPP ~ MAP)") +
  xlab("MAP")

ggplot(test_data, aes(x = max_temp, y = coef_temp, color = site)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = coef_temp - se_temp, ymax = coef_temp + se_temp), width = 1) +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = 2)

ggplot(test_data, aes(x = mean_spei, y = coef_spei, color = site)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = coef_spei - se_spei, ymax = coef_spei + se_spei), width = 0.05) +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = 2)
