### cleaning konza data and data exploration ###
rm(list = ls())

knz_biom <- read.csv("konza_biomass.csv")
knz_weather <- read.csv("konza_weather.csv")

names(knz_biom)
knz_biom <- select(knz_biom, -CUYRDEAD, -PRYRDEAD, -WOODY, -COMMENTS)
uniqueID <- unite(knz_biom, col = "uniqueID", c("WATERSHED", "SOILTYPE", "TRANSECT", "PLOTNUM"), sep = "-")
knz_biom$uniqueID <- uniqueID$uniqueID ##make a unique ID column and put it in the big df
knz_biom$biomass <- knz_biom$LVGRASS + knz_biom$FORBS

### what do we need: annual biomass per unique ID

knz_biom1 <- knz_biom %>%
  group_by(RECYEAR, uniqueID) %>%
  summarize(ANPP = )