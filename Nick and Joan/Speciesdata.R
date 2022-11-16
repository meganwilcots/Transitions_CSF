## --------------------------------------------------- ##
          # Transitions - Spp. Occurrence Data
## --------------------------------------------------- ##
# Written by Joan Dudney & Nick J Lyon

## ------------------------------------ ##
           # Housekeeping ----
## ------------------------------------ ##

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, spocc, sf, terra)

# Clear environment
rm(list = ls())

## ------------------------------------ ##
       # Occurrence Wrangling ----
## ------------------------------------ ##

# Identify species synonyms
sp_names <- c("Poa pratensis")

# Identify sources we want to check
sources <- c("gbif", "ecoengine", "bison")

# Identify occurrence records for a given species
## Note this function is *very* slow (worse if `limit` is set high)
raw_occ <- spocc::occ(query = sp_names,
                      from = sources,
                      limit = 1000, # set to 10,000 for relevant sample size
                      geometry = c(-140, 22, -58, 5), # bounding box
                      has_coords = TRUE)

# Make an empty list
occ_list <- list()

# Strip relevant information from that listed object
for(k in sources){
  for(sp in sp_names){
    # Swap the space in the species name for an underscore
    sp_fix <- gsub(pattern = " ", replacement = "_", x = sp)
      
    # Strip out that species from that source
  occ_list[[paste0(k, "__", sp_fix)]] <- raw_occ[[k]]$data[[sp_fix]]
  } # close sp loop
} # close source loop

# Wrangle this object
occ_df <- occ_list %>%
  # Unlist to a dataframe
  purrr::map_dfr(.f = dplyr::bind_rows) %>%
  # Pare down to only needed columns
  dplyr::select(name, longitude, latitude, geodeticDatum) %>%
  # Keep only unique rows
  unique() %>%
  # Get a new standardized species name column
  dplyr::mutate(species = sp_names[1], .before = dplyr::everything()) %>%
  # Drop old name column
  dplyr::select(-name) %>%
  # Drop impossible coordinates
  dplyr::filter(!is.na(longitude) & !is.na(latitude))
  
# Glimpse the data
dplyr::glimpse(occ_df)
# tibble::view(occ_df)

## ------------------------------------ ##
          # Spatial Wrangling ----
## ------------------------------------ ##

# Check whether the CRS is always WGS84
if(unique(occ_df$geodeticDatum) == "WGS84"){
  message("Creation of 'occ_spatial' with `crs` set to EPSG code 4326 is correct")
} else{ message("Need to tweak creation of 'occ_spatial' for non-WGS84 CRS") }

# Make a spatial variant
occ_pts <- occ_df %>%
  # Drop CRS column
  dplyr::select(-geodeticDatum) %>%
  # Make spatial (with package `sf`)
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  # Summarize within species
  dplyr::group_by(species) %>% 
  dplyr::summarize()

# Plot points
plot(occ_pts, axes = T)

# Now make it a polygon
occ_poly <- occ_pts %>%
  # Convert to polygon (instead of disparate points)
  st_cast("POLYGON") %>%
  # Convert the polygon into a boundary
  sf::st_convex_hull(x = .)

# Plot polygon
base::plot(occ_poly, axes = T, reset = F)
base::plot(occ_pts, add = T, pch = 20, col = "gray45")

# Export polygon as a shapefile
sf::st_write(obj = occ_poly, delete_layer = T,
             dsn = file.path("data", paste0(sp_names[1], "_border-sf.shp")))

# Convert to raster
occ_rast <- terra::vect(occ_poly)

# Check structure
str(occ_rast)

# Plot to see
plot(occ_rast, axes = T)

# Export raster
terra::writeVector(x = occ_rast,
                   filename = file.path("data", 
                                        paste0(sp_names[1], "_border-rast.shp")))

# End ----
