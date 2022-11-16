## ------------------------------ ##
      # Statistics Extraction
## ------------------------------ ##
# Written by Nick J Lyon

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, nlme, NCEAS/scicomptools)

# Clear environment
rm(list = ls())

## ------------------------------ ##
      # Actual Workflow ----
## ------------------------------ ##

# Read in data
df <- read.csv(file = file.path("data", "exp_clim.csv"))

# Fit model
my_fit <- nlme::lme(tot.cover ~ airtemp_max + precip, data = df, 
            random = ~1|uniqueID, method = "ML")

# Extract model components
est <- scicomptools::nlme_extract(fit = my_fit)

# Take a look
est

# Can now export that as a CSV / to Google Drive as desired!

# For more information:
?scicomptools::nlme_extract

# End ----
