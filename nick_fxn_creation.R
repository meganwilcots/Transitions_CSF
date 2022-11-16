## ------------------------------ ##
      # Stat Extract Function
## ------------------------------ ##

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, nlme)

# Clear environment
rm(list = ls())

## ------------------------------ ##
     # Pre-Function Stuff ----
## ------------------------------ ##

# Read in data
df <- read.csv(file = file.path("data", "exp_clim.csv"))

# Fit model
my_fit <- nlme::lme(tot.cover ~ airtemp_max + precip, data = df, 
            random = ~1|uniqueID, method = "ML")

# Summarize
summary(my_fit)

## ------------------------------ ##
    # Function Development ----
## ------------------------------ ##

# Grab summary as an object
summ_fit <- summary(my_fit)

# Find model estimates
summ_fit$tTable

# Find number observations
summ_fit$dims$N

# Strip out row names
fit_strip <- cbind(data.frame("Term" = rownames(summ_fit$tTable),
                              "N_obs" = summ_fit$dims$N),
                   summ_fit$tTable)

# Drop old (flawed row names)
rownames(fit_strip) <- NULL

# Check out actual product
fit_strip

## ------------------------------ ##
      # Function Actual ----
## ------------------------------ ##

nlme_extract <- function(fit = NULL){
  
  # Error out for missing fit
  if(is.null(fit))
    stop("`fit` must be specified")
  
  # Error out for inappropriate type of fit
  if(class(fit) != "lme")
    stop("`fit` must be class 'lme'")
  
  # Grab summary of the fit object
  table <- base::summary(fit)
  
  # Grab all relevant bits
  strip <- cbind(data.frame("Term" = rownames(table$tTable),
                            "N_obs" = table$dims$N),
                 table$tTable)
  
  # Drop old (flawed row names)
  rownames(strip) <- NULL
  
  # Return final table
  return(strip)
}

# Invoke function
nlme_extract(fit = my_fit)

# End ----
