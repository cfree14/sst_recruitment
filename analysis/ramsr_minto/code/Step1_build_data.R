
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(freeR)

# Directories
datadir <- "data/ramsr_minto"
inputdir <- "input"

# Read data
load(file.path(datadir, "ramsr_minto_data.Rdata"))


# Identify usable stocks
################################################################################

# Things to do:
# Add SST data
# Remove freshwater or anadramous species?
fw_species <- c("Pomoxis annularis", "Esox lucius")

# Identify stocks to use
stocks_orig <- stocks
stocks <- stocks_orig %>% 
  filter(nyr>=20 & !(species %in% fw_species) & family!="Salmonidae")

# Identify data to use
data_orig <- data
data <- data_orig %>% 
  # Reduce to stocks of interest
  filter(stockid %in% stocks$stockid) %>% 
  filter(!is.na(r_annual) & !is.na(ssb_scaled)) %>%
  # Standardize - center around one
  group_by(stockid) %>% 
  mutate(r_sd=exp(log(r_annual) - mean(log(r_annual))),
         ssb_sd=exp(log(ssb_scaled) - mean(log(ssb_scaled)))) %>% 
  # Ensure alphabetical arrangement
  arrange(stockid, year)
  
# Check standardiization
aggregate(data$r_sd, by=list(data$stockid), mean) # centered aorund 1
aggregate(data$ssb_sd, by=list(data$stockid), mean) # centered around 1

# Convince yourself that standardizations work
par(mfrow=c(1,2))
plot(r_annual ~ ssb_scaled, data, subset=stockid=="BWHIT")
plot(r_sd ~ ssb_sd, data, subset=stockid=="BWHIT")


# Export stocks
################################################################################

# Export
save(stocks, data, file=file.path(inputdir, "ramsr_minto_data_use.Rdata"))







