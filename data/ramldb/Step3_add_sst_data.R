
# Use PT data
# Use data based on STOCKID and not ASSESSID

# Setup
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(plyr)
library(dplyr)
library(reshape2)

# Directories
datadir <- "data/ramldb/data"
plotdir <- "data/ramldb/figures"
sstdir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/productivity/data/sst/data/averages"
bndrydir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/productivity/data/stock_boundaries/data"

# Read potential RAM data
load(file.path(datadir, "ramldb_stock_recruit_data_trimmed.Rdata"))
data_orig <- data
stocks_orig <- stocks

# Read SST data
cobe <- read.csv(paste(sstdir, "ramldb_sst_yearly_cobe.csv", sep="/"), as.is=T)

# Read stock boundary key
key <- read.csv(file.path(bndrydir, "ramldb_v3.8_stock_boundary_table_v2.csv"), as.is=T)

# Read centroids key
centroids <- read.csv(file.path(bndrydir, "ramldb_v3.8_stock_boundary_centroids_areas_fixed.csv"), as.is=T)

# Build assessid, stockid, lme key
key1 <- key %>% 
  select(assessid, stockid) %>% 
  left_join(select(centroids, assessid, lme_name), by="assessid")

# Build data
################################################################################

# Format SST data
sst <- cobe %>% 
  left_join(select(key1, assessid, stockid, lme_name), by="assessid") %>% 
  select(stockid, lme_name, year, sst_c)

# Add SST data
data1 <- data_orig %>% 
  # Add SST data
  left_join(sst, by=c("stockid", "year")) %>% 
  select(stockid, lme_name, year, ssb, r, sst_c) %>% 
  filter(!is.na(sst_c)) %>% 
  # Add order/family data
  left_join(select(stocks_orig, stockid, order, family), by="stockid") %>% 
  # Arrange columns
  select(stockid, lme_name, order, family, everything())

# Check sample size
check <- data1 %>% 
  group_by(stockid) %>% 
  summarize(nyr=n()) %>% 
  filter(nyr>=20)

# Final data
stocks <- filter(stocks_orig, stockid %in% check$stockid)

# Standardize data
data <- data1 %>% 
  # Reduce to final stocks
  filter(stockid %in% check$stockid) %>% 
  # Standardize SSB, R, and SST
  group_by(stockid) %>% 
  mutate(r_sd=r/sd(r),
         ssb_sd=ssb/sd(ssb),
         # r_sd=exp(log(r) - mean(log(r))),
         # ssb_sd=exp(log(ssb) - mean(log(ssb))),
         sst_sd=sst_c-mean(sst_c))

# Confirm standardization
aggregate(data$r_sd, by=list(data$stockid), sd) # SD=1
aggregate(data$ssb_sd, by=list(data$stockid), sd) # SD=1
aggregate(data$sst_sd, by=list(data$stockid), mean) # Mean=0

# Plot
par(mfrow=c(1,2))
plot(r ~ ssb, data, subset=stockid=="CODGB")
plot(r_sd ~ ssb_sd, data, subset=stockid=="CODGB")

# Complete
freeR::complete(data)
freeR::complete(stocks)

# Build data
################################################################################

# Export
save(data, stocks, file=file.path(datadir, "ramldb_stock_recruit_data_trimmed_sst.Rdata"))

