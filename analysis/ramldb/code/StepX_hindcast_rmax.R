

# Setup
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(plyr)
library(dplyr)
library(reshape2)

# Directories
codedir <- "code"
ramdir <- "data/ramldb/data"
outputdir <- "analysis/ramldb/output"
tabledir <- "analysis/ramldb/tables"
sstdir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/productivity/data/sst/data/averages"
bndrydir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/productivity/data/stock_boundaries/data"

# Load final model results
load(file.path(outputdir, "RAMLDB_ricker_sst.Rdata"))

# Read SST data
sst <- read.csv(paste(sstdir, "ramldb_sst_yearly_cobe.csv", sep="/"), as.is=T)

# Read stock boundary key
# THIS WILL BE UNNECESSARY WHEN YOU FIX STOCKID PROBLEM
key <- read.csv(file.path(bndrydir, "ramldb_v3.8_stock_boundary_table_v2.csv"), as.is=T)

# Read final results
results <- read.csv(file.path(outputdir, "final_model_results.csv"), as.is=T)


# Build SST data set
################################################################################

# Hindcast years
yr1 <- 1930
yr2 <- 2010
hind_years <- yr1:yr2

# Build SST data set
hind <- sst %>% 
  # Add stockid
  left_join(select(key, assessid, stockid), by="assessid") %>% 
  select(stockid, year, sst_c) %>% 
  # Filter to stocks in data
  filter(stockid %in% stocks$stockid & year %in% hind_years) %>% 
  # Add alpha, beta, theta, and average temperature
  left_join(select(results, stockid, alpha, beta, theta, sst_c_avg), by="stockid") %>% 
  # Compute average temperature
  mutate(sst_c_sd=sst_c-sst_c_avg,
         rmax=alpha/beta*exp(theta*sst_c_sd)*exp(-1))
  
    

# Stock level RMAXs
results <- results %>% 
  mutate(rmax=alpha/beta*exp(-1))

hist(results$rmax[results$rmax<100], seq(0,100,0.5))

# Problem RMAX stocks
prob_rmax <- results %>% 
  filter(rmax>10)

# Calculate global RMAX trend
global <- hind %>% 
  filter(!stockid %in% prob_rmax$stockid) %>% 
  group_by(year) %>% 
  summarize(rmax_avg=mean(rmax))


# Plot RMAX over time
plot(rmax_avg ~ year, global, type="l", bty="n", las=1,
     xlab="", ylab=expression("RMAX (billions)"), xlim=c(yr1, yr2), xaxt="n") 
axis(1, at=seq(1930, 2010, 10), las=2)


