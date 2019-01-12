

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

# Load helper functions
sapply(list.files(codedir, ".R"), function(x) source(file.path(codedir, x)))

# Load final model results
load(file.path(outputdir, "RAMLDB_ricker_sst.Rdata"))

# Load life history data
lhdata <- read.csv(file.path(ramdir, "ramldb_life_history_data.csv"), as.is=T)


# Merge data
################################################################################

# Temperature stats
tstats <- data %>%
  group_by(stockid) %>% 
  summarize(sst_c_avg=mean(sst_c),
            sst_c_trend=freeR::slope(lm(sst_c~year)),
            sst_c_cv=sd(sst_c)/mean(sst_c))

# Build final data
final <- stocks %>% 
  # Add life history data
  left_join(unique(select(lhdata, -c(species_orig, species_use, temp_c))), by="species") %>% 
  # Add SST stats
  left_join(tstats, by="stockid") %>% 
  # Add parameter estimates
  left_join(results$stock, by="stockid")

# Complete
freeR::complete(final)


# Export table
################################################################################

# Export
write.csv(final, file.path(outputdir, "final_model_results.csv"), row.names=F)

