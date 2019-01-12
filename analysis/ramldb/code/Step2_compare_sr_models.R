

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
outputdir <- "analysis/ramldb/output"
tabledir <- "analysis/ramldb/tables"

# Load helper functions
sapply(list.files(codedir, ".R"), function(x) source(file.path(codedir, x)))


# Read models
################################################################################

# Read SR model
load(file.path(outputdir, "RAMLDB_ricker.Rdata"))
ricker <- srfit

# Read SR-SST model
load(file.path(outputdir, "RAMLDB_ricker_sst.Rdata"))
ricker_sst <- srfit

# Read SR-SST-LME model
load(file.path(outputdir, "RAMLDB_ricker_sst_lme.Rdata"))
ricker_sst_lme <- srfit

# Compare models
models <- list(ricker, ricker_sst, ricker_sst_lme)
names <- c("Ricker", "SST-Ricker", "SST-LME-Ricker")
results <- compare_models(models, names)

# Export table
################################################################################

# Export
write.csv(results, file.path(tabledir, "Table1_model_comparison_aic.csv"), row.names=F)

