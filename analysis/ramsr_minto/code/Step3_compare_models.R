
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(freeR)
library(reshape2)
library(TMB)
library(TMBhelper)

# Directories
basedir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/sst_recruitment"
inputdir <- file.path(basedir, "input")
outputdir <- file.path(basedir, "output")
plotdir <- file.path(basedir, "figures")
codedir <- file.path(basedir, "code")

# Read helper functions
source(file.path(codedir, "helper_functions.R"))

# Compare models
################################################################################

# Read in models

# Merge models into list

# Compare models

