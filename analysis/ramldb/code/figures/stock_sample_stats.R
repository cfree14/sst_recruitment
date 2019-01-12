
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

# Read SR model object
load(file.path(outputdir, "RAMLDB_ricker.Rdata"))

# Taxanomic representation
################################################################################

# Total # of stocks
nrow(stocks)

# Number of problem stocks
length(prob_stocks)

# Taxa stats
n_distinct(stocks$species)
n_distinct(stocks$family)
n_distinct(stocks$order)
n_distinct(stocks$class)

# Class stats
class_stats <- stocks %>% 
  group_by(class) %>% 
  summarize(n=n())

# Order stats
order_stats <- stocks %>% 
  group_by(class, order) %>% 
  summarize(n=n())


# Proportion of reported catch
################################################################################

# Read RAMLDB
ramdir <- "/Users/cfree/Dropbox/Prelim Database Files/Versions/RAM v4.41 (8-20-18)/DB Files With Assessment Data"
source(file.path(ramdir, "DBdata (assessment data only).RData"))

# Get catch in MT only
# Calculate in year with highest representation - hopefully 2000

# Catch time series
colnames(timeseries_values_views)
catch <- timeseries_values_views %>% 
  setNames(tolower(colnames(.))) %>% 
  select(stockid, year, tc, tl) %>% 
  left_join(select(timeseries_units_views, stockid, TC, TL), by="stockid") %>% 
  rename(tc_units=TC, tl_units=TL) %>% 
  mutate(catch=pmax(tc, tl, na.rm=T)) %>% 
  filter(stockid %in% stocks$stockid)

table(catch$tl_units)










