
# Things to do:
# Add assessid, model type
# Add B/BMSY, F/FMSY?

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

# Read RAM Legacy Database
load("/Users/cfree/Dropbox/Prelim Database Files/Versions/RAM v4.41 (8-20-18)/DB Files With Assessment Data/DBdata (assessment data only).Rdata")


# Build stock key
################################################################################

# Format age-at-recruitment
# sort(unique(bioparams$bioid))
r_age <- bioparams %>% 
  filter(bioid %in% c("REC-AGE-yr")) %>% 
  select(assessid, stockid, biovalue) %>% 
  rename(r_age_yr=biovalue) %>% 
  mutate(r_age_yr=revalue(r_age_yr, c("0.00E+00"=0,
                                      "1 to 70"=NA,
                                      "13 to 28 years to 50% commercial selectivity"=NA,
                                      "3+"=3,
                                      "5.936"=6)),
         r_age_yr=ceiling(as.numeric(r_age_yr)))
sort(unique(r_age$r_age_yr))

# Format assessment info
assess_info <- assessment %>% 
  # Reduce columns
  select(assessid, stockid, assessmethod, assessyear, mostrecent) %>% 
  # Identify most recent, properly
  mutate(year2=sapply(assessyear, function(x) as.numeric(unlist(strsplit(x, "-"))[2])),
         mostrecent=as.numeric(mostrecent)) %>% 
  group_by(stockid) %>% 
  filter(year2==max(year2))

# Check for and fix duplicates
dup_ids <- assess_info[duplicated(assess_info$stockid),]$stockid
dup_data <- filter(assess_info, stockid %in% dup_ids)
assess_info <- assess_info %>% 
  filter(!(stockid=="ANCHMEDGSA16" & mostrecent==0))

# Build stock key
stock_key <- stock %>% 
  select(-c(tsn, inmyersdb, myersstockid)) %>% 
  # Add area name
  left_join(select(area, areaid, country, areaname), by="areaid") %>% 
  # Add assessid
  left_join(select(assess_info, stockid, assessid, assessmethod), by="stockid") %>% 
  # Add age-at-recruitment
  left_join(select(r_age, assessid, r_age_yr), by="assessid") %>% 
  # Rename columns
  rename(species=scientificname, comm_name=commonname, area=areaname, method=assessmethod) %>% 
  # Format columns
  mutate(comm_name=freeR::sentcase(comm_name),
         species=gsub("spp.", "spp", species),
         species=revalue(species, c("Chrysophrys auratus"="Pagrus auratus",
                                    "Clupea pallasii"="Clupea pallasii pallasii",
                                    "Epinephelus flavolimbatus"="Hyporthodus flavolimbatus",
                                    "Epinephelus niveatus"="Hyporthodus niveatus",
                                    "Etrumeus teres"="Etrumeus sadina",
                                    "Loligo bleekeri"="Heterololigo bleekeri",
                                    "Loligo pealeii"="Doryteuthis pealeii",
                                    "Loligo reynaudii"="Loligo vulgaris reynaudii",
                                    "Merluccius gayi"="Merluccius gayi gayi",
                                    "Mullus barbatus"="Mullus barbatus barbatus",
                                    "Neoplatycephalus richardsoni"="Platycephalus richardsoni",
                                    "Psetta maxima"="Scophthalmus maximus",
                                    "Strangomera bentincki"="Clupea bentincki",
                                    "Tetrapturus albidus"="Kajikia albida",
                                    "Sardinops melanostictus"="Sardinops sagax"))) %>% 
  # Rearrange columns
  select(stockid, stocklong, assessid, method, country, region, area, species, comm_name, r_age_yr)

# Check names
freeR::check_names(stock_key$species)
complete(stock_key)

# Identify usable data
################################################################################

# Identify potentially usable stocks
colnames(timeseries_values_views)
stats <- timeseries_values_views %>% 
  group_by(stockid) %>% 
  # How many years of each data?
  summarize(ssb_n=sum(!is.na(SSB)),
            r_n=sum(!is.na(R)),
            tl_n=sum(!is.na(TL)), 
            tc_n=sum(!is.na(TC))) %>% 
  # What are the catch units?
  left_join(select(timeseries_units_views, stockid, TB, SSB, R, TL, TC), by="stockid") %>% 
  rename(tb_units=TB, ssb_units=SSB, r_units=R, tl_units=TL, tc_units=TC) %>% 
  # Which to use?
  mutate(catch_use=ifelse(tc_n>=tl_n, "TC", "TL"),
         catch_n=ifelse(tc_n>=tl_n, tc_n, tl_n),
         catch_units=ifelse(tc_n>=tl_n, tc_units, tl_units)) %>% 
  # Remove ones with 20 yr of catch data
  filter(ssb_n >= 20 & r_n>=20)

# Build data - first pass
colnames(timeseries_values_views)
data_1st <- timeseries_values_views %>% 
  # Stocks of interest
  filter(stockid %in% stats$stockid) %>%
  # Columns of interest
  select(stockid, year, SSB, R, TB, TC, TL) %>% 
  rename(ssb=SSB, r=R, tb=TB, tc=TC, tl=TL) %>% 
  # Add catch to use
  left_join(select(stats, stockid, catch_use), by="stockid") %>% 
  # Create columns for catch and B/BMSY to use
  mutate(catch=ifelse(catch_use=="TC", tc, tl)) %>% 
  # Reduce data
  select(stockid, year, ssb, r, tb, catch) %>%
  filter(!is.na(ssb) & !is.na(r))

# 20 years of data after trimming?
# SSB in MT and R in E00?
check <- data_1st %>% 
  group_by(stockid) %>% 
  summarize(nyr=n()) %>% 
  left_join(select(stats, stockid, ssb_units, r_units), by="stockid") %>% 
  filter(nyr>=20) %>% 
  filter(ssb_units=="MT" & r_units=="E00")
  

# Build final data
################################################################################

# Near final data
###########################

# Data - second pass
data_2nd <- data_1st %>% 
  filter(stockid %in% check$stockid)

# Stock key
###########################

# Stock key - first pass
stocks1 <- data_2nd %>%
  group_by(stockid) %>% 
  summarize(nyr=n(),
            yr1=min(year),
            yr2=max(year)) %>% 
  # Add type/units 
  left_join(select(stats, stockid, ssb_units, r_units, tb_units, catch_units, catch_use), by="stockid") %>% 
  # Add stock info
  left_join(stock_key, by="stockid") %>% 
  # Rearrange columns
  select(stockid, stocklong:comm_name, nyr, yr1, yr2, ssb_units, r_units,  everything())

# Get taxa info
spp <- sort(unique(stocks1$species))
taxa1 <- freeR::taxa(spp)
taxa2 <- select(taxa1, sciname, type, class, order, family)

# Add taxa
stocks <- stocks1 %>% 
  left_join(taxa2, by=c("species"="sciname"))

# Any stocks missing taxa?
stocks$species[is.na(stocks$family)]
taxa_cols <- c("type", "class", "order", "family")
# Commented code is no longer necessary but saved as template for future
# stocks[stocks$species=="Loligo vulgaris reynaudii", taxa_cols] <- c("invert", "Cephalopoda", "Myopsina", "Loliginidae")

# Complete
freeR::complete(stocks)

# Final data
###########################

# Final data
data <- data_2nd %>% 
  filter(stockid %in% stocks$stockid)

# Inspect completeness
freeR::complete(data)
freeR::complete(stocks)


# Export data
################################################################################

# Export
save(data, stocks, file=file.path(datadir, "ramldb_stock_recruit_data.Rdata"))

