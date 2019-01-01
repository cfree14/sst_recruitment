
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(rio)
library(plyr)
library(dplyr)
library(freeR)
library(ggplot2)
library(ggplus)

# Directories
datadir <- "data/ramsr_minto"

# Read data
# RAMSR data was shared by Olaf and is the dataset used in Foss Grant et al. 2016
lh_orig <- import(file.path(datadir, "allstandard_lh_merge_v2.xlsx"), which=1)
data_orig <- import(file.path(datadir, "allstandard_lh_merge_v2.xlsx"), which=2)


# Species key
################################################################################

# Species key
spp_key <- lh_orig %>% 
  select(SPECIES, SCI_NAME) %>% 
  rename(comm_name_orig=SPECIES, species=SCI_NAME) %>% 
  mutate(species=revalue(species, c("Alosa sapidisima"="Alosa sapidissima",
                                    "Argyrosomus argentatus"="Pennahia argentata", 
                                    "Clupea harengus harengus"="Clupea harengus",
                                    "Harpodon nehereus"="Harpadon nehereus",
                                    "Plecoglossus altivelis"="Plecoglossus altivelis altivelis",
                                    "Sprattus sprattus sprattus"="Sprattus sprattus",
                                    "Stizostedion vitreum"="Sander vitreus"))) %>% 
  mutate(comm_name=freeR::sentcase(gsub("\\.", " ", comm_name_orig)))

# Check scientific names
freeR::check_names(spp_key$species)

# Taxa key
taxa_key <- freeR::taxa(spp_key$species)


# Format data
################################################################################

# The following is roughly true:
# R_LIFE = R_RAW * SPR(F0) / 1000
# R_ANNUAL = R_RAW * SPR(F0) * (1 - exp(-M)) / 1000

# I still need to format the SSB units and figure out exactly how
# SSB got standardized and if the scalar in the R standarization is always 1/1000.

# Format data
data <- data_orig %>% 
  # Rename columns
  setNames(tolower(colnames(.))) %>% 
  rename(comm_name_orig=species, m=natmort, ssb_raw=ssbraw, r_raw=rraw, 
         ssb_units=unitssbraw, r_units=unitrecraw, 
         r_age_yr=agerec, ssb_sd=ssbstand, r_life=rlife, r_annual=rannual) %>% 
  # Add correct species names
  left_join(spp_key, by="comm_name_orig") %>% 
  # Format natural mortality 
  mutate(m=as.numeric(ifelse(m=="NA", NA, m))) %>% 
  # Format units columns
  mutate(ssb_units=gsub(" of fish", "", freeR::sentcase((ssb_units)))) %>% 
  # Format recruitment units         
  mutate(r_units=gsub(" of fish", "", freeR::sentcase((r_units))),
         r_units=revalue(r_units, c("Number"="Numbers",
                                    "Fish"="Numbers",
                                    "Ten thousands"="Tens of thousands",
                                    "Thousand tonnes"="Thousands of tonnes"))) %>% 
  # Rearrange columns
  select(stockid, species, comm_name, year, 
         sprf0, m, r_age_yr, 
         ssb_raw, r_raw, ssb_units, r_units, 
         ssb_sd, r_life, r_annual)

# Correct HADVIS recruitment age
data$r_age_yr[data$stockid=="HADVIS"] <- 1

# Inspect completeness
str(data)
freeR::complete(data)

# Build stocks key
stocks <- data %>% 
  group_by(stockid, comm_name, species, sprf0, m, r_age_yr, ssb_units, r_units) %>% 
  summarize(nyr=sum(!is.na(ssb_sd) & !is.na(r_annual))) %>% 
  left_join(select(taxa_key, order, family, sciname), by=c("species"="sciname")) %>% 
  select(stockid, order, family, species, comm_name, sprf0:nyr)


# Export and visualize data
################################################################################

# Export data
save(stocks, data, file=file.path(datadir, "ramsr_minto_data.Rdata"))

# Plot stock-recruit relationships
pdf(file.path(datadir, "RAMSR_plots.pdf"), width=8.5, height=11)
p <- ggplot(data, aes(x=ssb_raw, y=r_raw)) + 
  geom_point() + scale_x_continuous(limits = c(0, NA)) + 
  scale_y_continuous(limits = c(0, NA)) + xlab("SSB") + ylab("Recruits")
p <- facet_multiple(plot=p, facets="stockid", ncol=4, nrow=6, scales="free")
dev.off()

