
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


# Build species key
################################################################################

# Build stock key
spp_key <- stock %>% 
  # Select columns
  select(scientificname, commonname) %>% 
  # Rename columns
  rename(species_orig=scientificname, comm_name=commonname) %>% 
  # Format columns
  mutate(comm_name=freeR::sentcase(comm_name),
         species=gsub("spp.", "spp", species_orig),
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
                                    "Sardinops melanostictus"="Sardinops sagax")),
         species_use=revalue(species, c("Loligo vulgaris reynaudii"="Loligo vulgaris",
                                        "Arius spp"="Arius latiscutatus", # Arius latiscutatus (dominant),  A. heudeloti
                                        "Cynoglossus spp"="Cynoglossus senegalensis", # Cynoglossus senegalensis (goreensis), C. canariensis, C. monodi.
                                        "Dentex spp"="Dentex macrophthalmus", # Dentex macrophthalmus (dominant), Dentex angolensis
                                        "Lepidorhombus spp"="Lepidorhombus whiffiagonis", # Lepidorhombus whiffiagonis (dominant), L. boscii
                                        "Penaeus spp"="Farfantepenaeus notialis", # Penaeus notialis, Parapeneus longirostris
                                        "Pomadasys spp"="Pomadasys jubelini", # Pomadasys jubelini (dominant), P. incisus, P. rogeri
                                        "Pseudotolithus spp"="Pseudotolithus elongatus",  # Pseudotolithus elongatus (dominant), P. typus, P. senegalensis, P. brachygnatus (P. senegallus)   
                                        "Sardinella spp"="Sardinella aurita", # Sardinella aurita (dominant), S. maderensis
                                        "Sebastes spp"="Sebastes mentella", # Sebastes mentella (dominant), S. fasciatus, S. marinus
                                        "Sepia spp"="Sepia officinalis"))) %>%  # Sepia officinalis (dominant), S. bertheloti and S. hierredda
  select(species, species_orig, species_use) %>% 
  unique()

# Check names
freeR::check_names(spp_key$species)
freeR::check_names(spp_key$species_use)
freeR::complete(spp_key)


# Taxanomic info and FishLife life history
################################################################################

# Taxa
taxa_key <- freeR::taxa(spp_key$species_use)

# FishLife life history
fl_data <- freeR::fishlife(taxa_key$sciname)


# Merge
################################################################################

# Merge data
data <- spp_key %>% 
  left_join(fl_data, by=c("species_use"="species"))

# Export data
################################################################################

# Export
write.csv(data, file=file.path(datadir, "ramldb_life_history_data.csv"), row.names=F)

