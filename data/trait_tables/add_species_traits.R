setwd("C:/Users/janin/OneDrive/Documents/Validation/Validation Analysis/Transcriber and Species Traits")

library(tidyverse)

spp <- read_csv("species_traits.csv")

lhreg <- read_csv("lhreg.csv")

spp$spp <- spp$ID1

all <- left_join(spp, lhreg, by = "spp")

all$np_combined <- paste0(all$np_tone, "_", all$np_pattern)

all <- all %>% select(ID1, scientific_name, common_name, np_tone, np_pattern, np_combined, spp_group, rarity, jmm_song_type, mass, logmass, MaxFreqkHz, habitat, Hab2, Hab3, Hab4)

write_csv(all, "species_traits_complete.csv")
