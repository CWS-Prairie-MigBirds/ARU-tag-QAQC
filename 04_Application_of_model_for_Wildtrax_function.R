## Create species verification tables using QAQC model
# Janine McManus
# Last updated: April 8, 2025


# Load libraries

library(tidyverse)
library(dplyr)
library(wildrtrax)
library(remotes)
library(lubridate)

# Authorize into WildTrax

Sys.setenv(WT_USERNAME = user, WT_PASSWORD = password)

wt_auth()

# Download list of projects. Optionally add filter if you know the name of project you want

my_project <- wt_get_projects("ARU") |>
  filter(project == "CWS-GBMP - Morning Recordings 2025") |>
  pull(project_id)

# Download the project that has tags you would like to verify

detail <- wt_download_report(project_id = my_project, sensor_id = "ARU", reports = c("tag"), weather_cols = FALSE)

# Filter out abiotic sounds
biotic <- wt_tidy_species(detail, remove = c("abiotic"), zerofill = FALSE) %>% 
  rename(ID1 = species_code, transcriber = observer)

#############################################################################
# Use QAQC model to predict probability of agreement by species

mm7 <- readRDS("mm7.rds")
summary(mm7)

## predict to the data

spp_traits <- read_csv("data/trait_tables/species_traits_aug2022.csv")

join1 <- left_join(biotic, spp_traits, by = "ID1")

t_traits <- read_csv("data/trait_tables/grassland_transcriber_traits.csv")

j <- left_join(join1, t_traits, by = "transcriber")

j$song_type_np <- ifelse(j$vocalization == "Song", yes = j$np_combined, no = "Call")

j$song_type_c <- ifelse(j$song_type_np == "click_trill", "click_trill", ifelse(j$song_type_np == "polynoise_phrase", "polynoise_phrase", "Other"))

## change *variables to factors 

j$song_type_c <- as.factor(j$song_type_c)
j$ID1 <- as.factor(j$ID1)

# should we remove unkowns prior to prediction?

j <- j %>% mutate(song_type_c = replace_na(song_type_c, "Other"))


##calculate rarity for species in new dataset

rarity <- j %>% group_by(ID1) %>% dplyr::summarise(rarity = n()/length(biotic$tag_id))

j <- left_join(j, rarity, by = "ID1")

j <- j %>% select(-rarity.x) %>% rename(rarity = rarity.y)


##Look at a summary for anything odd

summary(j)


##set confidence level to factor

unique(j$needs_review)

j$confidence <- ifelse(j$needs_review =="FALSE", "Confident", "To Be Checked")

j$confidence <- as.factor(j$confidence)
unique(j$confidence)

## agreement ~ test_score + song_type_c + confidence + (1+rarity|ID1)
## predict to the new data

pred <- predict(mm7, newdata = j, type = "response", allow.new.levels = TRUE, re.form = ~(1|ID1))

j$pagree <- pred
j$pdisagree <- 1- j$pagree

summary(j)

# Choose whether you would like to verify all SAR

SAR <- spp_traits %>% filter(SAR_prairies == "YES")


# Choose how much effort should go towards unknown tag verification
# for example only check "Needs REview" unkown tags



# Then take a random sample of n tags per species for all others weighted by the pdisagree


