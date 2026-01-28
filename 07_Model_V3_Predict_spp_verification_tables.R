# ---
# title: "Create Species Verification Tables" 
# ---

library(tidyverse)
library(dplyr)
library(wildrtrax)
library(remotes)
library(lubridate)
library(stringr)


# Authorize yourself!
# copy this to console and replace with your WildTrax username and password
Sys.setenv(WT_USERNAME = username, WT_PASSWORD = password)

wt_auth()


#Enter sensor and project name of interest and and pull project ID. Replace sensor and proj accordingly
sensor <- "ARU"
proj <- "Boreal Bird Monitoring Program - Prairie Region 2025 DUSK transcription"

my_project <- wt_get_projects(sensor = sensor) |>
  filter(project == proj) |>
  pull(project_id)


# Download the data from the projects you need to summarize: Error in x[[1]] : subscript out of bounds

detail <- wt_download_report(project_id = my_project, sensor_id = sensor, reports = c("tag"), weather_cols = FALSE) %>%
  rename(original_id = species_code)  

# Filter out abiotic sounds and domestic animals
detail <- detail %>%
  filter(!species_class %in% c("Abiotic")) %>%
  filter(!original_id %in% c("COWW", "DOGG", "CHIK"))

unique(detail$original_id)


###Model predictions by species
m12b<- readRDS("m12b.rds")
summary(m12b)


#predict to the data
#################################
#1. Calculate observer error rates from validated dataset used for model training
load("05_qaqc2.0_data_clean.Rdata")
dat <- combof
rm(combof)

# create observer summary by year for ALL data
obs_summary <- dat %>% group_by(observer) %>% 
  summarise(error_rate_emp = (1- mean(agreement)))

# see if there are any observers in the current project without error rate estimates from past data
unique(detail$observer[!(detail$observer %in% obs_summary$observer)])

# modify unsernames that have changed since training data was transcribed (# MODIFY SCRIPT TO DO THE NEXT TWO STEPS PROGRAMATICALLY. A MESSAGE WILL HAVE TO POP UP ASKING THE USER HOW TO RESOLVE MISSING NAMES
# OPTIONS ARE: 1) UPDATE NAMES THAT HAVE CHANGED SLIGHTLY SINCE TRAINING DATA OR 2)APPLY THE MEDIAN ERROR RATE IF NEW USER 
detail <- detail %>%
  mutate(observer = recode(observer,
                           "Andy Nguyen2" = "Andy Nguyen",
                           "Sean Jenniskens (BirdsCanada)" = "Sean Jenniskens",
                           "Enid Cumming" = "Enid"))

# check to make sure everything is fixed
unique(detail$observer[!(detail$observer %in% obs_summary$observer)])

# One observer, William Konze, has no data in the training dataset, so he must be new for 2025. Assign the median error rate
# obs_summary <- rbind(obs_summary, c("William Konze", as.numeric(median(obs_summary$error_rate_emp))))

# join error rates to prediction dataset
detail <- left_join(detail, obs_summary)

#look for NAs
any(is.na(detail$error_rate_emp))


#2. calculate rarity for species in project making predictions for
rarity_tags <- detail %>% group_by(original_id) %>% summarise(rarity_tags = n()/nrow(detail))
detail <- left_join(detail, rarity_tags)
any(is.na(detail$rarity_tags))


#3. predict prob of agreement with model 12b
# change variables to appropriate class
detail$original_id <- as.factor(detail$original_id)
detail$observer <- as.factor(detail$observer)
detail$error_rate_emp <- as.numeric(detail$error_rate_emp)

## agreement ~ error_rate + rarity_tags + (1|original_id) + (1|observer)
## predict to new data
pred <- predict(m12b, newdata = detail, type = "response", allow.new.levels = TRUE)
detail$predicted <- pred

# Calculate mean probability of for each species and add to tag data
spp <- detail %>% 
  group_by(original_id) %>% 
  summarise(mean = mean(predicted))
tags <- left_join(detail, spp)

# List out SAR or priority species from certain projects
#Priority species for Zack Moore's MSc...no longer needed
# z_spp <- c("LAZB", "SAVS", "VESP", "CCSP", "WEME", "BAIS", "BRSP", "BOBO", "GRSP", "BHCO", "YERA", "CONI", "FEHA", "BARS", "NESP")

#Count number of tags with and without Needs review for each species-vocalization type combo
d.stat <- tags %>% 
  group_by(original_id, vocalization, mean, needs_review) %>% 
  summarise(n_tags = n()) %>% 
  pivot_wider(names_from = needs_review, values_from = n_tags, values_fill = 0) %>%
  rename(Confident = "FALSE",NeedsReview = "TRUE")


#Now follow priority rules below to put species-vocalization type in priority order for validation
#1. SAR species (calls and songs) in ascending order of agreement prob (mean)
  # All Confident up to a max of 15
  # All needs review
#2. Species with <15 tags (including both calls and songs) in ascending order of agreement prob (excludin Unkowns, see next)
  #All Confident and needs review
#3. Unknown tags in ascending order of agreement prob
  #All Confident up to a max of 5
  #All Needs Review
#4. Songs of species not included in 1-3 in ascending order of agreement prob
  # All Confident up to a max of 15
  # All needs review
#5. Non-vocal tags (order of this could change depending on species. In SDMB, the only non-vocal is MODO, which I don't think should be prioritzed for validation)
#6. Calls of all species not included in 1-3 in ascending order of agreement prob
  # All Confident up to a max of 15
  # All needs review


#1.SAR/Priority species: use the list associated with project of interest
#Priority SAR for Grassland program
# sar <- c("FEHA", "BUOW", "GRSG", "LOSH", "CCLO", "SPPI", "TBLO", "MCLO", "LBCU", "SEOW", "BAIS", "GRSP", "BOBO")

#Priority SAR for Boreal program
sar <- c("CONI", "OSFL", "CAWA", "TRUS", "YERA", "EWPW", "RUBL", "LEBI", "EVGR", "LEYE", "GWWA", "HASP", "REKN", "SBDO", "MAGO", "HUGO", "STSA", "WRSA")

#Priority species for WHCR program
# sar <- c("CORA", "WHCR", "CONI", "OSFL", "CAWA", "TRUS", "YERA", "EWPW", "RUBL", "LEBI", "EVGR", "LEYE", "GWWA", "HASP", "REKN", "SBDO", "MAGO", "HUGO", "STSA", "WRSA")

sar <- d.stat %>% 
  filter(original_id %in% sar) %>%
  arrange(mean) %>%
  mutate(NuConfToVer = if_else(Confident > 15, 15, Confident),
         NuNeedsRevToVer = NeedsReview)

#For WHCR PROJECT ONLY, verify ALL CORA tags, not just 15
# sar <- sar %>%
#   mutate(NuConfToVer = if_else(original_id == "CORA", Confident, NuConfToVer))

#2.Species with <15 tags
n15spp <- tags %>%
  group_by(original_id) %>%
  summarise(n_tags = n()) %>%
  filter(n_tags <=15) %>%
  filter(!str_starts(original_id, "UN|UD")) %>% #remove unknowns
  filter(!original_id %in% sar$original_id)

n15 <- d.stat %>% 
  filter(original_id %in% n15spp$original_id) %>%
  filter(vocalization != "Non-vocal") %>%
  arrange(mean) %>%
  mutate(NuConfToVer = Confident,
         NuNeedsRevToVer = NeedsReview)

#3.Unknowns (need to be pulled out first so they aren't in the other categories)
unk <- d.stat %>%
  filter(str_starts(original_id, "UN|UD")) %>%
  arrange(mean) %>%
  mutate(NuConfToVer = if_else(Confident > 5, 5, Confident),
         NuNeedsRevToVer = NeedsReview)

#4.Remaining songs
song <- d.stat %>%
  filter(vocalization == "Song") %>%
  filter(!original_id %in% sar$original_id) %>%
  filter(!original_id %in% n15$original_id) %>%
  filter(!original_id %in% unk$original_id) %>%
  arrange(mean) %>%
  mutate(NuConfToVer = if_else(Confident > 15, 15, Confident),
         NuNeedsRevToVer = NeedsReview)

#5.Non-vocal tags
nv <- d.stat %>%
  filter(vocalization == "Non-vocal"| vocalization == "Nocturnal flight") %>%
  filter(!original_id %in% unk$original_id) %>%
  filter(!original_id %in% sar$original_id) %>%
  arrange(mean) %>%
  mutate(NuConfToVer = if_else(Confident > 15, 15, Confident),
         NuNeedsRevToVer = NeedsReview)

#6. Remaining calls
calls <- d.stat %>%
  filter(vocalization == "Call") %>%
  filter(!original_id %in% sar$original_id) %>%
  filter(!original_id %in% n15$original_id) %>%
  filter(!original_id %in% unk$original_id) %>%
  arrange(mean) %>%
  mutate(NuConfToVer = if_else(Confident > 15, 15, Confident),
         NuNeedsRevToVer = NeedsReview)


#Combine all and save
validate <- rbind(sar,n15,unk,song,nv,calls)

#validate should have same number of rows as d.stat. If it has more rows or less rows, identify which species-call type is duplicated or missing
if(nrow(validate) > nrow(d.stat)) {
  dup <- validate %>% group_by(original_id, vocalization) %>%
    filter(n() > 1)
}

if(nrow(validate) < nrow(d.stat)) {
  miss <- anti_join(d.stat, validate, by = c("original_id", "vocalization"))
}

write.csv(validate, "output/spp_verif_BBMP-PrairieRegion2025DUSK.csv", row.names = F)

#clear environment if you want to run for another project
rm(list=ls())



#########################
#sections of code from previous version
#########################
# spp_traits <- read_csv("./trait_tables/species_traits_aug2022.csv")

# join1 <- left_join(detail, spp_traits, by = "ID1")
# 
# t_traits <- read_csv("./trait_tables/grassland_transcriber_traits.csv")
# 
# ##need to fix transcribers
# unique(t_traits$transcriber)
# unique(join1$transcriber)
# 
# join1$transcriber <- gsub("Stéphane Menu", "Stephane Menu", x = join1$transcriber)
# join1$transcriber <- gsub("Steve Enid", "steve.enid", x = join1$transcriber)
# 
# j <- left_join(join1, t_traits, by = "transcriber")
# 
# j$song_type_np <- ifelse(j$vocalization == "Song", yes = j$np_combined, no = "Call")
# 
# j$song_type_c <- ifelse(j$song_type_np == "click_trill", "click_trill", ifelse(j$song_type_np == "polynoise_phrase", "polynoise_phrase", "Other"))
# 
# ## change *variables to factors 
# 
# j$song_type_c <- as.factor(j$song_type_c)
# j$ID1 <- as.factor(j$ID1)
# 
# 
# #review table and remove tags that don't need review
# 
# 
# ##filter out noise codes and seperate out unknowns
# unique(d.stat$original_id)
# 
# unkns <- d.stat %>% filter(species_code %in% c("UNSH", "UNSP", "UNBI", "UNBL",  "UNFR", "UNMA", "UNPA", "UNDU",  "UNFL", "UNGU", "UNKN", "UNMA", "UNTH", "UNTR", "UNWA", "UNWO" ))
# 
# print <- d.stat %>% filter(!species_code %in% c( "MOWI", "MOAI", "LIAI", "LIBA", "LIRA",  "HEWI", "LIWI", "MOTF", "HETF", "LITF", "LITN", "NONE", "MONO", "LINO", "MOBA"))
# 
# unique(print$species_code)
# 
# d.check <- d.stat %>% filter(species_code %in% d_check)
# 
# sum(d.check$CONFIDENT)
# 
# low.ss <- d.stat %>% filter(CONFIDENT < 16)
# 
# print <- rbind(d.check, low.ss)
# 
# write_csv(d.stat, "spp_verification_GBMP_Contract_2024.csv")
