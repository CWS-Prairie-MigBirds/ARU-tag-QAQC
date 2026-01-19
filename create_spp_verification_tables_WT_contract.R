---
title: "Create Species Verification Tables" 
output: html_notebook
---


library(tidyverse)
library(dplyr)
library(wildrtrax)
library(remotes)
library(lubridate)



# Authorize yourself!


Sys.setenv(WT_USERNAME = username, WT_PASSWORD = password)

wt_auth()




# Get a list of projects to see the project_ids


my_projects <- wt_get_download_summary(sensor_id = 'ARU')



# Download the data from the projects you need to summarize: Error in x[[1]] : subscript out of bounds



#dat <- wt_download_report(project_id = 1967, sensor_id = "ARU", reports = c("main"), weather_cols = FALSE)


detail <- wt_download_report(project_id = 2913, sensor_id = "ARU", reports = c("tag"), weather_cols = FALSE)

detail2 <- detail %>% rename(ID1 = species_code, transcriber = observer)



# Filter out abiotic sounds



detail2 <- detail2 %>% filter(!ID1 %in% c( "MOWI", "MOAI", "LIAI", "LIBA", "LIRA",  "HEWI", "LIWI", "MOTF", "HETF", "LITF", "LITN", "NONE", "MONO", "LINO", "MOBA", "NOISE", "HENO", "HEAI", "HUMVOC", "HUMNON", "HERA", "HEBA", "GUN", "MORA", "LIWT", "HETN", "MOTN"))

unique(detail2$ID1)




###Model predictions by species



mm7 <- readRDS("mm7.rds")
summary(mm7)

## predict to the data

spp_traits <- read_csv("./trait_tables/species_traits_aug2022.csv")

join1 <- left_join(detail2, spp_traits, by = "ID1")

t_traits <- read_csv("./trait_tables/grassland_transcriber_traits.csv")

##need to fix transcribers
unique(t_traits$transcriber)
unique(join1$transcriber)

join1$transcriber <- gsub("Stéphane Menu", "Stephane Menu", x = join1$transcriber)
join1$transcriber <- gsub("Steve Enid", "steve.enid", x = join1$transcriber)

j <- left_join(join1, t_traits, by = "transcriber")

j$song_type_np <- ifelse(j$vocalization == "Song", yes = j$np_combined, no = "Call")

j$song_type_c <- ifelse(j$song_type_np == "click_trill", "click_trill", ifelse(j$song_type_np == "polynoise_phrase", "polynoise_phrase", "Other"))

## change *variables to factors 

j$song_type_c <- as.factor(j$song_type_c)
j$ID1 <- as.factor(j$ID1)

##calculate rarity for species in new dataset, use all tags in the first dataset?

rarity <- j %>% group_by(ID1) %>% dplyr::summarise(rarity = n()/9542)

j <- left_join(j, rarity, by = "ID1")

j <- j %>% select(-rarity.x) %>% rename(rarity = rarity.y)
  

##Look at a summary for anything odd

summary(j)


##fix confidence codes: change "check data" to TBC, and "Confirmed" to Confident. If an ID was
##made then "unknown far" should be TBC. 

unique(j$needs_review)

j$confidence <- ifelse(j$needs_review =="FALSE", "Confident", "To Be Checked")

j$confidence <- as.factor(j$confidence)
unique(j$confidence)

## agreement ~ test_score + song_type2 + confidence + (1+rarity|ID1)
## predict to new data

new_dat <- j %>% mutate(song_type_c = replace_na(song_type_c, "Other"))

pred <- predict(mm7, newdata = new_dat, type = "response", allow.new.levels = TRUE, re.form = ~(1|ID1))

new_dat$predicted <- pred



# Now, make the dataset



spp <- new_dat %>% group_by(ID1) %>% summarise(mean = mean(predicted))

tags <- left_join(j, spp, by = "ID1")




# List out SAR or priority species from certain projects


z_spp <- c("LAZB", "SAVS", "VESP", "CCSP", "WEME", "BAIS", "BRSP", "BOBO", "GRSP", "BHCO", "YERA", "CONI", "FEHA", "BARS", "NESP")

gbmp_sar <- c("FEHA", "BUOW", "GRSG", "LOSH", "CCLO", "SPPI", "TBLO", "MCLO", "LBCU", "SEOW", "BAIS", "GRSP", "BOBO")

bbmp_sar <- c("CONI", "OSFL", "CAWA", "TRUS", "YERA", "EWPW", "RUBL")

d.stat <- tags %>% group_by(ID1, vocalization, mean, needs_review) %>% summarise(n_tags = n()) %>% pivot_wider(names_from = needs_review, values_from = n_tags, values_fill = 0)

##filter out noise codes and seperate out unknowns
unique(d.stat$species_code)

unkns <- d.stat %>% filter(species_code %in% c("UNSH", "UNSP", "UNBI", "UNBL",  "UNFR", "UNMA", "UNPA", "UNDU",  "UNFL", "UNGU", "UNKN", "UNMA", "UNTH", "UNTR", "UNWA", "UNWO" ))

print <- d.stat %>% filter(!species_code %in% c( "MOWI", "MOAI", "LIAI", "LIBA", "LIRA",  "HEWI", "LIWI", "MOTF", "HETF", "LITF", "LITN", "NONE", "MONO", "LINO", "MOBA"))

unique(print$species_code)

d.check <- d.stat %>% filter(species_code %in% d_check)

sum(d.check$CONFIDENT)

low.ss <- d.stat %>% filter(CONFIDENT < 16)

print <- rbind(d.check, low.ss)

write_csv(d.stat, "spp_verification_GBMP_Contract_2024.csv")



