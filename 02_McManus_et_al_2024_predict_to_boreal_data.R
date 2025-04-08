## Test Validation Model on new data: Boreal and Grasslands

library(tidyverse)
library(ggpubr)

###########################################
##BOREAL FIRST
##read in data from 2021 boreal project in the spring
##this is data before any changes were made from validation

dat1 <- read_csv("data/ECCC_Boreal_Monitoring_Strategy_-_Saskatchewan_Breeding_Bird_Atlas_collaboration_2021_report.csv")

## I need to collapse voc columns

dat1 <- dat1 %>% dplyr::select(project, location, latitude, longitude, recording_date, 
                               recording_time, transcriber, species_code, species_individual_name, confidence,  
                               min0_voc, min1_voc, min2_voc, min3_voc, min4_voc,min5_voc, min6_voc, min7_voc, min8_voc, min9_voc) %>% 
  unite("song_call", min0_voc:min9_voc, sep = "", na.rm = TRUE, remove = TRUE)

raw_tags <- dat1 %>% group_by(species_code) %>% dplyr::summarise(n = n())
##read in data from the most recent download from wildtrax

dat2 <- read_csv("./data/CWS-PRA_Boreal_Monitoring_Strategy_-_Saskatchewan_Breeding_Bird_Atlas_collaboration_2021_tag_details_report.csv")

##need to fix all the location names in dat2

dat2$loc2 <- gsub("SKBMS:", "", dat2$location) 
dat2$loc3 <- gsub(":", "-", dat2$loc2) 

unique(dat1$location)
unique(dat2$loc3)  

##create a pkey for both that includes the species and species individual

dat1$pkey <- paste0(dat1$location,"-", dat1$recording_date," ", dat1$recording_time, "-", dat1$transcriber, dat1$species_code, dat1$species_individual_name)

dat2$pkey <- paste0(dat2$loc3,"-", dat2$recording_date,"-", dat2$observer, dat2$species_code, dat2$individual_appearance_order)


# filter the second newer data set to only pkeys that exist in the first.
# join back to the first dataset, the pkeys in dataset 1 that don't exist in dataset 2 
# are the ones that where the spp ID was changed or the tag was deleted.
# This is the hack until we get the audit data, we don't know what it was changed to but that's ok for now

p <- unique(dat1$pkey)

dat2_t <- dat2 %>% dplyr::filter(pkey %in% p)

dat2_t <- dat2_t %>% dplyr::select(pkey, species_code, individual_appearance_order, vocalization, abundance, tag_clip_status, verified_by) %>% 
  rename(species_code2 = species_code, individual2 = individual_appearance_order, voc2 = vocalization, abundance2 = abundance)


dat_full <- full_join(dat1, dat2_t, by = "pkey")

#filter unknowns and noise codes

noi <- c("HETF", "LIBA", "LITF", "MORA", "MOWI", "HEAI", "LIWI", "MOBA",
         "MOTF", "LIAI", "LINO", "LIRA", "HEWI", "LIDT", "MOAI", 
         "LITN","HEBA", "MOTN", "MODT", "HENO", "MONO", "HERA", "LITC" )

unkns <- c("UNWO", "UNPA", "UNWA", "UNTR", "UNKN", "UNYE", "UNSH", "UNTH", "UNBT", "UNBL", "UNQK", "UNVI",
           "UNFL","UNCV", "UNAM", "UNGU",  "UNFI", "UNMA", "UNSP", "UNFR", "UNBP", "UNOW", "UNDU", "UNTE", "UNSW", "NONE")

mam <- c("DOGG", "COYT", "LECH","WOLF", "ABBE", "BEAV", "COWW", "MOOS", "WTDE", "ELKK")


dat_bio <- dat_full %>% dplyr::filter(!species_code %in% noi) %>% dplyr::filter(!species_code %in% unkns) %>% dplyr::filter(!species_code %in% mam)


unique(dat_bio$species_code2)

#If species_code2 is NA, that means the original tag was changed. 

dat_bio$species_code2[is.na(dat_bio$species_code2)]<-"CHANGED"

## if species is Changed that means it was validated, if tag clip status = verified that also means a tag was validated

dat_bio$validated <- ifelse(dat_bio$species_code2 == "CHANGED", "Yes", dat_bio$tag_clip_status)

##any tags with validated = NA haven't been validated yet, so remove them

dat_val <- dat_bio %>% drop_na(validated) %>% dplyr::filter(!validated == "NEEDS_REVIEW")


unique(dat_val$species_code)

##as of Nov. 17 we have 4210 tags validated of 103 species
##as of March 25 we have 4992 tags validated of 107 species

##fix CAJA and GRAJ

###fix CAJA and GRAJ

dat_val$species_code <- gsub(pattern = "CAJA", replacement = "GRAJ", x = dat_val$species_code)
dat_val$species_code2 <- gsub(pattern = "CAJA", replacement = "GRAJ", x = dat_val$species_code2)

##make an agreement column between species_code and species_code2

dat_val$agreement <- ifelse(dat_val$species_code == dat_val$species_code2, 1, 0)

dat_val$ID1 <- dat_val$species_code

dat_val %>% count(agreement)

## mean agreement by observer

new_tabl1 <- dat_val %>% group_by(transcriber) %>% dplyr::summarise(mean_agreement = mean(agreement), n = n())

write_csv(new_tabl1, "tables/mean_agreement_by_transcriber_borealSK2021.csv")


## mean agreement by species

new_tabl2 <- dat_val %>% group_by(species_code) %>% dplyr::summarise(mean = mean(agreement), n = n())

write_csv(new_tabl2, "tables/appendixA2_step2.csv")

mean(new_tabl2$mean)

poor <- c("ATSP", "GBHE", "AMTO", "COME", "CAGU", "HERG", "MEGU", "CATE", "FRGU", "RBGU", "COTE", "FOTE", "MALL", "RNDU", "COGO")

new_spp <- new_tabl2 %>% dplyr::filter(n > 5) %>% dplyr::filter(!species_code %in% poor)

mean(new_spp$mean)
sd(new_spp$mean)


spp_n <- new_spp$species_code
spp_n

##filter out WCSP, single observer with a known issue of lagging data entry
# he thought he was clicking WTSP from drop down but WCSP was being selected.
# noticed after a few instances of this happening

dat_val <- dat_val %>% dplyr::filter(!ID1 == "WCSP")

## predict to the data

spp_traits <- read_csv("data/trait_tables/species_traits_aug2022.csv")

join1 <- left_join(dat_val, spp_traits, by = "ID1")

t_traits <- read_csv("data/trait_tables/boreal_transcriber_traits.csv")

j <- left_join(join1, t_traits, by = "transcriber")

j$song_type_np <- ifelse(j$song_call == "Song", yes = j$np_combined, no = "Call")

j$song_type_c <- ifelse(j$song_type_np == "click_trill", "click_trill", ifelse(j$song_type_np == "polynoise_phrase", "polynoise_phrase", "Other"))

## change *variables to factors 

j$song_type_c <- as.factor(j$song_type_c)
j$ID1 <- as.factor(j$ID1)

##calculate rarity for species in new dataset, use all tags in the first dataset?

rarity <- dat_bio %>% group_by(species_code) %>% dplyr::summarise(rarity_new = n()/length(dat_bio$project))

## join rarity to species in test data set

j <- left_join(j, rarity, by = "species_code")


##Look at a summary for anything odd

summary(j)


##fix confidence codes: change "check data" to TBC, and "Confirmed" to Confident. If an ID was
##made then "unknown far" should be TBC. 


j$confidence[j$confidence == 'Check Data'] <- 'To Be Checked'

j$confidence[j$confidence == 'Confirmed'] <- 'Confident'

j$confidence[j$confidence == 'Unknown far'] <- 'To Be Checked'

j$confidence <- as.factor(j$confidence)
unique(j$confidence)


## agreement ~ test_score + song_type2 + confidence + (1+rarity|ID1)
## predict to new data

new_dat <- j %>% select(-rarity) %>% rename(rarity = rarity_new)

new_dat$agreement <- as.factor(new_dat$agreement)

mm7 <- readRDS("mm7.rds")

summary(mm7)

pred <- predict(mm7, newdata = new_dat, type = "response", allow.new.levels = TRUE, re.form = ~(1|ID1))

j$predicted <- pred

write_csv(j, "output/mm7_predicted_agreement_boreal2021.csv")

j <- read_csv("output/mm7_predicted_agreement_boreal2021.csv")
##Want to compare just species based on model building inclusion criteria

j2 <- j %>% dplyr::filter(ID1 %in% spp_n) %>% dplyr::filter(!ID1 %in% poor)

plot_test <- j2 %>% group_by(ID1) %>% dplyr::summarise(mean_pred = mean(predicted), Empirical = mean(agreement), n = n())


##make graph of mean agreement empirical and mean pred agreement

fig4 <- ggplot(plot_test, aes(mean_pred, Empirical)) + geom_point() +  ylim(0,1) + xlim(0,1) +
  #geom_text(aes(label=ifelse(Empirical < 0.5, as.character(ID1),'')),hjust=0,vjust=0) +
  geom_abline(intercept = 0, slope = 1) + labs(y = "Empirical Agreement", x = "Predicted Agreement") + theme_bw()

fig4

ggsave(filename = "figure4_empiricalvspred_agreement.tiff", plot =  fig4, path = "figures/",
       height = 5, width = 7, dpi = 320)


ggplot(plot_test, aes(mean_pred, Empirical)) + geom_point() +  ylim(0,1) + xlim(0,1) +
  geom_text(aes(label=ifelse(Empirical < 0.51, as.character(ID1),'')),hjust=0,vjust=0) +
  geom_abline(intercept = 0, slope = 1) + labs(y = "Empirical Agreement", x = "Predicted Agreement") + theme_bw()

##correlation test

cor.test(plot_test$mean_pred, plot_test$Empirical, method=c("pearson", "kendall", "spearman"))


library(pROC)

par(pty="s")

roc1 <- roc(j2$agreement, j2$predicted)

roc1

ci.auc(roc1)

write_csv(j,"output/mm7_predicted_agreement_boreal2021.csv")


library(pROC)
citation("lme4")
