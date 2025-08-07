## Wrangle audit data for QAQC model update

#1. Load libraries

library(tidyverse)
library(wildrtrax)
library(data.table)

#2. Load 306 audit report from Michael, received March 2025
# use the copy version with some manual Comment manipulation Janine couldn't figure out in R

audit <- read_csv("data/wt_audit/306_report - Copy.csv")

#3. Create a year column

audit <- audit %>% mutate(year = year(recording_date))

#4. What are the options in ar_action column?
  # DELETE
  # UPDATE
  # VERIFY
  # UNVERIFY

unique(audit$audit_comment)

#5. Need to separate audit_comment by ":" to isolate "Species Changed to"

audit <- audit %>% separate_wider_delim(cols = audit_comment, delim = ":", 
                                        names = c("Comment", "join_name"),
                                        too_few = "align_start", too_many = "debug")


spp_change <- audit %>% filter(Comment %in% c("Species Changed to")) %>% 
  mutate(join_name = 
           ifelse(join_name == "", "none", join_name),
         join_name = word(join_name, 1, 2))

dim_change <- audit %>% filter(Comment == "Dimension Change.") %>% 
  mutate(pkey = paste0(task_id, source_code, ar_unique_name))

unique(audit$Comment)

#6. Need the wildtrax species table lookup to get alpha codes for changed species

# Login to WildTrax----

config <- "wt_login.R"
source(config)
wt_auth()

wt_spp <- wt_get_species()

#fix duplicate species that have old codes in wt table

wt_spp <- wt_spp %>% filter(!species_code %in% c("GRAJ", "CORBRA", "32", "PICHUD",
                                                 "CIRCYA", "183", "178"))

#if there is no species name, use common name

wt_spp$join_name <- ifelse(wt_spp$species_scientific_name == " ", wt_spp$species_common_name,
                           wt_spp$species_scientific_name)

# now join
wt_spp$join_name <- tolower(wt_spp$join_name)
spp_change$join_name <- tolower(spp_change$join_name)

spp_join <- left_join(
  spp_change,
  wt_spp,
  by = "join_name")

unique(spp_join$species_code)

spp_join <- spp_join %>% mutate(species_code = ifelse(is.na(join_name), "NONE",
                                                   ifelse(join_name == "aechmorphus occidentalis", "WEGR",
                                                      ifelse(join_name == "no species", "NONE",
                                                        ifelse(join_name == "leiothylpis peregrina", "TEWA",
                                                          ifelse(join_name == "leiothylpis celata", "OCWA",
                                                             ifelse(join_name == "heavy background", "HEBA",
                                                                ifelse(join_name == "rana pipiens", "NLFR",
                                                                  ifelse(join_name == "moderate background", "MOBA",
                                                                    ifelse(join_name == "lanus ludovicianus", "LOSH",
                                                                      ifelse(join_name == "caprimulgus vociferus", "EWWP",
                                                                        ifelse(join_name == " ", "NONE",     
                                                   species_code))))))))))))

# whyyyy are some species scientific names non-existent in the wtspecies table???
# ok all fixed now

missing <- spp_join %>% filter(is.na(species_code))

## can we add tag_id at this stage using the "Dimension Change."
# Is a species change always accompanied by a dimension change?

spp_join <- spp_join %>% mutate(pkey = paste0(task_id, source_code, ar_unique_name))

dc_s <- dim_change %>% select(pkey, tag_id_dc = tag_id) %>% distinct() %>% 
  filter(!tag_id_dc %in% c(3672715, 2649144, 2246578, 3648874, 2296698, 2296703, 2728437,
                        2988225, 3645592))

spp_join_dc <- left_join(spp_join, dc_s, by = "pkey")

# tidy up the data frame so we pick the few weird tag ids and
#keep project info, original species, changed species and observer

spp_join_dc <- spp_join_dc %>% select(project_id, location_name, recording_date, year, task_id, 
                                tag_id_dc, original_id = source_code, new_id = species_code,
                                created_by, changed_by = deleted_by, ar_unique_name)

# remove records where it was changed by the same person it was created by, but the
# fields in the audit data aren't reliable. I need to download projects and merge
# to get original transcriber

projects <- wt_get_download_summary(sensor_id = "ARU")
projs_306 <- read.csv("data/wt_audit/projects_in_306_report.csv")

wt_data = purrr::map(.x = unique(projs_306$new_project_id),
                     .f = ~wt_download_report(project_id = .x, sensor_id = "ARU", 
                                              weather_cols = F, reports = "main"))
                 
df_main <- rbindlist(wt_data, fill = TRUE)

saveRDS(df_main, "306_audit_project_data.rds")

wt_data_tag = purrr::map(.x = unique(projs_306$new_project_id),
                     .f = ~wt_download_report(project_id = .x, sensor_id = "ARU", 
                                              weather_cols = F, reports = "tag"))

df_tag <- rbindlist(wt_data_tag, fill = TRUE)

wt_data_bn = purrr::map(.x = unique(projs_306$new_project_id),
                         .f = ~wt_download_report(project_id = .x, sensor_id = "ARU", 
                                                  weather_cols = F, reports = "birdnet"))

df_bn <- rbindlist(wt_data_bn, fill = TRUE)

## how to join? I just need the right observer for the right task

obs <- df_main %>% select(task_id, observer) %>% distinct()


test <- left_join(spp_join_dc, obs, by = "task_id")

CHANGED <- test %>% mutate(same_observer = ifelse(observer == changed_by, "yes", "no")) %>% 
  filter(same_observer == "no")

CHANGED <- CHANGED %>% mutate(tag_duration = NA,
                              rms_peak_dbfs = NA,
                              vocalization = NA,
                              pkey = paste0(task_id, new_id, ar_unique_name)) %>% 
  select(project_id, location_name, recording_date, 
         year, task_id, tag_id = tag_id_dc, original_id,
         new_id, observer, changed_by, ar_unique_name, vocalization,
         tag_duration, rms_peak_dbfs, pkey)

# create verified 

VERIFIED <- audit %>% filter(ar_action == "VERIFY")

VER_data <- df %>% filter(tag_is_verified == TRUE) %>% mutate(new_id = species_code,
                                                              changed_by = NA,
                                                              year = year(recording_date_time),
                                                              pkey = paste0(task_id, species_code, individual_order)) %>% 
  select(project_id, location_name = location, recording_date = recording_date_time, 
         year, task_id, tag_id, original_id = species_code,
         new_id, observer, changed_by, ar_unique_name = individual_order, vocalization,
         tag_duration, rms_peak_dbfs, pkey)


# ok use data download, audit version seems to have something funky going on

final <- rbind(VER_data, CHANGED)


final$agreement <- ifelse(final$original_id == final$new_id, 1, 0)

mean(final$agreement)

# ok time to bind all the other covariates

# Covariates:

# rms_peak_dbfs
# mean_tag_freq
# max_tag_freq
# tag_duration
# needs_review
# birdnet confidence: going to need some work here

# main data first to get peak dbs and tag duration

join_main <- df_main %>% mutate(pkey = paste0(task_id, species_code, individual_order)) %>% 
  select(pkey, peak_db2 = rms_peak_dbfs, duration2 = tag_duration, tag_id2 = tag_id)

final2 <- left_join(final, join_main, by = "pkey")

# then tag data for detection_time, mean freq, max freq, and needs_review

join_tag <- df_tag %>% mutate(pkey = paste0(task_id, species_code, individual_order)) %>% 
  select(pkey, detection_time, max_tag_freq, needs_review)

final3 <- left_join(final2, join_tag, by = "pkey")


## split dataframe into where we have covariates and where we don't

good <- final3 %>% filter(!is.na(duration2)) %>% select(-tag_duration, -rms_peak_dbfs, -tag_id) %>% 
  rename(tag_id = tag_id2)

join_tag <- df_tag %>% mutate(pkey = paste0(task_id, species_code, individual_order)) %>% 
  select(tag_id, species_code, detection_time, max_tag_freq, needs_review)

join_main <- df_main %>% mutate(pkey = paste0(task_id, species_code, individual_order)) %>% 
  select(tag_id, peak_db2 = rms_peak_dbfs, duration2 = tag_duration) %>% drop_na()

bad <- final3 %>% filter(is.na(duration2)) %>% 
  select(project_id, location_name, recording_date, year, task_id, tag_id, 
         original_id, new_id, observer, changed_by, ar_unique_name ) %>% 
  left_join(join_tag, by = "tag_id") %>% left_join(join_main, by = "tag_id")


#split bad into NONE, good2, and remaining bad

good2 <- bad %>% filter(!is.na(detection_time)) %>% select(-new_id) %>% 
  rename(new_id = species_code) #use the species_code column for new id??

bad2 <- bad %>% filter(is.na(detection_time))

none <- bad2 %>% filter(new_id == "NONE") %>% select(-species_code)

remain_to_fix <- bad2 %>% filter(new_id != "NONE")

cols <- c("project_id" ,    "location_name",  "recording_date", "year" ,         
          "task_id",        "tag_id" ,        "original_id" ,   "new_id" ,       
          "observer",       "changed_by",     "ar_unique_name", "duration2",
          "peak_db2" ,     "detection_time",
          "max_tag_freq" ,  "needs_review")

good <- good %>% select(all_of(cols))
good2 <- good2 %>% select(all_of(cols)) 
none <- none %>% select(all_of(cols)) 

combo <- rbind(good, good2, none)

# recalculate agreement
# change all MYWA codes to YRWA
#No way to recover the tag covariates for deleted tags: could we take means to fill
# those NAs?

combo$original_id <- gsub("MYWA", "YRWA", combo$original_id)
combo$new_id <- gsub("MYWA", "YRWA", combo$new_id)
combo$max_tag_freq <- gsub("kHz", "", combo$max_tag_freq)

combo <- combo %>% mutate(agreement = ifelse(original_id == new_id, 1, 0),
                          max_tag_freq = as.numeric(max_tag_freq),
                          needs_review = ifelse(is.na(needs_review), "FALSE", needs_review),
                          across(where(is.numeric), ~replace_na(., replace = mean(., na.rm = TRUE))))

mean(combo$agreement)


# then bn data


# save file for use in next script

save(combo, file = "05_qaqc2.0_data_clean.Rdata")

# a couple summaries

year_summary <- final %>% group_by(year) %>% summarise(n_tags = n(), n_observers = length(unique(observer)),
                                                       mean_agree = mean(agreement))

spp_agree <- final %>% group_by(original_id) %>% 
  summarise(n_tags = n(), mean_agree = mean(agreement)) %>% 
  filter(n_tags > 15)

final %>% group_by(year, project_id) %>% summarise(n_tags = n())

write_csv(year_summary, "year_summary_audit306.csv")
