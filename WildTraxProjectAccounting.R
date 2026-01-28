#Wildtrax project accounting
#Written by: Barry Robinson
#Jan 23, 2026

#This script is used to calculate total number of tasks and hours in WildTrax projects and verify number of tasks and hours transcribed.
#These calculations are for accounting purposes for the WildTrax transcription contract

#Load packages
library(tidyverse)
library(dplyr)
library(wildrtrax)

# Authorize yourself!
# copy this to console and replace with your WildTrax username and password
Sys.setenv(WT_USERNAME = username, WT_PASSWORD = password)

wt_auth()


#Enter sensor and project name (or list of project names) of interest and and pull project ID. Replace sensor and proj accordingly
sensor <- "ARU"
proj <- c("Boreal Bird Monitoring Program - Prairie Region MB peatland restoration NOCTURNAL",
          "Boreal Bird Monitoring Program - Prairie Region MB peatland restoration DUSK",
          "Boreal Bird Monitoring Program - Prairie Region MB peatland restoration DAWN",
          "Boreal Bird Monitoring Program - Localization 2025",
          "Boreal Bird Monitoring Program - Prairie Region 2025 NOCTURNAL transcription",
          "Boreal Bird Monitoring Program - Prairie Region 2025 DUSK transcription",
          "Boreal Bird Monitoring Program - Prairie Region 2025 DAWN transcription",
          "Boreal Bird Monitoring Program - Manitoba 2023 Nocturnal")

proj_info <- wt_get_projects(sensor = sensor) |>
  filter(project %in% proj)

proj_ids <- proj_info |>
  pull(project_id)

# Download the data from the projects you need to summarize: Error in x[[1]] : subscript out of bounds
recordings <- lapply(proj_ids, FUN = function(x) {
  return(wt_download_report(project_id = x, sensor_id = sensor, reports = c("recording")))
})

#Count number of recordings of each length and calculate total hours of recordings for each project
summary_rec <- lapply(recordings, FUN = function(x) {
  name = unique(x$project)
  tmp = x %>%
    mutate(length = round(task_duration/60)) %>%
    group_by(length) %>%
    summarise(task_count = n()) %>%
    mutate(hours = length*task_count/60,
           project = rep(name, time = n()))
  return(tmp)
})

tot_proj <- do.call(rbind, summary_rec) %>%
  group_by(project) %>%
  summarise(count = sum(task_count),
            hours = sum(hours)) %>%
  mutate(percent = hours/sum(hours),
         verif_hours = percent*10)

tot_recs <- do.call(rbind, summary_rec) %>%
  group_by(length) %>%
  summarise(count = sum(task_count),
            hours = sum(hours))

#Summarize number of tags and total hours transcribed to date for each project
summary_trans <- lapply(recordings, FUN = function(x) {
  name = unique(x$project)
  tmp = x %>%
    filter(task_is_complete == TRUE) %>%
    mutate(length = round(task_duration/60)) %>%
    group_by(length) %>%
    summarise(task_count = n()) %>%
    mutate(hours = length*task_count/60,
           project = rep(name, time = n())) %>% 
    group_by(project) %>%
    summarise(totalHrs = sum(hours))
  return(tmp)
})

transAll <- do.call(rbind, summary_trans)
write.csv(transAll, "output/borealTranscriptionSummary.csv", row.names = F)

#calculate total hours transcribed across all projects

sum(transAll$totalHrs)-(55*3/60)


