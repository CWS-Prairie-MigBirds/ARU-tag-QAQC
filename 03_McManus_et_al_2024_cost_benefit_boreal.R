##Bootstrap Boreal 2021 data and Model 7
## Create cost benefit figure

########BOREAL COST BENEFIT

# I want to take the validated dataset, and have a prediction for each tag. Did that above.

##Then I need the mean length of time it takes to validate a tag per species

## Graph: x-axis = # of tags validated, y-axis1 = # of errors fixed, y-axis2 validation time

##spp_n = species with > 5 tags validated

library(sampling)
library(tidyverse)
library(ggpubr)

j <- read_csv("output/mm7_predicted_agreement_boreal2021.csv")

## sample 10%, 15%, 20%, 25%, 30%, 40% and 50% of tags (in each sample, bootstrap each sample 100 times)
##Do it once with simple random sample, once with a prob of disagreement sample. 

set.seed(101)

j$pdisagree <- 1 - j$predicted

##add in species mean time to validate

timed_tags <- read_csv("data/time_individual_tag_validation_aug28_2023.csv") 

spp_time <- timed_tags %>% group_by(Species) %>% 
  dplyr::summarize(minutes_total = sum(`Time (minutes)`), tags_n = sum(`Number of tags`), minutes_per_tag = minutes_total/tags_n, changed_tags = sum(`Changed Spp tags`)) %>% 
  rename(species_code = Species)


j_time <- left_join(j, spp_time, by = "species_code")

## replace NAs with median time

med_time <- median(j_time$minutes_per_tag, na.rm = TRUE)

j_time <- j_time %>% replace_na(list(minutes_per_tag = med_time))

unique(j_time$minutes_per_tag)

##start bootstrap

n.simulations = 100

#10% weighted random sample
empty_list10_wt <- vector(mode = "list", length = n.simulations)


for (i in 1:length(empty_list10_wt)) {
  
  wt_s <- sample_n(j_time, size = nrow(j)*0.1, replace = FALSE, weight = pdisagree) 
  empty_list10_wt[[i]] <- wt_s %>% count(agreement) %>% 
    dplyr::filter(agreement == 0) %>% 
    mutate(val_time = sum(wt_s$minutes_per_tag), type = "weighted", sample = nrow(wt_s))
}

ten_wt <- do.call(rbind.data.frame, empty_list10_wt)

#10% simple random sample
empty_list10_s <- vector(mode = "list", length = n.simulations)


for (i in 1:length(empty_list10_s)) {
  
  wt_s <- sample_n(j_time, size = nrow(j)*0.1, replace = FALSE) 
  empty_list10_s[[i]] <- wt_s %>% count(agreement) %>% 
    dplyr::filter(agreement == 0) %>% 
    mutate(val_time = sum(wt_s$minutes_per_tag), type = "simple", sample = nrow(wt_s))
}

ten_s <- do.call(rbind.data.frame, empty_list10_s)

###################
#20% weighted random sample
empty_list20_wt <- vector(mode = "list", length = n.simulations)


for (i in 1:length(empty_list20_wt)) {
  
  wt_s <- sample_n(j_time, size = nrow(j)*0.2, replace = FALSE, weight = pdisagree) 
  empty_list20_wt[[i]] <- wt_s %>% count(agreement) %>% 
    dplyr::filter(agreement == 0) %>% 
    mutate(val_time = sum(wt_s$minutes_per_tag), type = "weighted", sample = nrow(wt_s))
}

twenty_wt <- do.call(rbind.data.frame, empty_list20_wt)

#20% simple random sample
empty_list20_s <- vector(mode = "list", length = n.simulations)


for (i in 1:length(empty_list20_s)) {
  
  wt_s <- sample_n(j_time, size = nrow(j)*0.2, replace = FALSE) 
  empty_list20_s[[i]] <- wt_s %>% count(agreement) %>% 
    dplyr::filter(agreement == 0) %>% 
    mutate(val_time = sum(wt_s$minutes_per_tag), type = "simple", sample = nrow(wt_s))
}

twenty_s <- do.call(rbind.data.frame, empty_list20_s)

#30% weighted random sample
empty_list30_wt <- vector(mode = "list", length = n.simulations)


for (i in 1:length(empty_list30_wt)) {
  
  wt_s <- sample_n(j_time, size = nrow(j)*0.3, replace = FALSE, weight = pdisagree) 
  empty_list30_wt[[i]] <- wt_s %>% count(agreement) %>% 
    dplyr::filter(agreement == 0) %>% 
    mutate(val_time = sum(wt_s$minutes_per_tag), type = "weighted", sample = nrow(wt_s))
}

thirty_wt <- do.call(rbind.data.frame, empty_list30_wt)

#30% simple random sample
empty_list30_s <- vector(mode = "list", length = n.simulations)


for (i in 1:length(empty_list30_s)) {
  
  wt_s <- sample_n(j_time, size = nrow(j)*0.3, replace = FALSE) 
  empty_list30_s[[i]] <- wt_s %>% count(agreement) %>% 
    dplyr::filter(agreement == 0) %>% 
    mutate(val_time = sum(wt_s$minutes_per_tag), type = "simple", sample = nrow(wt_s))
}

thirty_s <- do.call(rbind.data.frame, empty_list30_s)

#40% weighted random sample
empty_list40_wt <- vector(mode = "list", length = n.simulations)


for (i in 1:length(empty_list40_wt)) {
  
  wt_s <- sample_n(j_time, size = nrow(j)*0.4, replace = FALSE, weight = pdisagree) 
  
  empty_list40_wt[[i]] <- wt_s %>% count(agreement) %>% 
    dplyr::filter(agreement == 0) %>% 
    mutate(val_time = sum(wt_s$minutes_per_tag), type = "weighted", sample = nrow(wt_s))
}

forty_wt <- do.call(rbind.data.frame, empty_list40_wt)

#40% simple random sample
empty_list40_s <- vector(mode = "list", length = n.simulations)


for (i in 1:length(empty_list40_s)) {
  
  wt_s <- sample_n(j_time, size = nrow(j)*0.4, replace = FALSE) 
  
  empty_list40_s[[i]] <- wt_s %>% count(agreement) %>% 
    dplyr::filter(agreement == 0) %>%
    mutate(val_time = sum(wt_s$minutes_per_tag), type = "simple", sample = nrow(wt_s))
  
}

forty_s <- do.call(rbind.data.frame, empty_list40_s)

#50% weighted random sample
empty_list50_wt <- vector(mode = "list", length = n.simulations)


for (i in 1:length(empty_list50_wt)) {
  
  wt_s <- sample_n(j_time, size = nrow(j)*0.5, replace = FALSE, weight = pdisagree) 
  
  empty_list50_wt[[i]] <- wt_s %>% count(agreement) %>% 
    dplyr::filter(agreement == 0) %>% 
    mutate(val_time = sum(wt_s$minutes_per_tag), type = "weighted", sample = nrow(wt_s))
}

fifty_wt <- do.call(rbind.data.frame, empty_list50_wt)

#50% simple random sample
empty_list50_s <- vector(mode = "list", length = n.simulations)


for (i in 1:length(empty_list50_s)) {
  
  wt_s <- sample_n(j_time, size = nrow(j)*0.5, replace = FALSE) 
  empty_list50_s[[i]] <- wt_s %>% count(agreement) %>% 
    dplyr::filter(agreement == 0) %>% 
    mutate(val_time = sum(wt_s$minutes_per_tag), type = "simple", sample = nrow(wt_s))
}

fifty_s <- do.call(rbind.data.frame, empty_list50_s)



#60% weighted random sample
empty_list60_wt <- vector(mode = "list", length = n.simulations)


for (i in 1:length(empty_list60_wt)) {
  
  wt_s <- sample_n(j_time, size = nrow(j)*0.6, replace = FALSE, weight = pdisagree) 
  
  empty_list60_wt[[i]] <- wt_s %>% count(agreement) %>% 
    dplyr::filter(agreement == 0) %>% 
    mutate(val_time = sum(wt_s$minutes_per_tag), type = "weighted", sample = nrow(wt_s))
}

sixty_wt <- do.call(rbind.data.frame, empty_list60_wt)

#60% simple random sample
empty_list60_s <- vector(mode = "list", length = n.simulations)


for (i in 1:length(empty_list60_s)) {
  
  wt_s <- sample_n(j_time, size = nrow(j)*0.6, replace = FALSE) 
  empty_list50_s[[i]] <- wt_s %>% count(agreement) %>% 
    dplyr::filter(agreement == 0) %>% 
    mutate(val_time = sum(wt_s$minutes_per_tag), type = "simple", sample = nrow(wt_s))
}

sixty_s <- do.call(rbind.data.frame, empty_list50_s)


#70% weighted random sample
empty_list70_wt <- vector(mode = "list", length = n.simulations)


for (i in 1:length(empty_list70_wt)) {
  
  wt_s <- sample_n(j_time, size = nrow(j)*0.7, replace = FALSE, weight = pdisagree) 
  
  empty_list70_wt[[i]] <- wt_s %>% count(agreement) %>% 
    dplyr::filter(agreement == 0) %>% 
    mutate(val_time = sum(wt_s$minutes_per_tag), type = "weighted", sample = nrow(wt_s))
}

seventy_wt <- do.call(rbind.data.frame, empty_list70_wt)

#70% simple random sample
empty_list70_s <- vector(mode = "list", length = n.simulations)


for (i in 1:length(empty_list70_s)) {
  
  wt_s <- sample_n(j_time, size = nrow(j)*0.7, replace = FALSE) 
  empty_list50_s[[i]] <- wt_s %>% count(agreement) %>% 
    dplyr::filter(agreement == 0) %>% 
    mutate(val_time = sum(wt_s$minutes_per_tag), type = "simple", sample = nrow(wt_s))
}

seventy_s <- do.call(rbind.data.frame, empty_list70_s)


#80% weighted random sample
empty_list80_wt <- vector(mode = "list", length = n.simulations)


for (i in 1:length(empty_list80_wt)) {
  
  wt_s <- sample_n(j_time, size = nrow(j)*0.8, replace = FALSE, weight = pdisagree) 
  
  empty_list80_wt[[i]] <- wt_s %>% count(agreement) %>% 
    dplyr::filter(agreement == 0) %>% 
    mutate(val_time = sum(wt_s$minutes_per_tag), type = "weighted", sample = nrow(wt_s))
}

eighty_wt <- do.call(rbind.data.frame, empty_list80_wt)

#80% simple random sample
empty_list80_s <- vector(mode = "list", length = n.simulations)


for (i in 1:length(empty_list80_s)) {
  
  wt_s <- sample_n(j_time, size = nrow(j)*0.8, replace = FALSE) 
  empty_list80_s[[i]] <- wt_s %>% count(agreement) %>% 
    dplyr::filter(agreement == 0) %>% 
    mutate(val_time = sum(wt_s$minutes_per_tag), type = "simple", sample = nrow(wt_s))
}

eighty_s <- do.call(rbind.data.frame, empty_list80_s)


#90% weighted random sample
empty_list90_wt <- vector(mode = "list", length = n.simulations)


for (i in 1:length(empty_list90_wt)) {
  
  wt_s <- sample_n(j_time, size = nrow(j)*0.9, replace = FALSE, weight = pdisagree) 
  
  empty_list90_wt[[i]] <- wt_s %>% count(agreement) %>% 
    dplyr::filter(agreement == 0) %>% 
    mutate(val_time = sum(wt_s$minutes_per_tag), type = "weighted", sample = nrow(wt_s))
}

ninety_wt <- do.call(rbind.data.frame, empty_list90_wt)

#90% simple random sample
empty_list90_s <- vector(mode = "list", length = n.simulations)


for (i in 1:length(empty_list90_s)) {
  
  wt_s <- sample_n(j_time, size = nrow(j)*0.9, replace = FALSE) 
  empty_list50_s[[i]] <- wt_s %>% count(agreement) %>% 
    dplyr::filter(agreement == 0) %>% 
    mutate(val_time = sum(wt_s$minutes_per_tag), type = "simple", sample = nrow(wt_s))
}

ninety_s <- do.call(rbind.data.frame, empty_list90_s)


cb_data <- rbind(ten_wt, ten_s, twenty_wt, 
                 twenty_s, thirty_wt, thirty_s, forty_wt, forty_s, fifty_wt,
                 fifty_s, sixty_wt, sixty_s, seventy_wt, seventy_s, eighty_wt, 
                 eighty_s, ninety_wt, ninety_s)



##############################################################################################################
#############################################################################################################

##how many errors?
j_time %>% count(agreement) #there are 499 errors in full dataset


cb_data$errors_remain <- 100 - cb_data$n/499 * 100

cb_data$perc_validated <- cb_data$sample/nrow(j_time)*100

sum(j_time$minutes_per_tag)


dtw <-data.frame(0, 499, 2195.918, "weighted", 4484, 0, 100)
names(dtw)<-c("agreement","n", "val_time", "type", "sample", "errors_remain", "perc_validated")

dts <-data.frame(0, 499, 2195.918, "simple", 4484, 0, 100)
names(dts)<-c("agreement","n", "val_time", "type", "sample", "errors_remain", "perc_validated")

dtw2 <-data.frame(0, 499, 0, "weighted", 4484, 100, 0)
names(dtw2)<-c("agreement","n", "val_time", "type", "sample", "errors_remain", "perc_validated")

dts2 <-data.frame(0, 499, 0, "simple", 4484, 100, 0)
names(dts2)<-c("agreement","n", "val_time", "type", "sample", "errors_remain", "perc_validated")

newcb <- rbind(cb_data, dtw, dts, dtw2, dts2)

#########
#THREE PANEL FIGURE FOR MANUSCRIPT

p1 <- ggplot(newcb, aes(x = perc_validated, y = 100- errors_remain, color = type)) + 
  geom_smooth(show.legend = FALSE) + scale_color_manual(values=c("#440154FF", "#238A8DFF")) + 
  labs(x = "% Tags Verified", y = "% Errors Detected") +
  theme_bw()

p1
p2 <- ggplot(newcb, aes(x = perc_validated, y = val_time, color = type)) + 
  geom_smooth(show.legend = FALSE) + scale_color_manual(values=c("#69b3a2", rgb(0.2, 0.6, 0.9, 1))) + labs(x = "% Validated", y = "Validation Time (minutes)") +
  theme_classic2()

p3 <- ggplot(newcb, aes(x = 100-errors_remain, y = val_time, color = type)) + 
  geom_smooth(show.legend = FALSE) + scale_color_manual(values=c("#69b3a2", rgb(0.2, 0.6, 0.9, 1))) + labs(x = "% Errors Detected", y = "Validation Time (minutes)") +
  theme_classic2()

ggarrange(p1, p2 + font("x.text", size = 10), 
          ncol = 2, nrow = 1, labels = c("A", "B"))

##########
#PARETO PLOT OPTION

temperatureColor <- "#69b3a2"
priceColor <- rgb(0.2, 0.6, 0.9, 1)

ggplot(newcb, aes(x=perc_validated)) +
  
  geom_smooth( aes(y= val_time), size=1, color=temperatureColor) + 
  geom_smooth( aes(y= errors_remain * 20), size=1, color=priceColor) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Validation Time (minutes)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./20, name="% Errors Remaining")
  ) + 
  
  theme_bw() +
  
  theme(
    axis.title.y = element_text(color = temperatureColor, size=10),
    axis.title.y.right = element_text(color = priceColor, size=10)
  ) +
  labs(x = "% Validated")


ggplot(newcb, aes(x=perc_validated, color = type)) +
  
  geom_smooth( aes(y= val_time), size=1) + 
  geom_smooth( aes(y= errors_remain * 20), size=1) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Validation Time (minutes)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./20, name="% Errors Remaining")
  ) + 
  
  theme_bw() +
  
  labs(x = "% Validated")

### estimate time from higest error spp only to including all species
##ned a row for each scenario: row1 = highest predicted error species, row2= row1 + 2nd species etc.
## I need a table with a row for each species, the error rate, time per tag

test <- j_time %>% mutate(error = 1 - agreement) %>% 
  group_by(species_code, minutes_per_tag) %>% 
  dplyr::summarise(n_tags = n(), n_err = sum(error), mean = mean(pdisagree))

write_csv(test, "output/test_time_est.csv")

#### Now need to sample at different bootstrap iterations of the boreal 2021 data for whole recordings
## do it at each percent with simple random and sum # of tags and # of errors

##Need the file that has model pdisagree, group by pkey (location, date. time, observer) and su, # of tags and mean pdisagree

rec <- j_time %>% 
  group_by(location, recording_date, recording_time, transcriber) %>% 
  dplyr::summarise(n_tags = n(), n_err = n_tags - sum(agreement), time = sum(minutes_per_tag), mean_pdis = mean(pdisagree)) %>% ungroup()


summary(rec)

ggplot(rec, aes(x = time, y = mean_pdis)) + geom_point()

ggplot(rec, aes(x = n_err, y = mean_pdis)) + geom_point()

ggplot(rec, aes(x = n_err, y = n_tags)) + geom_point()


##then bootstrap different numbers of recordings by weight the sample with the mean pdisagree for the recording

##start bootstrap

n.simulations = 100


#10% weighted random sample
empty_list10_wt <- vector(mode = "list", length = n.simulations)


for (i in 1:length(empty_list10_wt)) {
  
  wt_s <- sample_n(rec, size = nrow(rec)*0.1, replace = FALSE, weight = mean_pdis) 
  empty_list10_wt[[i]] <- data.frame(errors = sum(wt_s$n_err), tot_time = sum(wt_s$time), type = "weighted", sample = nrow(wt_s))
}

ten_wt <- do.call(rbind.data.frame, empty_list10_wt)

#10% simple random sample
empty_list10_s <- vector(mode = "list", length = n.simulations)


for (i in 1:length(empty_list10_s)) {
  
  wt_s <- sample_n(rec, size = nrow(rec)*0.1, replace = FALSE) 
  empty_list10_s[[i]] <- data.frame(errors = sum(wt_s$n_err), tot_time = sum(wt_s$time), type = "simple", sample = nrow(wt_s))

}

ten_s <- do.call(rbind.data.frame, empty_list10_s)

###################
#20% weighted random sample
empty_list20_wt <- vector(mode = "list", length = n.simulations)


for (i in 1:length(empty_list20_wt)) {
  
  wt_s <- sample_n(rec, size = nrow(rec)*0.2, replace = FALSE, weight = mean_pdis) 
  empty_list20_wt[[i]] <- data.frame(errors = sum(wt_s$n_err), tot_time = sum(wt_s$time), type = "weighted", sample = nrow(wt_s))

}

twenty_wt <- do.call(rbind.data.frame, empty_list20_wt)

#20% simple random sample
empty_list20_s <- vector(mode = "list", length = n.simulations)


for (i in 1:length(empty_list20_s)) {
  
  wt_s <- sample_n(rec, size = nrow(rec)*0.2, replace = FALSE) 
  empty_list20_s[[i]] <-data.frame(errors = sum(wt_s$n_err), tot_time = sum(wt_s$time), type = "simple", sample = nrow(wt_s))

}

twenty_s <- do.call(rbind.data.frame, empty_list20_s)

#30% weighted random sample
empty_list30_wt <- vector(mode = "list", length = n.simulations)


for (i in 1:length(empty_list30_wt)) {
  
  wt_s <- sample_n(rec, size = nrow(rec)*0.3, replace = FALSE, weight = mean_pdis) 
  empty_list30_wt[[i]] <- data.frame(errors = sum(wt_s$n_err), tot_time = sum(wt_s$time), type = "weighted", sample = nrow(wt_s))

}

thirty_wt <- do.call(rbind.data.frame, empty_list30_wt)

#30% simple random sample
empty_list30_s <- vector(mode = "list", length = n.simulations)


for (i in 1:length(empty_list30_s)) {
  
  wt_s <- sample_n(rec, size = nrow(rec)*0.3, replace = FALSE) 
  empty_list30_s[[i]] <- data.frame(errors = sum(wt_s$n_err), tot_time = sum(wt_s$time), type = "simple", sample = nrow(wt_s))

}

thirty_s <- do.call(rbind.data.frame, empty_list30_s)

#40% weighted random sample
empty_list40_wt <- vector(mode = "list", length = n.simulations)


for (i in 1:length(empty_list40_wt)) {
  
  wt_s <- sample_n(rec, size = nrow(rec)*0.4, replace = FALSE, weight = mean_pdis) 
  
  empty_list40_wt[[i]] <- data.frame(errors = sum(wt_s$n_err), tot_time = sum(wt_s$time), type = "weighted", sample = nrow(wt_s))

}

forty_wt <- do.call(rbind.data.frame, empty_list40_wt)

#40% simple random sample
empty_list40_s <- vector(mode = "list", length = n.simulations)


for (i in 1:length(empty_list40_s)) {
  
  wt_s <- sample_n(rec, size = nrow(rec)*0.4, replace = FALSE) 
  
  empty_list40_s[[i]] <- data.frame(errors = sum(wt_s$n_err), tot_time = sum(wt_s$time), type = "simple", sample = nrow(wt_s))

  
}

forty_s <- do.call(rbind.data.frame, empty_list40_s)

#50% weighted random sample
empty_list50_wt <- vector(mode = "list", length = n.simulations)


for (i in 1:length(empty_list50_wt)) {
  
  wt_s <- sample_n(rec, size = nrow(rec)*0.5, replace = FALSE, weight = mean_pdis) 
  
  empty_list50_wt[[i]] <- data.frame(errors = sum(wt_s$n_err), tot_time = sum(wt_s$time), type = "weighted", sample = nrow(wt_s))

}

fifty_wt <- do.call(rbind.data.frame, empty_list50_wt)

#50% simple random sample
empty_list50_s <- vector(mode = "list", length = n.simulations)


for (i in 1:length(empty_list50_s)) {
  
  wt_s <- sample_n(rec, size = nrow(rec)*0.5, replace = FALSE) 
  empty_list50_s[[i]] <- data.frame(errors = sum(wt_s$n_err), tot_time = sum(wt_s$time), type = "simple", sample = nrow(wt_s))

}

fifty_s <- do.call(rbind.data.frame, empty_list50_s)

#60% weighted random sample
empty_list60_wt <- vector(mode = "list", length = n.simulations)


for (i in 1:length(empty_list60_wt)) {
  
  wt_s <- sample_n(rec, size = nrow(rec)*0.6, replace = FALSE, weight = mean_pdis) 
  
  empty_list60_wt[[i]] <- data.frame(errors = sum(wt_s$n_err), tot_time = sum(wt_s$time), type = "weighted", sample = nrow(wt_s))
  
}

sixty_wt <- do.call(rbind.data.frame, empty_list60_wt)

#60% simple random sample
empty_list60_s <- vector(mode = "list", length = n.simulations)


for (i in 1:length(empty_list60_s)) {
  
  wt_s <- sample_n(rec, size = nrow(rec)*0.6, replace = FALSE) 
  empty_list60_s[[i]] <- data.frame(errors = sum(wt_s$n_err), tot_time = sum(wt_s$time), type = "simple", sample = nrow(wt_s))
  
}

sixty_s <- do.call(rbind.data.frame, empty_list60_s)

#70% weighted random sample
empty_list70_wt <- vector(mode = "list", length = n.simulations)


for (i in 1:length(empty_list70_wt)) {
  
  wt_s <- sample_n(rec, size = nrow(rec)*0.7, replace = FALSE, weight = mean_pdis) 
  
  empty_list70_wt[[i]] <- data.frame(errors = sum(wt_s$n_err), tot_time = sum(wt_s$time), type = "weighted", sample = nrow(wt_s))
  
}

seventy_wt <- do.call(rbind.data.frame, empty_list70_wt)

#70% simple random sample
empty_list70_s <- vector(mode = "list", length = n.simulations)


for (i in 1:length(empty_list70_s)) {
  
  wt_s <- sample_n(rec, size = nrow(rec)*0.7, replace = FALSE) 
  empty_list70_s[[i]] <- data.frame(errors = sum(wt_s$n_err), tot_time = sum(wt_s$time), type = "simple", sample = nrow(wt_s))
  
}

seventy_s <- do.call(rbind.data.frame, empty_list70_s)

#80% weighted random sample
empty_list80_wt <- vector(mode = "list", length = n.simulations)


for (i in 1:length(empty_list80_wt)) {
  
  wt_s <- sample_n(rec, size = nrow(rec)*0.8, replace = FALSE, weight = mean_pdis) 
  
  empty_list80_wt[[i]] <- data.frame(errors = sum(wt_s$n_err), tot_time = sum(wt_s$time), type = "weighted", sample = nrow(wt_s))
  
}

eighty_wt <- do.call(rbind.data.frame, empty_list80_wt)

#80% simple random sample
empty_list80_s <- vector(mode = "list", length = n.simulations)


for (i in 1:length(empty_list80_s)) {
  
  wt_s <- sample_n(rec, size = nrow(rec)*0.8, replace = FALSE) 
  empty_list80_s[[i]] <- data.frame(errors = sum(wt_s$n_err), tot_time = sum(wt_s$time), type = "simple", sample = nrow(wt_s))
  
}

eighty_s <- do.call(rbind.data.frame, empty_list80_s)

#90% weighted random sample
empty_list90_wt <- vector(mode = "list", length = n.simulations)


for (i in 1:length(empty_list90_wt)) {
  
  wt_s <- sample_n(rec, size = nrow(rec)*0.9, replace = FALSE, weight = mean_pdis) 
  
  empty_list90_wt[[i]] <- data.frame(errors = sum(wt_s$n_err), tot_time = sum(wt_s$time), type = "weighted", sample = nrow(wt_s))
  
}

ninety_wt <- do.call(rbind.data.frame, empty_list90_wt)

#90% simple random sample
empty_list90_s <- vector(mode = "list", length = n.simulations)


for (i in 1:length(empty_list90_s)) {
  
  wt_s <- sample_n(rec, size = nrow(rec)*0.9, replace = FALSE) 
  empty_list90_s[[i]] <- data.frame(errors = sum(wt_s$n_err), tot_time = sum(wt_s$time), type = "simple", sample = nrow(wt_s))
  
}

ninety_s <- do.call(rbind.data.frame, empty_list90_s)

cb_data_rec <- rbind(ten_wt, ten_s, twenty_wt, 
                     twenty_s, thirty_wt, thirty_s, forty_wt, forty_s, fifty_wt,
                     fifty_s, sixty_wt, sixty_s, seventy_wt, seventy_s, eighty_wt, 
                     eighty_s, ninety_wt, ninety_s)


cb_data_rec$errors_remain <- 100 - cb_data_rec$errors/499 * 100

cb_data_rec$perc_validated <- cb_data_rec$sample/nrow(rec)*100

nrow(rec)
#########
###(0, 498, 2196.567, "weighted", 4982, 0, 100)

dtw <-data.frame(499, 2195.918, "weighted", nrow(rec), 0, 100)
names(dtw)<-c("errors","tot_time", "type", "sample", "errors_remain", "perc_validated")

dts <-data.frame(499, 2195.918, "simple", nrow(rec), 0, 100)
names(dts)<-c("errors","tot_time", "type", "sample", "errors_remain", "perc_validated")

dtw2 <-data.frame(499, 0, "weighted", nrow(rec), 100, 0)
names(dtw2)<-c("errors","tot_time", "type", "sample", "errors_remain", "perc_validated")

dts2 <-data.frame(499, 0, "simple", nrow(rec), 100, 0)
names(dts2)<-c("errors","tot_time", "type", "sample", "errors_remain", "perc_validated")

newcb2 <- rbind(cb_data_rec, dtw, dts, dtw2, dts2)

####time estimate

cb_data_rec %>% group_by(type) %>% 
  dplyr::summarise(mean_time = mean(tot_time), sd = sd(tot_time))

cb_plot <- cb_data_rec %>% mutate(bin = as.factor(perc_validated))

diff_time <- cb_plot %>% group_by(type, bin) %>% dplyr::summarise(mean_time = mean(tot_time)) %>% 
  pivot_wider(names_from = type, values_from = mean_time) %>% mutate(diff_time = weighted - simple, perc_diff = (diff_time/simple)*100)

mean(diff_time$perc_diff)
sd(diff_time$perc_diff)

ggplot(cb_plot, aes(x = type, y = tot_time, fill = bin)) + geom_boxplot()

library(scales)
show_col(viridis_pal()(20))

p1_rec <- ggplot(newcb2, aes(x = perc_validated, y = 100- errors_remain, color = type)) + 
  geom_smooth(show.legend = TRUE) + coord_cartesian(xlim = c(0, 100), ylim = c(0,100)) + 
  scale_color_manual(values=c("#440154FF", "#238A8DFF")) + 
  labs(x = "% Recordings Verified", y = "% Errors Detected", color = "Scenario") +
  theme_bw() + theme(legend.position = c(0.8, 0.2))

p1_rec

p2 <- ggplot(newcb, aes(x = perc_validated, y = tot_time, color = type)) + 
  geom_smooth(show.legend = FALSE) + scale_color_manual(values=c("#69b3a2", rgb(0.2, 0.6, 0.9, 1))) + labs(x = "% Validated", y = "Validation Time (minutes)") +
  theme_classic()

p2


####effect size for recording sample

fifty <- newcb2 %>% filter(perc_validated > 40 & perc_validated < 50) %>%
  group_by(type) %>% dplyr::summarize(mean = mean(per_err_found), sd = sd(per_err_found))




####manuscript figure 5

fig5 <- ggarrange(p1, p1_rec + font("x.text", size = 10), 
          ncol = 2, nrow = 1, labels = c("A", "B"))

fig5

ggsave(filename = "figure5_tagvsrecording_costbenefit.tiff", plot =  fig5, path = "./figures/",
       height = 5, width = 7, dpi = 320)


#####make a time to validate for percent errors found for the three scenarios

s3 <- read_csv("time_by_cummulative_bad_spp.csv")

s3$scenario <- "scenario 3"


s3 <- s3 %>% select(scenario, time_est, per_err_found) %>% rename(time = time_est)

s3$perc_validated <- "unkn"

newcb$scenario <- "scenario 1"
newcb$per_err_found <- 100 - newcb$errors_remain

newcb_f <- newcb %>% filter(type == "weighted") %>% 
  select(scenario, val_time, per_err_found, perc_validated) %>% rename(time = val_time)

newcb2$scenario <- "scenario 2"
newcb2$per_err_found <- 100 - newcb2$errors_remain
newcb2_f <- newcb2  %>% filter(type == "weighted") %>% 
  select(scenario, tot_time, per_err_found, perc_validated)%>% rename(time = tot_time)

fig7_dat <- rbind(s3, newcb_f, newcb2_f)

write_csv(fig7_dat, "output/fig7_dat.csv")

###############decay curve option

fig7_dat$per_agree <- 100 - fig7_dat$per_err_found


library(viridis)

fig7 <- ggplot(fig7_dat, aes(x = time, y = per_err_found)) + 
  geom_smooth(aes(colour = scenario))  +
  scale_color_viridis(discrete = TRUE, name = "") + labs(x = "Verification Time (minutes)", y = "% Errors Found") +
  theme_bw() + theme(legend.position = c(0.8, 0.2))

fig7

ggsave(filename = "figure7_time_for_errors_found.tiff", plot =  fig7, path = "./figures/",
       height = 7, width = 7, dpi = 320)

fig7d <- ggplot(fig7_dat, aes(x = per_agree, y = time)) + 
  geom_smooth(aes(colour = scenario)) + geom_point(aes(colour = scenario)) +
  scale_color_viridis(discrete = TRUE, name = "") + labs(x = "AGree", y = "Time") +
  theme_classic() + theme(legend.position = c(0.8, 0.2))

fig7d

ggsave(filename = "figure7_time_for_errors_found_test.png", plot =  fig6, path = "./figures/",
       height = 7, width = 7, dpi = 320)


##### how long did it take to find 50% of the errors with the fig 7 dat?

fif_sc1 <- fig7_dat %>% filter(scenario == "scenario 1", per_err_found >50 & per_err_found < 51)

mean(fif_sc1$time)

fif_sc2 <- fig7_dat %>% filter(scenario == "scenario 2", per_err_found >50 & per_err_found < 51)

mean(fif_sc2$time)


fif_sc3 <- fig7_dat %>% filter(scenario == "scenario 3")

mean(fif_sc3$time)

##percent long = (difference in time)/(time of the first task) *100%

(888-765)/765*100



#### Validation time for species with >10% error rate

st1 <- j_time %>% group_by(species_code) %>% dplyr::summarize(n_tags = n(), minutes = sum(minutes_per_tag))

err_rate <- j_time %>% group_by(species_code) %>% count(agreement) %>% dplyr::filter(agreement == 0)

st2 <- left_join(st1, err_rate, by = "species_code")

st3 <- st2 %>% rename(number_errors = n) %>% replace_na(list(number_errors = 0)) %>% 
  mutate(error_rate = number_errors/n_tags * 100)

bad_spp <- st3 %>% dplyr::filter(error_rate > 10)

sum(bad_spp$minutes)
sum(bad_spp$n_tags)

palette = "jco"


.libPaths()
