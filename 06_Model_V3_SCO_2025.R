#### this script removes observers who have <=20 tags verified across whole dataset

library(tidyverse)
library(lme4)
library(ggpmisc)
library(AICcmodavg)
library(DHARMa)

## we want to get away from needing to include test score, can we predict agreement
# using data from 2017

load("05_qaqc2.0_data_clean.Rdata")

dat <- combof
rm(combof)


# species list to filter data by, remove unknowns and species with very few tags

spp_list <- dat %>% group_by(original_id) %>% summarise(n = n()) %>% 
  filter(n > 15, !original_id %in% c("UNWT", "UNFR", "UNGU", "UNYE", "UNKN", "UNMA", "UNPA",
                                    "UNQK", "UNSH", "UNSP", "UNTH", "UNAM", "UNTR", "UNVI", "UNBI",
                                    "UNBL", "UNWA", "UNWO", "UNDU", "LIBA", "UNFL", "UNWR"))

dat_spp <- dat %>% filter(original_id %in% spp_list$original_id)


# create observer summary by year for ALL data, identify observers who have 
# < 20 tags verified by a second observer

obs_summary <- dat_spp %>% group_by(observer) %>% summarise(n_verified = n(), mean_agree = mean(agreement))

obs_under_20 <- obs_summary %>% filter(n_verified <=20) %>% mutate(error_rate_emp = (1- mean_agree))


## split those out of data and save observer with less than 20 obs for test dataset

dat_filt <- dat_spp %>% filter(!observer %in% obs_under_20$observer)

dat_obs_under_20 <- dat_spp %>% filter(observer %in% obs_under_20$observer)


## data filter summary by observer

obs_summary_filt <- dat_filt %>% group_by(observer) %>% summarise(n_verified = n(), mean_agree = mean(agreement))


## Split dataset into validation and training
#use 70% of dataset as training set and 30% as test set

set.seed(101)

# Create a random split (70% training, 30% testing)
sample_size <- floor(0.7 * nrow(dat_filt))
model_indices <- sample(seq_len(nrow(dat_filt)), size = sample_size)
model_data <- dat_filt[model_indices, ]
empirical_data <- dat_filt[-model_indices, ]


# calculate observer error rate

empirical_summary <- empirical_data %>% group_by(observer) %>% summarise(n_tags_emp = n(), 
                                                                         n_spp_emp = length(unique(original_id)), 
                                                                         error_rate_emp = (1- mean(agreement)))


# split model data into test and train

set.seed(123)

# Create a random split (70% training, 30% testing)
sample_size <- floor(0.7 * nrow(model_data))
train_indices <- sample(seq_len(nrow(model_data)), size = sample_size, replace = FALSE)
train_data <- model_data[train_indices, ]
test_data <- model_data[-train_indices, ]

#summaries for train and test data

train_summary <- train_data %>% group_by(observer) %>% summarise(n_tags_train = n(), 
                                                                 n_spp_train = length(unique(original_id)), 
                                                                 error_rate_train = (1- mean(agreement)))

test_summary <- test_data %>% group_by(observer) %>% summarise(n_tags_test = n(), 
                                                               n_spp_test = length(unique(original_id)), 
                                                               error_rate_test = (1- mean(agreement))) 

# join into one data frame and save

s1 <- full_join(empirical_summary, train_summary, by = "observer")

obs_summary <- full_join(s1, test_summary, by = "observer")

obs_summary <- obs_summary %>% filter(observer != "Not Assigned")

write_csv(obs_summary, "observer_error_summary_obsandspp_filtered.csv")

ggplot(obs_summary, aes(x = error_rate_train, y = error_rate_test)) + geom_point()

ggplot(obs_summary, aes(x = error_rate_test, y = error_rate_emp)) + geom_point() +
  geom_smooth(method = "lm") + stat_poly_eq(aes(label = paste(stat(eq.label), stat(rr.label), sep = " * \", \" * ")), 
                                            formula = y ~ x, parse = TRUE)

# join error rate to main data

train_data <- left_join(train_data, empirical_summary, by = "observer")
test_data <- left_join(test_data, empirical_summary, by = "observer")

# in new data do I calculate error rate just from test data?
test_data2 <- left_join(test_data, test_summary, by = "observer")
test_data2$error_rate_emp <- test_data2$error_rate_test


## add under 20 tag observers into test data
test_data <- test_data  %>% 
  select(-n_tags_emp, -n_spp_emp) %>% mutate(new_level = "No")

dat_obs_under_20 <- dat_obs_under_20 %>% left_join(obs_under_20, by = "observer") %>% 
  select(-n_verified, -mean_agree) %>% mutate(new_level = "Yes")

test_data <- rbind(test_data, dat_obs_under_20)


## model building time

## what needs to be factors?

train_data$original_id <- as.factor(train_data$original_id)
train_data$needs_review <- as.factor(train_data$needs_review)


m1 <- glmer(agreement ~ scale(error_rate_emp) + scale(duration2) + scale(peak_db2) +
              scale(max_tag_freq) + scale(rarity_tags)  + (1|original_id), 
            data = train_data, family = binomial)

m2 <- glmer(agreement ~ scale(error_rate_emp) + scale(duration2) + scale(peak_db2) +
              scale(max_tag_freq) + scale(rarity_loc) + (1|original_id), 
            data = train_data, family = binomial)

m3 <- glmer(agreement ~ scale(error_rate_emp) + scale(peak_db2) +
              scale(max_tag_freq) + scale(rarity_loc) + (1|original_id), 
            data = train_data, family = binomial)

m4 <- glmer(agreement ~ scale(error_rate_emp) + scale(duration2)  +
              scale(max_tag_freq) + scale(rarity_loc)  + (1|original_id), 
            data = train_data, family = binomial)

m5 <- glmer(agreement ~ scale(error_rate_emp)  +
              scale(max_tag_freq) + scale(rarity_loc) + (1|original_id), 
            data = train_data, family = binomial)

m6 <- glmer(agreement ~ scale(error_rate_emp) + scale(duration2) + scale(peak_db2) + (1|original_id), 
            data = train_data, family = binomial)

m7 <- glmer(agreement ~ scale(error_rate_emp) + scale(duration2) +
              scale(max_tag_freq)  + (1|original_id), 
            data = train_data, family = binomial)

m8 <- glmer(agreement ~ scale(error_rate_emp) + scale(duration2) + (1|original_id), 
            data = train_data, family = binomial)

m9 <- glmer(agreement ~ scale(error_rate_emp) + scale(duration2) + scale(rarity_tags) + (1|original_id), 
            data = train_data, family = binomial)

m10 <- glmer(agreement ~ scale(error_rate_emp)   + (1|original_id), 
            data = train_data, family = binomial)

m11a <- glmer(agreement ~ scale(error_rate_emp)  + scale(rarity_loc) + (1|original_id), 
            data = train_data, family = binomial)

m11b <- glmer(agreement ~ scale(error_rate_emp)  + scale(rarity_loc) + (1|observer) + (1|original_id), 
             data = train_data, family = binomial)

m12a <- glmer(agreement ~ scale(error_rate_emp)  + scale(rarity_tags)  + (1|original_id), 
            data = train_data, family = binomial)

m12b <- glmer(agreement ~ scale(error_rate_emp)  + scale(rarity_tags) + (1|observer) + (1|original_id), 
             data = train_data, family = binomial)

m13 <- glmer(agreement ~ scale(error_rate_emp)  + (1|original_id), 
             data = train_data, family = binomial)


Cand.mods <- list("m1" = m1, "m2" = m2,  "m3" = m3, "m4" = m4,
                  "m5" = m5, "m6" = m6, "m7" = m7, "m8" = m8, "m9" = m9, 
                  "m10" = m10, "m11a" = m11a,"m11b" = m11b, "m12a" = m12a, "m12b" = m12b, "m13" = m13)

aic <- aictab(cand.set = Cand.mods, mod.names = NULL, second.ord = FALSE, nobs = NULL, sort = TRUE)

aic

summary(m12b)
saveRDS(m12b, file="m12b.rds")

simulationOutput <- simulateResiduals(fittedModel = m12b, n = 1000, use.u = T, plot = T)

testResiduals(simulationOutput)

## predict to witheld test data

#filter before prediction? We would normally review all tags from any species with 7 or fewer tags

pred <- predict(m12b, newdata = test_data, type = "response", allow.new.levels = TRUE)

test_data$predicted <- pred

spp_list_test <- test_data %>% group_by(original_id) %>% summarise(n = n()) %>% 
  filter(n > 7)

test_data_filt <- test_data %>% filter(original_id %in% spp_list_test$original_id)


plot_test1 <- test_data_filt %>% 
  group_by(original_id) %>% 
  dplyr::summarise(mean_predicted = mean(predicted), mean_test = mean(agreement), n = n())


plot_test2 <- test_data_filt %>% 
  group_by(original_id, observer) %>% 
  dplyr::summarise(mean_predicted = mean(predicted), mean_test = mean(agreement), n = n())


plot_test3 <- test_data_filt %>% 
  group_by(observer, new_level) %>% 
  dplyr::summarise(mean_predicted = mean(predicted), mean_test = mean(agreement), n = n())



##make graph of mean agreement empirical and mean pred agreement

fig1 <- ggplot(plot_test1, aes(mean_predicted, mean_test)) + geom_point(aes(size = n), alpha = 0.5) +  ylim(0,1) + xlim(0,1) +
  geom_text(aes(label=ifelse(mean_test < 0.5, as.character(original_id),'')),hjust=-0.05,vjust=-0.1) +
  geom_abline(intercept = 0, slope = 1) + labs(title = "Mean agreement by species", y = "Agreement from test data", x = "Predicted agreement") + theme_bw()

fig1


fig2 <- ggplot(plot_test2, aes(mean_predicted, mean_test)) + geom_point(aes(size = n), alpha = 0.5) +  ylim(0,1) + xlim(0,1) +
  #geom_text(aes(label=ifelse(mean_test < 0.5, as.character(original_id),'')),hjust=0,vjust=0) +
  geom_abline(intercept = 0, slope = 1) + labs(title = "Mean agreement by species and observer",  y = "Agreement from test data", x = "Predicted Agreement") + theme_bw()

fig2


fig3 <- ggplot(plot_test3, aes(mean_predicted, mean_test)) + geom_point(aes(size = n, colour = new_level), alpha = 0.5) +
  ylim(0,1) + xlim(0,1) +
  #geom_text(aes(label=ifelse(mean_test < 0.5, as.character(original_id),'')),hjust=0,vjust=0) +
  geom_abline(intercept = 0, slope = 1) + labs(title = "Mean agreement by observer", y = "Agreement from test data", x = "Predicted Agreement") + theme_bw()

fig3


#ggplot(plot_test, aes(mean_prediction, Empirical)) + geom_point() +  ylim(0,1) + xlim(0,1) +
# geom_text(aes(label=ifelse(Empirical < 0.51, as.character(ID1),'')),hjust=0,vjust=0) +
#geom_abline(intercept = 0, slope = 1) + labs(y = "Empirical Agreement", x = "Predicted Agreement") + theme_bw()

##correlation test

cor.test(plot_test$mean_predicted, plot_test$mean_test, method=c("pearson", "kendall", "spearman"))


library(pROC)

par(pty="s")

roc1 <- roc(test_data$agreement, test_data$predicted)

roc1

ci.auc(roc1)  

plot(roc1)


mean(train_data$agreement)
mean(test_data$agreement)

##make some plots

p1 <- plot_model(m12b, type = "pred", terms = c("error_rate_emp [all]"), title = "", 
                 axis.title = c("Empirical error rate", "Probability of agreement"),
                 colors = "#482677FF") + theme_classic()

p1


p2 <- plot_model(m12b, type = "pred", terms = c("rarity_tags [all]"), title = "", 
                                                axis.title = c("Species commonness", "Probability of agreement"),
                                                colors = "#d47400") + theme_classic()

p2

plot_model(m12b, type = "pred", terms = c("error_rate_emp [all]", "observer[Christ Chutter, Erica Alex, strix.temp, Thea Carpenter]"), pred.type = "re", ci.lvl = NA)


tab_model(m12b, transform = NULL)

## graph using data filt

top_15 <- spp_list %>% arrange(-n) %>% slice_head(n =15)

ggplot(top_15, aes(x = n, y = reorder(original_id, n))) + geom_bar(stat = "identity") +
  labs(x = "Number of tags verified", y = "")  + 
  scale_x_continuous(limits = c(0,1250), expand = c(0, 0)) +
  theme_bw()



