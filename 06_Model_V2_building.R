## Build new models

library(lme4)
library(ggpmisc)
library(AICcmodavg)
library(DHARMa)

## we want to get away from needing to include test score, can we predict agreement
# using data from 2017

load("05_qaqc2.0_data_clean.Rdata")

#load observer look up tables

b_obs <- read.csv("data/trait_tables/boreal_transcriber_traits.csv")
g_obs <- read.csv("data/trait_tables/grassland_transcriber_traits.csv")


# create observer summary by year

obs_summary <- combof %>% group_by(observer) %>% summarise(n_verified = n(), mean_agree = mean(agreement))


## Split dataset into validation and training
#use 70% of dataset as training set and 30% as test set

set.seed(101)

# Create a random split (70% training, 30% testing)
sample_size <- floor(0.7 * nrow(combof))
model_indices <- sample(seq_len(nrow(combof)), size = sample_size)
model_data <- combof[model_indices, ]
empirical_data <- combof[-model_indices, ]


# calculate observer error rate

empirical_summary <- empirical_data %>% group_by(observer) %>% summarise(n_tags_emp = n(), 
                                                n_spp_emp = length(unique(original_id)), 
                                                error_rate_emp = (1- mean(agreement)))


# split model data into test and train

set.seed(123)

# Create a random split (70% training, 30% testing)
sample_size <- floor(0.7 * nrow(model_data))
train_indices <- sample(seq_len(nrow(model_data)), size = sample_size)
train_data <- model_data[train_indices, ]
test_data <- model_data[-train_indices, ]

#summaries for train and test data

train_summary <- train_data %>% group_by(observer) %>% summarise(n_tags_train = n(), 
                                                                 n_spp_train = length(unique(original_id)), 
                                                                 error_rate_train = (1- mean(agreement))) %>% 
  filter(n_tags_train > 20)

test_summary <- test_data %>% group_by(observer) %>% summarise(n_tags_test = n(), 
                                                                 n_spp_test = length(unique(original_id)), 
                                                                 error_rate_test = (1- mean(agreement))) %>% 
  filter(n_tags_test > 20)

# join into one data frame and save

s1 <- full_join(empirical_summary, train_summary, by = "observer")

obs_summary <- full_join(s1, test_summary, by = "observer")

obs_summary <- obs_summary %>% filter(observer != "Not Assigned")

write_csv(obs_summary, "observer_error_summary.csv")

ggplot(obs_summary, aes(x = error_rate_train, y = error_rate_test)) + geom_point()

ggplot(obs_summary, aes(x = error_rate_test, y = error_rate_emp)) + geom_point() +
  geom_smooth(method = "lm") + stat_poly_eq(aes(label = paste(stat(eq.label), stat(rr.label), sep = " * \", \" * ")), 
                                            formula = y ~ x, parse = TRUE)

# join error rate to main data

train_data <- left_join(train_data, empirical_summary, by = "observer")
test_data <- left_join(test_data, empirical_summary, by = "observer")


## model building time

## what needs to be factors?

train_data$original_id <- as.factor(train_data$original_id)
train_data$needs_review <- as.factor(train_data$needs_review)


m1 <- glmer(agreement ~ scale(error_rate_emp) + scale(duration2) + scale(peak_db2) +
              scale(max_tag_freq) + scale(rarity_tags) + needs_review + (1|original_id), 
            data = train_data, family = binomial)

m2 <- glmer(agreement ~ scale(error_rate_emp) + scale(duration2) + scale(peak_db2) +
              scale(max_tag_freq) + scale(rarity_loc) + needs_review + (1|original_id), 
            data = train_data, family = binomial)

m3 <- glmer(agreement ~ scale(error_rate_emp) + scale(duration2) + scale(peak_db2) +
              needs_review + (1|original_id), 
            data = train_data, family = binomial)

m4 <- glmer(agreement ~ scale(error_rate_emp) + scale(duration2) +
              scale(max_tag_freq) + needs_review + (1|original_id), 
            data = train_data, family = binomial)

m5 <- glmer(agreement ~ scale(error_rate_emp) + scale(duration2) + needs_review + (1|original_id), 
            data = train_data, family = binomial)

m6 <- glmer(agreement ~ scale(error_rate_emp) + scale(duration2) + scale(rarity_tags) + needs_review + (1|original_id), 
            data = train_data, family = binomial)

m7 <- glmer(agreement ~ scale(error_rate_emp)  + needs_review + (1|original_id), 
            data = train_data, family = binomial)

m8 <- glmer(agreement ~ scale(error_rate_emp)  + scale(rarity_loc) + needs_review + (1|original_id), 
            data = train_data, family = binomial)

m9 <- glmer(agreement ~ scale(error_rate_emp)  + scale(rarity_tags) + needs_review + (1|original_id), 
            data = train_data, family = binomial)

m10 <- glmer(agreement ~ scale(error_rate_emp)  + (1|original_id), 
            data = train_data, family = binomial)


Cand.mods <- list("m1" = m1, "m2" = m2,  "m3" = m3, "m4" = m4,
                  "m5" = m5, "m6" = m6, "m7" = m7, "m8" = m8, "m9" = m9, "m10" = m10)

aic <- aictab(cand.set = Cand.mods, mod.names = NULL, second.ord = FALSE, nobs = NULL, sort = TRUE)

aic

summary(m8)

simulationOutput <- simulateResiduals(fittedModel = m8, n = 1000, use.u = T, plot = T)

## predict to witheld test data

pred <- predict(m8, newdata = test_data, type = "response", allow.new.levels = TRUE, re.form = ~(1|original_id))

test_data$predicted <- pred

spp_list <- test_data %>% group_by(original_id) %>% summarise(n = n()) %>% 
  filter(n >10)

test_data_filt <- test_data %>% filter(original_id %in% spp_list$original_id)

plot_test <- test_data %>% filter(original_id %in% spp_list$original_id) %>% 
  group_by(original_id) %>% 
  dplyr::summarise(mean_predicted = mean(predicted), mean_test = mean(agreement), n = n())


##make graph of mean agreement empirical and mean pred agreement

fig4 <- ggplot(plot_test, aes(mean_predicted, mean_test)) + geom_point() +  ylim(0,1) + xlim(0,1) +
  #geom_text(aes(label=ifelse(Empirical < 0.5, as.character(ID1),'')),hjust=0,vjust=0) +
  geom_abline(intercept = 0, slope = 1) + labs(y = "Agreement from test data", x = "Predicted Agreement") + theme_bw()

fig4

ggsave(filename = "figure4_empiricalvspred_agreement.tiff", plot =  fig4, path = "figures/",
       height = 5, width = 7, dpi = 320)


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
