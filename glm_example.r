#LIBRARIES#####
#___________________________________________________________________________________________________

#install and load packages function
load_or_install <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

#install (if needed) and then load packages
load_or_install("tidyverse")
load_or_install("data.table")

#LOAD DATA#####
#___________________________________________________________________________________________________

#We're going to load data from CASdataset GitHub repository

##frequency#####
url_freq <- "https://github.com/dutangc/CASdatasets/raw/master/data/freMTPL2freq.rda"
load(url(url_freq))
freq <- freMTPL2freq   #rename dataset
rm(freMTPL2freq)  #remove original to avoid clutter

##severity#####
url_sev <- "https://github.com/dutangc/CASdatasets/raw/master/data/freMTPL2sev.rda"
load(url(url_sev))
sev <- freMTPL2sev   #rename dataset
rm(freMTPL2sev)  #remove original to avoid clutter

##define predictors#####

#check for column type and values
str(freq)
#categorical columns are already stored as factors. No need too convert.
#normally in dataset you'd have categorical values as character, so to be consumed by GLM you'd need
#to convert them to factors (usually by using "mutate(across(where(is.character), as.factor)")

#list column names to identify predictors
names(freq)

#define predictors
predictors <- c("VehPower","VehAge","DrivAge","BonusMalus",
                "VehBrand","VehGas","Area","Density","Region")

#FREQ MODEL#####
#___________________________________________________________________________________________________

glm_pois <- glm(
  as.formula(paste("ClaimNb ~", paste(predictors, collapse = " + "), #our target is ClaimNb, hence it's at the beginning with "~" sign. We're also using paste() with "+" collapse to define our predictors. Feel free to run what's inside as.formula() it will return a string.
                   "+ offset(log(Exposure))")), #we're offsetting by log(Exposure) to account for the fact that the longer policy in force the more chance for a claim
  data = freq,
  family = poisson() #default link log
)

summary(glm_pois) #comprehensive summary of the model including predictor importance (p-values)

dispersion_freq <- glm_pois$deviance / glm_pois$df.residual
dispersion_freq # 0.243 - good

freq_pred <- freq %>%
  mutate(freq_hat = predict(glm_pois, type = "response") / Exposure)

#SEV MODEL#####
#___________________________________________________________________________________________________

#we'll use freq table because it contains features that we need, and join it with our severity table by IDpol
#first let's make sure we don't have any duplicated rows in freq data
freq %>% 
  duplicated() %>% 
  sum() #result is 0, so no duplicated rows

#then join severity data to our features
sev_full <- sev %>%
  inner_join(freq, by = "IDpol") %>%
  filter(ClaimAmount > 0)

glm_sev <- glm(
  as.formula(paste("ClaimAmount ~", paste(predictors, collapse = " + "))), #note that here we don't need to offset our Exposures since we model severity per claim that already happen and doesn't depend on how long exposure has been in-force
  data = sev_full, 
  family = Gamma(link = "log")
)

summary(glm_sev)

freq_sev_pred <- freq_pred %>%
  mutate(sev_hat = predict(glm_sev, newdata = freq, type = "response"))

#PURE PREM MODEL#####
#___________________________________________________________________________________________________

#now we have frequency and seveerity saved in freq_sev_pred dataset, all we need to do is to multiply both
pure_premium_pred <- freq_sev_pred %>%
  mutate(pp_hat = freq_hat * sev_hat,
         expected_cost_per_record = pp_hat * Exposure)

#here's how the final table looks like
head(pure_premium_pred)

#important metrics
sum(pure_premium_pred$expected_cost_per_record) #58.9M expected total losses under the model
sum(pure_premium_pred$pp_hat) #121M expected total losses for the year of exposure for this portfolio (e g if we underwrote all of them at once today, how much we'd get)

head(pure_premium_pred)

#CALIBRATE AND VISUALIZE#####
#___________________________________________________________________________________________________

#actual total claims
actual_by_pol <- sev %>%
  group_by(IDpol) %>%
  summarise(actual_total = sum(ClaimAmount), .groups = "drop")

calib <- pure_premium_pred %>%
  left_join(actual_by_pol, by = "IDpol") %>%
  mutate(actual_total = ifelse(is.na(actual_total), 0, actual_total),
         actual_losscost = actual_total / Exposure) %>%
  mutate(decile = ntile(pp_hat, 10)) %>%   #rank policies by predicted pure premium. ntile() will split data into 10 groups by pp_hat and assign value to each group in column decile
  group_by(decile) %>%
  summarise(
    n = n(),
    avg_pred = mean(pp_hat),
    avg_actual = mean(actual_losscost),
    .groups = "drop"
  )

print(calib)
