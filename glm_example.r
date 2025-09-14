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
  as.formula(paste("ClaimNb ~", paste(predictors, collapse = " + "), #our target is ClaimNb, hence it's at the beginning with "~" sign
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



#PURE PREM MODEL#####
#___________________________________________________________________________________________________


