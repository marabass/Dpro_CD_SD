##Setup
library(readr)

## BMB: try not to load packages you're not using yet
## (or comment them out)
library(glmmTMB)
library(lme4)
library(DHARMa)

MainData <- read.csv("MP_SpeciesStarvation_Clean.csv")
saveRDS(MainData, file = "MainData.rds")
MainData <- read_rds("MainData.rds")

##Model Creation, starting with very simple linear model just as a start, will update it to a more accurate model later
lm1 <- lm(leg_tibL~species_strain*block, data = MainData)
## BMB: this is kind of terrible.
plot(lm1)
performance::check_model(lm1)
plot(simulateResiduals(lm1))

## log-transforming helps a lot
## (what is the extreme group in the data?)

lm2 <- lm(log(leg_tibL)~species_strain*block, data = MainData)
plot(simulateResiduals(lm2))
performance::check_model(lm2)

