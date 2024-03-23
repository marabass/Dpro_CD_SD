##Setup
library(readr)
library(glmmTMB)
library(lme4)

MainData <- read.csv("MP_SpeciesStarvation_Clean.csv")
saveRDS(MainData, file = "MainData.rds")
MainData <- read_rds("MainData.rds")

##Model Creation, starting with very simple linear model just as a start, will update it to a more accurate model later
lm1 <- lm(leg_tibL~species_strain*block, data = MainData)
plot(lm1)



