library(tidyverse)
library(readr)

#load in the data 
DmelFull_size <- read.csv("MP_SpeciesStarvation_Clean.csv")

#extracting D prol data 
Dprol_size <- DmelFull_size[DmelFull_size$species_full == "D_prolongata",]
Dmel_size <- DmelFull_size[DmelFull_size$species_full == "D_melanogaster",]
melprol_size <- rbind(Dprol_size, Dmel_size)


#will need to convert character vectors to factors - Should probably have another script that does this (i.e., data clean up)
Dprol_size$sex <- as.factor(Dprol_size$sex)
Dprol_size$condition <- as.factor(Dprol_size$condition)
Dprol_size$block <- as.factor(Dprol_size$block)
Dprol_size$cohort  <- as.factor(Dprol_size$cohort)
str(Dprol_size)

Dmel_size$sex <- as.factor(Dmel_size$sex)
Dmel_size$condition <- as.factor(Dmel_size$condition)
Dmel_size$block <- as.factor(Dmel_size$block)
Dmel_size$cohort  <- as.factor(Dmel_size$cohort)
str(Dmel_size)

melprol_size$sex <- as.factor(melprol_size$sex)
melprol_size$condition <- as.factor(melprol_size$condition)
melprol_size$block <- as.factor(melprol_size$block)
melprol_size$cohort  <- as.factor(melprol_size$cohort)
str(melprol_size)

saveRDS(Dmel_size, "Dmel_size.rds")
saveRDS(Dprol_size, "Dprol_size.rds")
saveRDS(melprol_size, "melprol_size.rds")

#separate trait values by sex
Dprol_NoCondition <- Dprol_size[,-19]
Dprol_male <- Dprol_NoCondition[Dprol_NoCondition$sex == "M",8:20,]
Dprol_female <- Dprol_NoCondition[Dprol_NoCondition$sex == "F",8:20,]

