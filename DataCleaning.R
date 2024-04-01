library(tidyverse)

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

#separate trait values by sex
Dprol_NoCondition <- Dprol_size[,-19]
Dprol_male <- Dprol_NoCondition[Dprol_NoCondition$sex == "M",8:20,]
Dprol_female <- Dprol_NoCondition[Dprol_NoCondition$sex == "F",8:20,]

#Dprol and Dmel data
Combine_size <- DmelFull_size[DmelFull_size$species_full == "D_melanogaster", "D_prolongata",]

