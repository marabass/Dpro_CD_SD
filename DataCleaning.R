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

#Converting to a long data set 

Dprol_trait_full <- select(Dprol_size, leg_tibL, leg_tibW, leg_tar1L, thorax_length_mm, species_full, cohort, sex, specimen, condition) 

#log2(Dprol_size$leg_tibL*1000)

Dprol_trait_full$tibL_log2 <- (log2((Dprol_trait_full[,"leg_tibL"])*1000))
Dprol_trait_full$tibW_log2 <- (log2((Dprol_trait_full[,"leg_tibW"])*1000))
Dprol_trait_full$tar1L_log2 <- (log2((Dprol_trait_full[,"leg_tar1L"])*1000))
Dprol_trait_full$thoraxl_log2 <- (log2((Dprol_trait_full[,"thorax_length_mm"])*1000))

Dprol_long <- (Dprol_trait_full[,5:13] 
               %>% gather(trait,value, c(tibL_log2 , tibW_log2, tar1L_log2, thoraxl_log2))
)

head(Dprol_long)
str(Dprol_long)

saveRDS(Dprol_long, "Dprol_long.rds")
