
#library
library(tidyverse)

#load in the data 
DmelFull_size <- read.csv("MP_SpeciesStarvation_Clean.csv")

#extracting D prol data 
Dprol_size <- DmelFull_size[DmelFull_size$species_full == "D_prolongata",]


#will need to convert character vectors to factors - Should probably have another script that does this (i.e., data clean up)
Dprol_size$sex <- as.factor(Dprol_size$sex)
Dprol_size$condition <- as.factor(Dprol_size$condition)
Dprol_size$block <- as.factor(Dprol_size$block)
Dprol_size$cohort  <- as.factor(Dprol_size$cohort)
str(Dprol_size)



Dprol_male <- Dprol_size[Dprol_size$sex == "M", c(8:21)]
Dprol_male_trait <- select(Dprol_male, -condition, -cohort_num) 

Dprol_female <- Dprol_size[Dprol_size$sex == "F", 7:]

#variance covariance matrix 

cov(Dprol_male_trait)

#PCA

PC <- prcomp(Dprol_male_trait)
summary(PC)
plot(PC)
princomp(Dprol_male_trait)



ggplot(Dprol_male, aes(y = PC$x[,2], x = PC$x[,1])) +
  geom_point() +
  xlim(-3, 4) +
  ylim(-3, 4) 