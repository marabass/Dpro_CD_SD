##library
library(tidyverse)
##install.packages("factoextra")
library(factoextra)

##load in the data 
DmelFull_size <- read.csv("MP_SpeciesStarvation_Clean.csv")

##extracting D prol data 
Dprol_size <- DmelFull_size[DmelFull_size$species_full == "D_prolongata",]


##will need to convert character vectors to factors - Should probably have another script that does this (i.e., data clean up)
Dprol_size$sex <- as.factor(Dprol_size$sex)
Dprol_size$condition <- as.factor(Dprol_size$condition)
Dprol_size$block <- as.factor(Dprol_size$block)
Dprol_size$cohort  <- as.factor(Dprol_size$cohort)
str(Dprol_size)



Dprol_male <- Dprol_size[Dprol_size$sex == "M", c(8:21)]
Dprol_male_trait <- select(Dprol_male, -condition, -cohort_num) 

Dprol_female <- Dprol_size[Dprol_size$sex == "F", 8:21]
Dprol_female_trait <- select(Dprol_female, -condition, -cohort_num)

##variance covariance matrix 

cov(Dprol_male_trait)
cov(Dprol_female_trait)

##PCA

PC_male <- prcomp(Dprol_male_trait)
summary(PC_male)
plot(PC_male)
princomp(Dprol_male_trait)

PC_female <- prcomp(Dprol_female_trait)
summary(PC_female)
plot(PC_female)
female_PCA <- princomp(Dprol_female_trait)

ggplot(Dprol_male, aes(y = PC_male$x[,2], x = PC_male$x[,1])) +
  geom_point() +
  xlim(-2, 2) +
  ylim(-2, 2) 

ggplot(Dprol_female, aes(y = PC_female$x[,2], x = PC_female$x[,1])) +
  geom_point() +
  xlim(-2, 2) +
  ylim(-2, 2) 

##Trying to determine variable contributions
loadings(female_PCA)
##Both of these look like theyre doing the proper thing but are not the same, maybe showing slightly different things?
var <- get_pca_var(PC_female)
head(var$contrib)
print(var$contrib)