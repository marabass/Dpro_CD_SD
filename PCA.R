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
Dprol_log2_male_trait <- Dprol_male_trait[,c(4,5,6,7)] #This only contaains leg and thorax measurements - does not contain any wing measurements. 

Dprol_female <- Dprol_size[Dprol_size$sex == "F", 8:21]
Dprol_female_trait <- select(Dprol_female, -condition, -cohort_num)
Dprol_log2_female_trait <- Dprol_female_trait[,c(4,5,6,7)]

###Anything above this should go in a separate data clean up script 
  
##variance covariance matrix 

cov(Dprol_male_trait)
cov(Dprol_female_trait)

cov(Dprol_log2_male_trait)
cov(Dprol_log2_female_trait)

##PCA - Note that the subset contains both raw values in millieters and log2 transformed values in micrometers. 
#We should probably decide which to use in our PCA - If it makes sensses to use log2 transformed values are a PCA, then we should use those, because we will have to log2 transform anwyays to compare between traits

PC_male <- prcomp(Dprol_male_trait) # raw values including wing 
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

PC_male_log <- prcomp(Dprol_log2_male_trait) #log transformed without wing 
plot(PC_male_log)

PC1 <- PC_male_log$x[,1]

PC_female_log <- prcomp(Dprol_log2_female_trait)
plot(PC_female_log)
  
#NOte: These won't work if you use princomp
ggplot(Dprol_log2_male_trait, aes(y = PC_male_log$x[,2], x = PC_male_log$x[,1])) +
  geom_point() +
  xlim(-2, 2) +
  ylim(-2, 2) 


ggplot(Dprol_log2_female_trait, aes(y = PC_female_log$x[,2], x = PC_female_log$x[,1])) +
  geom_point() +
  xlim(-2, 2) +
  ylim(-2, 2) 

#calculating size corrected values
Size_corrected_tibL <- Dprol_log2_male_trait$leg_log_tibL/PC1




  

##Trying to determine variable contributions
loadings(female_PCA)
##Both of these look like theyre doing the proper thing but are not the same, maybe showing slightly different things?
var <- get_pca_var(PC_female)
head(var$contrib)
print(var$contrib)