#calculating allometric slopes using standardized major axis regression 

#Read in packages
#install.packages("smatr")
library(smatr)
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

#separate trait values by sex 
Dprol_male <- Dprol_size[Dprol_size$sex == "M",]
Dprol_female <- Dprol_size[Dprol_size$sex == "F",]

#Will use standardized least squares residuals to model a regression line using log2 transformed value of thorax size 
#as my predictor value - then extract the coefficient of my regression line as my estimate of relative trait size

#pool cohort level, but separate by sex - log 2 transformedd values in micrometers 

#male tibia length 
sma(leg_log_tibL ~ thorax_log_length_mm, data = Dprol_male)
