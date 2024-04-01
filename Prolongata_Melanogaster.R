library(readr)
library(lme4)
library(Matrix)
library(ggplot2)
library(tidyverse)
library(dotwhisker)

Dmel_size <- readRDS("Dmel_size.rds")
Dprol_size <- readRDS("Dprol_size.rds")
melprol_size <- readRDS("melprol_size.rds")

##Doing data analysis with melanogaster, might be unnecessary depending on how we end up doing second step of analysis

##PCA melanogaster
Dmel_trait_size <- select(Dmel_size, leg_tibL, leg_tibW, leg_tar1L, thorax_length_mm) 
Dmel_PC <- prcomp(Dmel_trait_size)

ggplot(Dmel_trait_size, aes(y = Dmel_PC$x[,1], x = Dmel_PC$x[,2])) +
  geom_point() +
  xlim(-2, 2) +
  ylim(-2, 2) 

ggplot(Dmel_trait_size, aes(y = Dmel_PC$x[,2], x = Dmel_PC$x[,3])) +
  geom_point() +
  xlim(-2, 2) +
  ylim(-2, 2) 

##Shape variation melanogaster, PC2 and on is small, is this necessary to run with principal components?
Dmel_shapVar <- Dmel_PC$x[,2]

lm_total_size_Dmel <- lm(Dmel_shapVar ~ sex * condition, data = Dmel_size)
plot(lm_total_size)

lm2 <- lm(Dmel_shapVar ~ sex * condition + log2(leg_tibL), data = Dmel_size)

ggplot(data = Dmel_size, mapping = aes(leg_tibL, Dmel_shapVar, color=sex)) + geom_point()

plot(lm2)
dwplot(lm2)

##Size variation melanogaster
Dmel_sizeVar <- Dmel_PC$x[,1]
lm_total_size_Dmel <- lm(Dmel_sizeVar ~ sex * condition, data = Dmel_size)
plot(lm_total_size)

lm2 <- lm(Dmel_sizeVar ~ sex * condition + log2(leg_tibL), data = Dmel_size)

ggplot(data = Dmel_size, mapping = aes(leg_tibL, Dmel_sizeVar, color=sex)) + geom_point()

plot(lm2)
dwplot(lm2)
