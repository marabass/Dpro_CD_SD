##Note: I don't think block number has to be incorporated as a random effect because this species was all done in the same experimental block
#Maybe specimen number (i.e., the size differences between individuals) would make more sense as a random effect in our models

library(lme4)
library(Matrix)
library(ggplot2)
library(tidyverse)
library(dotwhisker)
#install.packages("lme4")
#install.packages("Matrix")

##load in the data 
DmelFull_size <- read.csv("MP_SpeciesStarvation_Clean.csv")

##extracting D prol data 
Dprol_size <- DmelFull_size[DmelFull_size$species_full == "D_prolongata",]

Dprol_size$sex <- as.factor(Dprol_size$sex)
Dprol_size$condition <- as.factor(Dprol_size$condition)
Dprol_size$block <- as.factor(Dprol_size$block)
Dprol_size$cohort  <- as.factor(Dprol_size$cohort)
Dprol_size$specimen <- as.factor(Dprol_size$specimen)
str(Dprol_size)

# response: total size - lm(PCx ~ sex + condition + (1|block))
#PC1 as a measure of total shape 

Dprol_trait_size <- select(Dprol_size, leg_tibL, leg_tibW, leg_tar1L, thorax_length_mm) 
Dprol_PC <- prcomp(Dprol_trait_size) #Not sperated by sex 

ggplot(Dprol_trait_size, aes(y = Dprol_PC$x[,1], x = Dprol_PC$x[,2])) +
  geom_point() +
  xlim(-2, 2) +
  ylim(-2, 2) 

ggplot(Dprol_trait_size, aes(y = Dprol_PC$x[,2], x = Dprol_PC$x[,3])) +
  geom_point() +
  xlim(-2, 2) +
  ylim(-2, 2) 

Dprol_shapVar <- Dprol_PC$x[,2]

lm_total_size <- lm(Dprol_shapVar ~ sex * condition, data = Dprol_size)
plot(lm_total_size)

lm2 <- lm(Dprol_shapVar ~ sex * condition + log2(leg_tibL), data = Dprol_size)

ggplot(data = Dprol_size, mapping = aes(leg_tibL, Dprol_shapVar, color=sex)) + geom_point()

plot(lm2)
dwplot(lm2)
# response: tibia length -  lm(leg_log_tibL ~ sex + condtion)

lmm1 <- lmer(leg_log_tibL ~ sex * condition + (1|specimen), data = Dprol_size) # random intercept model 
lmm2 <- lmer(leg_log_tibL ~ sex * condition + (condition|specimen), data = Dprol_size)

#Neither of the mixed models seem to work - trying to incorporate specimen into the model makes the model singular 

ggplot(data = Dprol_size, mapping = aes(condition, leg_log_tibL, color=sex)) + geom_point()
lmTibL <- lm(leg_log_tibL ~ sex * condition, data = Dprol_size)
plot(lmTibL)

# response: tibia width - lm(leg_log_tibW ~ sex + condition)
ggplot(data = Dprol_size, mapping = aes(condition, leg_log_tibW, color=sex)) + geom_point() 
lmTibW <- lm(leg_log_tibW ~ sex * condition, data = Dprol_size)
plot(lmTibW)

#response: leg_log_tar1L - lm(leg_log_tar1L ~ sex + condition)
ggplot(data = Dprol_size, mapping = aes(condition, leg_log_tar1L, color=sex)) + geom_point() 
lmTarW <- lm(leg_log_tar1L ~ sex * condition, data = Dprol_size)
plot(lmTarW)

#multivariate linear regression that incorporates all leg traits 
Ysize <- as.matrix(Dprol_size[,c("leg_log_tibL", "leg_log_tibW", "leg_log_tar1L")])
lmMultiv <- lm(Ysize ~ sex * condition, data = Dprol_size)
performance::check_model(lmMultiv)
summary(lmMultiv)
