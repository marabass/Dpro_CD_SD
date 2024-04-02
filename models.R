##Note: I don't think block number has to be incorporated as a random effect because this species was all done in the same experimental block
#Maybe specimen number (i.e., the size differences between individuals) would make more sense as a random effect in our models

library(lme4)
library(Matrix)
library(ggplot2)
library(tidyverse)
library(dotwhisker)
library(performance)
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
Dprol_PC <- prcomp(Dprol_trait_size) #Not separated by sex 
summary(Dprol_PC)

ggplot(Dprol_size, aes(y = Dprol_PC$x[,1], x = Dprol_PC$x[,2], col = condition)) +
  geom_point() +
  #xlim(-5, 0.5) +
  #ylim(-0.5, 0.5) +
  labs(x = "PC1 (92.7%)", y = "PC2 (7.2%)")

ggplot(Dprol_size, aes(y = Dprol_PC$x[,2], x = Dprol_PC$x[,3], col = sex)) +
  geom_point() +
  #xlim(-0.5, 0.5) +
  #ylim(-0.5, 0.5) +
  labs(x = "PC2 (7.2%)", y = "PC3 (7.2%)")

Dprol_shapVar <- Dprol_PC$x[,1]

lm_total_size <- lm(Dprol_shapVar ~ sex * condition, data = Dprol_size)
plot(lm_total_size)

lm2 <- lm(Dprol_shapVar ~ sex * condition + log2(leg_tibL), data = Dprol_size)

ggplot(data = Dprol_size, mapping = aes(leg_tibL, Dprol_shapVar, color=sex)) + geom_point()

plot(lm2)
dwplot(lm2)


# response: tibia length -  lm(leg_log_tibL ~ sex * condition) - The effect of the interaction between sex and condition on absolute tibia length

ggplot(data = Dprol_size, mapping = aes(condition, leg_log_tibL, group = sex, colour = sex)) + 
  geom_point() +
  geom_smooth(method = lm) + 
  ylab(" Log2 Tibia length (um)")

#Linear model of the interaction between sex and condition on log2-transformed tibia length in micrometers 

lm1 <- lm(leg_log_tibL ~ sex * condition, data = Dprol_size) 
plot(lm1)
check_model(lm1)
confint(lm1)

#Interpreting the intercept: 9.47645184 -  mean tibia length for high condition females 

tibL_HC_F <-  Dprol_size %>%
  group_by(sex, condition) %>%
  summarize(mean(leg_log_tibL))

tibL_F <- Dprol_size %>%
  group_by(sex) %>%
  summarize(mean(leg_log_tibL))

# sexM coefficient: 0.95287478 - Difference in mean log2 tibia length between high condition females and males 
# conditionLC coefficient: -0.29397591  - difference in mean log2 tibia length between high condition and low condition females 
#Interpreting the coefficient of the interaction term: change in log2 tibia length from high condition to low condition for the difference in mean tibia length between males and females.  

#Note leg_log_tibL has been log2 transformed and is in micrometers 

ggplot(data = Dprol_size, mapping = aes(thorax_log_length_mm, leg_log_tibL, colour = sex)) + 
  geom_point() +
  facet_wrap(~condition) + 
  ylab(" Log2 Tibia length (um)")

#Linear model of the effect of the interaction between log2 transformed (and centered) thorax length, sex and condition on log2(tibia length)
# Mean-centering thorax length 

lm2 <- lm(leg_log_tibL ~ thorax_log_length_mm * sex * condition, data = Dprol_size) 
#Interpreting the intercept: Without mean-centering, the intercept is the log2 (tibia length) for high condition females when thorax length is 0. 
 
#Mean centering thorax thorax log length - This subtracts every value of log2 thorax length by mean thorax length 
Dprol_size$thorax_log_length_mm_centered <- scale(Dprol_size$thorax_log_length_mm, center = T, scale = F)

ggplot(data = Dprol_size, mapping = aes(thorax_log_length_mm_centered, leg_log_tibL, colour = sex)) + 
  geom_point() +
  geom_smooth(method = lm) +
  facet_wrap(~condition) + 
  ylab(" Log2 Tibia length (um)")

lm3 <- lm(leg_log_tibL ~ thorax_log_length_mm_centered* sex * condition, data = Dprol_size) 
plot(lm3)
summary(lm3)

#After mean centering, the intercept of the model is estimated mean log2(tibia length) high condition females at mean body size. OR the predicted log2(tibia length) for high condition females that have an average body size
#Interpreting the three-way interaction coefficient: thorax_log_length_mm_centered:sex:condition - 
#The difference between sexes at high vs low condition for the slope of the relationship between log2(tibia length) and log2(thorax length) 

#Trying to incorporate a random intercept(s?) to control for variability between individuals in condition cohorts 
lmm1 <- lmer(leg_log_tibL ~ thorax_log_length_mm_centered * sex * condition + (1|condition:specimen), data = Dprol_size)
isSingular(lmm2)
#Neither of the mixed models seem to work - trying to incorporate specimen into the model makes the model singular? 

ggplot(data = Dprol_size, mapping = aes(condition, leg_log_tibL, color=sex)) + geom_point()

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
lmMultiv <- lm(Ysize ~ thorax_log_length_mm_centered * sex * condition, data = Dprol_size)

