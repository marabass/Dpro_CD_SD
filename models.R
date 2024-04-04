#Loading packages 

library(lme4)
library(blme)
#library(Matrix)
#library(ggplot2)
library(tidyverse)
#library(dotwhisker)
#library(performance)
library(emmeans)
library(ggplot2); theme_set(theme_bw())
library(corrplot)
#install.packages("lme4")
#install.packages("Matrix")

##load in the data 
Dprol_size <- readRDS("Dprol_size.rds")

# response: total size - lm(PCx ~ sex + condition + (1|block))
#PC1 as a measure of total shape 

Dprol_trait_size <- select(Dprol_size, leg_tibL, leg_tibW, leg_tar1L, thorax_length_mm) 

Dprol_trait_size_log <- log2(Dprol_trait_size*1000)

Dprol_PC <- prcomp(Dprol_trait_size_log) 
summary(Dprol_PC)
PC_loadings <- Dprol_PC$rotation

ggplot(Dprol_size, aes(y = Dprol_PC$x[,1], x = Dprol_PC$x[,2], col = condition)) +
  geom_point() +
  #xlim(-5, 0.5) +
  #ylim(-0.5, 0.5) +
  labs(x = "PC1 (92.7%)", y = "PC2 (7.2%)")

ggplot(Dprol_size, aes(y = Dprol_PC$x[,2], x = Dprol_PC$x[,3], col = sex)) +
  geom_point() +
  #xlim(-0.5, 0.5) +
  #ylim(-0.5, 0.5) +
  labs(x = "PC2 (7.2%)", y = "PC3 (0.094%)")

Dprol_sizeVar <- Dprol_PC$x[,1]

lm_size <- lm(Dprol_sizeVar ~ sex * condition, data = Dprol_size)
plot(lm_size)

ggplot(data = Dprol_size, mapping = aes(condition, Dprol_sizeVar, group = sex, colour = sex)) + 
  geom_point() +
  geom_smooth(method = lm) + 
  ylab("variation in size (PC1)")

summary(lm_size)

#I want to correct for variation between individuals within the condition cohorts - using a nested design 

lmm <- lmer(Dprol_sizeVar ~ sex * condition + (1 | condition) + (1 |condition:specimen), data = Dprol_size)
isSingular(lmm)

#lm2 <- lm(Dprol_shapVar ~ sex * condition + log2(leg_tibL), data = Dprol_size)
#ggplot(data = Dprol_size, mapping = aes(leg_tibL, Dprol_shapVar, color=sex)) + geom_point()
#plot(lm2)



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

Dprol_trait_full <- select(Dprol_size, leg_tibL, leg_tibW, leg_tar1L, thorax_length_mm, species_full, cohort, sex, specimen, condition) 

Dprol_trait_full$tibL_log2 <- (log2(Dprol_trait_full[,"leg_tibL"])*1000)
Dprol_trait_full$tibW_log2 <- (log2(Dprol_trait_full[,"leg_tibW"])*1000)
Dprol_trait_full$tar1L_log2 <- (log2(Dprol_trait_full[,"leg_tar1L"])*1000)
Dprol_trait_full$thoraxl_log2 <- (log2(Dprol_trait_full[,"thorax_length_mm"])*1000)



#variance co-variance matrix - checking the correlation between our variables - covariation seems quite low - is this good? 
cor(Dprol_trait_full[Dprol_trait_full$sex == "M",1:4])

cor(Dprol_trait_full[Dprol_trait_full$sex == "F",1:4])

cor(Dprol_trait_full[,1:4]) # this is not that meaningful, but shows the impact of Simpson's paradox

pairs(Dprol_trait_full[,1:4])

scatterplotMatrix( ~ tibL_log2 + tibW_log2 + tar1L_log2 +thoraxl_log2|sex, 
                   ellipse = T, data = Dprol_trait_full,
                   transform = T)
#fitting a mixed linear model 

Dprol_trait_size <- Dprol_trait_full[,10:13]

Ysize <- as.matrix(Dprol_trait_size)
lmMultiv <- lm(Ysize ~ sex * condition, data = Dprol_trait_full)
#class(lmMultiv)
summary(lmMultiv )
summary(manova(lmMultiv))

all_traits_ssd <- emmeans(lmMultiv, pairwise ~ sex*condition)

all_traits_ssd_contrasts <- mvcontrast(all_traits_ssd[[1]], interaction = c(condition = "pairwise", sex = "pairwise"),
                                    mult.name = Ysize)


all_traits_ssd_contrasts

confint(all_traits_ssd_contrasts)


#The contrasts on display here are the differences in log2 trait size between males and females across high and low condition 
plot(all_traits_ssd_contrasts) + 
  geom_vline(xintercept = 0, lty = 2, alpha = 0.5) + 
  labs(x = "log2 change in SSD in HC vs LC", y = "comparison") +
  theme_bw()

all_traits_ssd_extract  <- pairs(emmeans(lmm1, 
                                         specs = ~ sex | condition + trait), simple = "sex", by = "trait")

#converting our data set to the long version 
head(Dprol_trait_size_log)

Dprol_long <- (Dprol_trait_full 
             %>% gather(trait,value, c(leg_tibL, leg_tibW, leg_tar1L, thorax_length_mm))
             %>% mutate(value=log2(value*1000))
)

head(Dprol_long)
#fitting a linear mixed model 
lmm1 <- lmer(value ~ trait:(sex * condition) - 1 + (trait-1|specimen), data = Dprol_long, 
             control = lmerControl(optCtrl=list(ftol_abs=1e-8),
                                   check.nobs.vs.nlev="ignore",
                                  check.nobs.vs.nRE="ignore"))


summary(lmm1)
#par(mfrow=c(1,2))
vv1 <- VarCorr(lmm1) #somthing wrong here - 
## fix unit variance-covariance by adding residual variance:
diag(vv1$specimen) <- diag(vv1$specimen)+sigma(lmm1)^2
corrplot.mixed(cov2cor(vv1$specimen),upper="ellipse")


##contrasts 



all_traits_ssd <- emmeans(lmm1,  pairwise ~ sex*condition*trait)

all_traits_ssd_contrasts <- contrast(all_traits_ssd[[1]], 
                                     interaction = c(condtion = "pairwise", sex = "pairwise"),
                                     by = "trait")


all_traits_ssd_contrasts

confint(all_traits_ssd_contrasts)


#The contrasts on display here are the differences in log2 trait size between males and females across high and low condition 
plot(all_traits_ssd_contrasts) + 
  geom_vline(xintercept = 0, lty = 2, alpha = 0.5) + 
  labs(x = "log2 change in SSD in HC vs LC", y = "comparison") +
  theme_bw()

all_traits_ssd_extract  <- pairs(emmeans(lmm1, 
                                         specs = ~ sex | condition + trait), simple = "sex", by = "trait")

