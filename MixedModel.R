
#libraries 
library(blme)
library(performance)
library(tidyverse)
library(emmeans)
library(dotwhisker)
library(lattice)
library(DHARMa)

#load in long D.pro data set 
Dprol_long <- readRDS("Dprol_long.rds")

Dprol_long_dummy <- readRDS("Dprol_long_dummy.rds")
head(Dprol_long_dummy)
Dprol_long_dummy$units

#linear mixed model
blmm1 <- blmer(value ~ trait:(sex * condition) - 1 + (trait-1|specimen), data = Dprol_long)

lmm_dummy <- lmer(value ~ trait:(sex * condition) - 1 + (trait-1|units), data = Dprol_long_dummy)

lmm2_dummy <- lmer(value ~ trait:(sex * condition) - 1 + (trait-1|condition:specimen), data = Dprol_long_dummy)
library(lme4)

#Diagnostics
class(blmm1) <- "merMod" #diagnostics using the preformance package 
check_model(blmm1)
qqmath(blmm1) #QQ plot using Lattice package 
Diagnostic_blmm1 <- simulateResiduals(blmm1)
plot(Diagnostic_blmm1)


VarCorr(lmm2_dummy)
summary(blmm1)

#dw plot
dwplot(blmm1)

#Emmeans 
all_traits_ssd <- emmeans(blmm1,  pairwise ~ sex*condition*trait)

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

all_traits_ssd_extract  <- pairs(emmeans(blmm1, 
                                         specs = ~ sex | condition + trait), simple = "sex", by = "trait")
