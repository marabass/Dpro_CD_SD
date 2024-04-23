
#libraries 

library(performance)
library(tidyverse)
library(emmeans)
library(dotwhisker)
library(lattice)
library(DHARMa)
library(lme4)
library(broom.mixed)


#load in long D.pro long data set 
Dprol_long_dummy <- readRDS("Dprol_long_dummy.rds")
head(Dprol_long_dummy)
str(Dprol_long_dummy)
Dprol_long_dummy$units

#lmer - random effects units 
lmm_dummy <- lmer(value ~ trait:(sex * condition) - 1 + (trait-1|units), data = Dprol_long_dummy, 
                  control = lmerControl(optCtrl=list(ftol_abs=1e-8),
                                        check.nobs.vs.nlev="ignore",
                                        check.nobs.vs.nRE="ignore"))

isSingular(lmm_dummy) # with a higher threshold for random effects variance. The model does not return a singular fit 
all(abs(getME(lmm_dummy,"theta"))>1e-4) #Note: 'theta' is the VCV parameters


varcovR<- VarCorr(lmm_dummy) #random effects variance covariance matrix

diag(varcovR$units) <- diag(varcovR$units)+sigma(lmm_dummy)^2
corrplot.mixed(cov2cor(varcovR$units),upper="ellipse") #correlation within individuals is very high 
VarCorr(varcovR$units)

#diagnostics and troubleshooting 

diagnostic_Dummy <- simulateResiduals(lmm_dummy)
plot(diagnostic_Dummy)

check_model(lmm_dummy)

qqmath(lmm_dummy)

fit_opt <- allFit(lmm_dummy)
glance(fit_opt) %>% select(optimizer, AIC)
tidy(fit_opt, conf.int = TRUE) %>% arrange(effect, term, estimate) 

#parameter estimates 
summary(lmm_dummy)
coef(lmm_dummy) #indiv-level estimates
confint(lmm_dummy) #confidence intervals 
fixef(lmm_dummy) #fixed effect coefficients 
ranef(lmm_dummy) #indiv deviations from pop mean

#coefficient plot 
cc1 <- tidy(lmm_dummy,effect="fixed") %>%
  separate(term, into = c("trait","fixeff"), extra = "merge",
                  remove = FALSE)
dwplot(cc1)+facet_wrap(~fixeff,scale="free",ncol=2)+
  geom_vline(xintercept=0,lty=2)

names(cc1)

#emmeans
all_traits_ssd <- emmeans(lmm_dummy,  pairwise ~ sex*condition*trait)
all_traits_ssd_contrasts <- contrast(all_traits_ssd[[1]], 
                                     interaction = c(condtion = "pairwise", sex = "pairwise"),
                                     by = "trait")
all_traits_ssd_contrasts
confint(all_traits_ssd_contrasts)

plot(all_traits_ssd_contrasts) + 
  geom_vline(xintercept = 0, lty = 2, alpha = 0.5) + 
  labs(x = "log2 change in SSD in HC vs LC", y = "comparison") +
  theme_bw()

