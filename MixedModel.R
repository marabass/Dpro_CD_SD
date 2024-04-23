
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

#load in the wide D.pro data set 
Dpro_wide <- readRDS("Dprol_size.rds")
str(Dpro_wide)

#lm - 

lm_multi <- lm(cbind(leg_log_tibL, leg_log_tibW, leg_log_tar1L,  thorax_log_length_mm) ~ sex*condition,
               data = Dpro_wide)

coefLM <- coef(lm_multi)
confintLM <- confint(lm_multi)

#coefplot::coefplot(lm_multi)
dwlm <- tidy(lm_multi, conf.int = TRUE) 

dwplot(dwlm) + facet_wrap(~term, scale="free",ncol=3)+
  geom_vline(xintercept=0,lty=2)

dwlm  %>%
  filter(term != "(Intercept)") %>%
  # reorder the coefficients so that the largest is at the top of the plot
  mutate(term = fct_reorder(term, estimate)) %>%
  ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = .2) +
  # add in a dotted line at zero
  facet_wrap(~response, scale="free",ncol=2) +
  geom_vline(xintercept=0,lty=2)
  

#lmer - random effects units 

Dprol_long_dummy$trait <- as.factor(Dprol_long_dummy$trait)
levels(Dprol_long_dummy$trait)

lmm_dummy <- lmer(value ~ trait:(1 + condition + sex + condition:sex) - 1 + (trait-1|units), data = Dprol_long_dummy, 
                  control = lmerControl(optCtrl=list(ftol_abs=1e-8),
                                        check.nobs.vs.nlev="ignore",
                                        check.nobs.vs.nRE="ignore"))

isSingular(lmm_dummy) # with a higher threshold for random effects variance. The model does not return a singular fit 
all(abs(getME(lmm_dummy,"theta"))>1e-4) #Note: 'theta' is the VCV parameters

checkModelFits <- allFit(lmm_dummy)

glance(checkModelFits)
summary(checkModelFits)


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

