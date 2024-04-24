
#libraries 
library(performance)
library(tidyverse)
library(emmeans)
library(dotwhisker)
library(lattice)
library(DHARMa)
library(lme4)
library(broom.mixed)
library(mvinfluence)
library(dfoptim) #delete if not needed anymore for current models
library(optimx) #delete if not needed anymore for current models


#load in long D.pro long data set 
Dprol_long_dummy <- readRDS("Dprol_long_dummy.rds")
head(Dprol_long_dummy)
str(Dprol_long_dummy)
Dprol_long_dummy$units

#load in the wide D.pro data set 
Dprol_wide_dummy <- readRDS("Dprol_wide_dummy.rds")
str(Dprol_wide_dummy)

#lmer - multivariate 'mixed' effect  

Dprol_long_dummy$trait <- as.factor(Dprol_long_dummy$trait)
levels(Dprol_long_dummy$trait)

lmm_dummy <- lmer(value ~ trait:(sex + condition)^2 - 1 + (trait-1|units), data = Dprol_long_dummy, REML = FALSE,
                  control = lmerControl(optCtrl=list(ftol_abs=1e-8),
                                        check.nobs.vs.nlev="ignore",
                                        check.nobs.vs.nRE="ignore"))

isSingular(lmm_dummy) # with a higher threshold for random effects variance. The model does not return a singular fit 
all(abs(getME(lmm_dummy,"theta"))>1e-4) #Note: 'theta' is the VCV parameter for the random effects variable 

#diagnostics and troubleshooting 

diagnostic_Dummy <- simulateResiduals(lmm_dummy)
plot(diagnostic_Dummy)

check_model(lmm_dummy)

qqmath(lmm_dummy)

fit_opt <- allFit(lmm_dummy)
glance(fit_opt) %>% select(optimizer, AIC, NLL_rel) #negative log-likelihood ratio
tidy(fit_opt, conf.int = TRUE) %>% arrange(effect, term, estimate) 

#parameter estimates 

#varcovR<- VarCorr(lmm_dummy) #random effects variance covariance matrix
#diag(varcovR$units) <- diag(varcovR$units)+sigma(lmm_dummy)^2
#corrplot.mixed(cov2cor(varcovR$units),upper="ellipse") #correlation within individuals is very high 
#VarCorr(varcovR$units)

summary(lmm_dummy)
RE_coefficients <- coef(lmm_dummy) #indiv-level estimates
dummy_CIs <- confint(lmm_dummy) #confidence intervals 
FE_coef <- fixef(lmm_dummy) #fixed effect coefficients 
ranef(lmm_dummy) #indiv deviations from pop mean

#coefficient plot 
dwlmm <- tidy(lmm_dummy,effect="fixed") %>%
  separate(term, into = c("trait","fixeff"), sep = ":", extra = "merge", remove = FALSE) 

dwplot(dwlmm)+facet_wrap(~fixeff,scale="free",ncol=2)+
  geom_vline(xintercept=0,lty=2)

#emmeans

all_traits_vals <- emmeans(lmm_dummy, specs = ~ sex | condition + trait)


all_traits_ssd <- emmeans(lmm_dummy,  pairwise ~ sex*condition*trait)
all_traits_sex <- emmeans(lmm_dummy,  condition ~ sex  + trait, by = "sex")

contrast(all_traits_sex, "trt.vs.ctrl")

all_traits_ssd_contrasts <- contrast(all_traits_ssd[[1]], 
                                     interaction = c(condition = "pairwise", sex = "pairwise"),
                                     by = "trait")

all_traits_sex_contrasts <- contrast(all_traits_ssd[[1]], method = (condition = "pairwise"),  by = c("trait", "sex"))

all_traits_ssd_contrasts
all_traits_sex_contrasts

confint(all_traits_ssd_contrasts)

plot(all_traits_ssd_contrasts) + 
  geom_vline(xintercept = 0, lty = 2, alpha = 0.5) + 
  labs(x = "log2 change in SSD at HC vs LC", y = "comparison") +
  theme_bw()


#multivariate fixed effect only 
lm_multi <- lm(cbind(leg_tibL, leg_tibW , leg_tar1L,  thorax_length_mm) ~ sex*condition,
               data = Dprol_wide_dummy)

influencePlot(lm_multi)


summary(manova(lm_multi))
coefLM <- coef(lm_multi)
confintLM <- confint(lm_multi)

dwlm <- tidy(lm_multi, conf.int = TRUE)

dwlm  %>%
  filter(term != "(Intercept)") %>%
  #mutate(term = fct_reorder(term, estimate)) %>%
  ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = .2) +
  facet_wrap(~response, scale="free",ncol=2) +
  geom_vline(xintercept=0,lty=2) + 
  xlab("Effect size") + 
  ylab("Treatment")

blah <- emmeans(lm_multi, specs = ~ condition | rep.meas + sex )

contrast(blah, "pairwise")

all_traits_ssd_contrasts <- contrast(blah, 
                                     interaction = c(condition = "pairwise", sex = "pairwise"),
                                     by = "rep.meas")

all_traits_CD_contrasts <- contrast(blah, 
                                     method = "pairwise",
                                     by = c("rep.meas", "sex"))
plot(all_traits_ssd_contrasts) + 
  geom_vline(xintercept = 0, lty = 2, alpha = 0.5) + 
  labs(x = "log2 change in SSD at HC vs LC", y = "comparison") +
  theme_bw() 

plot(all_traits_CD_contrasts) + 
  geom_vline(xintercept = 0, lty = 2, alpha = 0.5) + 
  labs(x = "log2 change in SSD at HC vs LC", y = "comparison") +
  #facet_grid(vars(sex), vars(rep.meas)) + 
  theme_bw()

##MODEL FOR WING AND LEG COMPARISON

#Loading in data
Dprol_leg_wing <- readRDS("Dprol_leg_wing.rds")

#Fixed effects model
lm_leg_wing <- lm(cbind(leg_log_tibL, leg_log_tibW, leg_log_tar1L, thorax_log_length_mm, wing_log_area_mm_sq) ~ sex*condition,
               data = Dprol_leg_wing)

plot(lm_leg_wing)
influencePlot(lm_leg_wing)
summary(manova(lm_leg_wing))
coefLM <- coef(lm_leg_wing)
confintLM <- confint(lm_leg_wing)

##I'm not sure what this is meant to show, but leaving it in so I have it if needed
dw_wing <- tidy(lm_leg_wing, conf.int = TRUE)

dw_wing  %>%
  filter(term != "(Intercept)") %>%
  #mutate(term = fct_reorder(term, estimate)) %>%
  ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = .2) +
  facet_wrap(~response, scale="free",ncol=2) +
  geom_vline(xintercept=0,lty=2) + 
  xlab("Effect size") + 
  ylab("Treatment")

comparison <- emmeans(lm_leg_wing, specs = ~ condition | rep.meas + sex)

#rot_strips <-   theme_bw() +
  #theme(text = element_text(size = 16),
        #strip.text.y.right = element_text(angle = 0))
#this is just visual stuff that can be fixed later

plot(comparison,
     xlab = "model estimates, trait measurements, log2 transformed")

#not going to keep things pairwise, pairwise is just placeholder for right now
contrast(comparison, "pairwise")

leg_wing_ssd_contrasts <- contrast(comparison, 
                                     interaction = c(condition = "pairwise", sex = "pairwise"),
                                     by = "rep.meas")
leg_wing_ssd_contrasts

leg_wing_CD_contrasts <- contrast(comparison, 
                                    method = "pairwise",
                                    by = c("rep.meas", "sex"))

leg_wing_CD_contrasts

confint(leg_wing_ssd_contrasts)

plot(leg_wing_ssd_contrasts) + 
  geom_vline(xintercept = 0, lty = 2, alpha = 0.5) + 
  labs(x = "change in SSD at HC and LC (log2)", y = "comparison") +
  theme_bw() + theme(text = element_text(size = 16))

#non-pairwise stuff
comparison_contrasts_ratios <- contrast(comparison, 
                                        interaction = c(condition = "eff", sex = "pairwise"),
                                        by = "rep.meas",type = "response")

rot_strips <-   theme_bw() +
  theme(text = element_text(size = 12),
        strip.text.y.right = element_text(angle = 0))

plot(comparison_contrasts_ratios) + 
  geom_vline(xintercept = 0, lty = 2, alpha = 0.5) + 
  labs(x = "change in SSD at HC and LC (log2)", y = "comparison") +
  theme_bw() + theme(text = element_text(size = 16)) +
  rot_strips
