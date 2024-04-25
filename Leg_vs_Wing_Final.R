##MODEL FOR WING AND LEG COMPARISON

library(performance)
library(tidyverse)
library(emmeans)
library(dotwhisker)
library(lme4)
library(mvinfluence)

#Loading in data
Dprol_leg_wing <- readRDS("Dprol_wing.rds")

#Fixed effects model
lm_leg_wing <- lm(cbind(leg_tibL, leg_tibW, leg_tar1L, thorax_length_µm, wing_area_µm_sq) ~ sex*condition,
                  data = Dprol_leg_wing)

#diagnostic
qqnorm(residuals(lm_leg_wing))
qqline(residuals(lm_leg_wing))

summary(lm_leg_wing)
influencePlot(lm_leg_wing)
summary(manova(lm_leg_wing))
coef_leg_wing <- coef(lm_leg_wing)
confint_leg_wing <- confint(lm_leg_wing)

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

rot_strips <-   theme_bw() +
theme(text = element_text(size = 10),
strip.text.y.right = element_text(angle = 0))
#this is just visual stuff that can be fixed later

custom_labels1 <- as_labeller(function(x){
  return(paste0(c("Tarsus Length", "Tibia Length", "Tibia Width", "Thorax Length", "Wing Area")))
}) ##Trait labels for plots 

#non-pairwise stuff
comparison_contrasts_ratios <- contrast(comparison, 
                                        interaction = c(condition = "eff", sex = "pairwise"),
                                        by = "rep.meas",type = "response")

plot(comparison_contrasts_ratios) + 
  geom_vline(xintercept = 0, lty = 2, alpha = 0.5) + 
  labs(x = "log2 change in SSD at HC and LC", y = "comparison") +
  facet_wrap(~ rep.meas, labeller = custom_labels1, ncol = 1, strip.position = "right")
  theme_bw() + theme(text = element_text(size = 16)) +
  rot_strips
  