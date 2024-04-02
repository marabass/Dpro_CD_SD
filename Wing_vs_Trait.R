##Setting up everything we need
library(lme4)
library(Matrix)
library(ggplot2)
library(tidyverse)
library(dotwhisker)
library(performance)

Dprol_size <- readRDS("Dprol_size.rds")

Dprol_size$sex <- as.factor(Dprol_size$sex)
Dprol_size$condition <- as.factor(Dprol_size$condition)
Dprol_size$block <- as.factor(Dprol_size$block)
Dprol_size$cohort  <- as.factor(Dprol_size$cohort)
Dprol_size$specimen <- as.factor(Dprol_size$specimen)

##Getting relevant traits and their transformations
Dprol_wing_body <- select(Dprol_size, species_full, species_strain, block, cohort, sex, specimen, cohort_num, condition, leg_tibL, leg_tibW, leg_tar1L, thorax_length_mm, wing_area_mm_sq) 

Dprol_wing_body$log2tibL <- log2(as.numeric(Dprol_wing_body$leg_tibL))
Dprol_wing_body$log2tibW <- log2(as.numeric(Dprol_wing_body$leg_tibW))
Dprol_wing_body$log2tar1L <- log2(as.numeric(Dprol_wing_body$leg_tar1L))
Dprol_wing_body$log2thoraxL <- log2(as.numeric(Dprol_wing_body$thorax_length_mm))
Dprol_wing_body$log2wing_area <- log2(as.numeric(Dprol_wing_body$wing_area_mm_sq))

lm_wing <- lm(log2wing_area ~ sex * condition, data = Dprol_wing_body)
plot(lm_wing)
check_model(lm_wing)
confint(lm_wing)

wing_HC_F <-  Dprol_wing_body %>%
  group_by(sex, condition) %>%
  summarize(mean(wing_area_mm_sq))

wing_F <- Dprol_wing_body %>%
  group_by(sex) %>%
  summarize(mean(wing_area_mm_sq))