library(tidyverse)

#Load in the data 
DmelFull_size <- read_csv("MP_SpeciesStarvation_Clean.csv")
str(DmelFull_size)
names(DmelFull_size) #Variable names
DmelFull_size$species_full # To identify naming convention for species 

#extracting D prol data 
Dprol_size <- DmelFull_size[DmelFull_size$species_full == "D_prolongata",]
str(Dprol_size)

sum(is.na(Dprol_size)) #No missing values 
identical(Dprol_size$cohort_num, Dprol_size$cohort)#cohort_num and cohort are identical

#convert character vectors and cohort number into factors 
Dprol_size <- (Dprol_size %>% mutate_if(is.character, as.factor) 
               %>% mutate(cohort=as.factor(cohort))
               )
str(Dprol_size)
levels(Dprol_size$sex)

#Number of observations per sex and cohort/condition level
print(Dprol_size %>% count(sex, condition))

# Removing variables: transformed values, cohort_num
Dprol_size_full <- select(Dprol_size, -leg_log_tibL, leg_log_tibW, -leg_log_tar1L, -thorax_log_length_mm, -wing_log_area_mm_sq, -wing_log_sqroot_area_mm_sq, -wing_sqroot_area_mm_sq,  -cohort_num)
str(Dprol_size_full)

#For multivariate leg model: converting to long data frame
Dprol_leg_full <- select(Dprol_size_full, -wing_area_mm_sq) 

#log2(Dprol_size$leg_tibL*1000)

Dprol_leg_full$tibL_log2 <- (log2((Dprol_trait_full[,"leg_tibL"])*1000))
Dprol_leg_full$tibW_log2 <- (log2((Dprol_trait_full[,"leg_tibW"])*1000))
Dprol_leg_full$tar1L_log2 <- (log2((Dprol_trait_full[,"leg_tar1L"])*1000))
Dprol_leg_full$thoraxl_log2 <- (log2((Dprol_trait_full[,"thorax_length_mm"])*1000))

Dprol_long <- (Dprol_leg_full[,5:13] 
               %>% gather(trait,value, c(tibL_log2 , tibW_log2, tar1L_log2, thoraxl_log2))
)

head(Dprol_long)
str(Dprol_long)

#separate trait values by sex - Might take out, not sure we need this 
Dprol_NoCondition <- Dprol_size[,-19]
Dprol_male <- Dprol_NoCondition[Dprol_NoCondition$sex == "M",8:20,]
Dprol_female <- Dprol_NoCondition[Dprol_NoCondition$sex == "F",8:20,]


saveRDS(Dprol_size, "Dprol_size.rds") # full wide D. prolongata data frame
saveRDS(Dprol_long, "Dprol_long.rds") # fill long D. prolongata data frame