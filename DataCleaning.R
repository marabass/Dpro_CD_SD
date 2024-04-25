#Packages
library(tidyverse)
library(plyr)

#Load in the data 
DmelFull_size <- read_csv("MP_SpeciesStarvation_Clean.csv")

str(DmelFull_size)
names(DmelFull_size) #Variable names
DmelFull_size$species_full # To identify naming convention for species 

#extracting D prol data 
Dprol_size <- DmelFull_size[DmelFull_size$species_full == "D_prolongata",]
str(Dprol_size)
saveRDS(Dprol_size, "Dprol_size.rds") # full D. prolongata data 

sum(is.na(Dprol_size)) #No missing values 
identical(Dprol_size$cohort_num, Dprol_size$cohort)#cohort_num and cohort are identical


##Exploratory plots

#tibia length
print(ggplot(data = Dprol_size, mapping = aes(thorax_length_mm, leg_tibL, colour = sex)) + 
  geom_point() +
  facet_wrap(~condition) + 
  ylab("Tibia length (mm)")) +
  xlab(("Thorax length (mm)"))

#tibia width
print(ggplot(data = Dprol_size, mapping = aes(thorax_length_mm, leg_tibW, colour = sex)) + 
        geom_point() +
        facet_wrap(~condition) + 
        ylab("Tibia width (mm)") +
        xlab("Thorax length (mm)"))

#tarsus length
print(ggplot(data = Dprol_size, mapping = aes(thorax_length_mm, leg_tar1L, colour = sex)) + 
        geom_point() +
        facet_wrap(~condition) + 
        ylab("Tarsus length (mm)") +
        xlab("Thorax length (mm)"))
#wing area 
print(ggplot(data = Dprol_size, mapping = aes(thorax_length_mm, wing_area_mm_sq, colour = sex)) + 
        geom_point() +
        facet_wrap(~condition) + 
        ylab("√wing area")+
        xlab("Thorax length (mm)"))

#covariance among traits 

cov(Dprol_size[Dprol_size$sex == "M",c(8:10, 15)]) #variance covariance matrix 
cov(Dprol_size[Dprol_size$sex == "F",c(8:10, 15)])

pairs( Dprol_size[Dprol_size$sex == "M",c(8:10, 15)])
pairs( Dprol_size[Dprol_size$sex == "F",c(8:10, 15)])



#leg and body size wide 
Dprol_wide_dummy <- (Dprol_size
                     %>%select(leg_tibL, leg_tibW, leg_tar1L, thorax_length_mm, species_full, sex, condition)
                     %>% mutate_if(is.character, as.factor)
                     %>% mutate_if(is.double, (~.* 1000), round, 4)
                     %>% mutate_if(is.double, log2))

head(Dprol_wide_dummy)
print(Dprol_wide_dummy %>% count(sex, condition))

#leg and body size long 
Dprol_long_dummy <- (Dprol_size
                     %>%select(leg_tibL, leg_tibW, leg_tar1L, thorax_length_mm, species_full, sex, specimen, condition)
                     %>% mutate_if(is.character, as.factor)
                     %>% mutate(units=factor(1:n()))
                     %>% gather(trait,value, c(leg_tibL, leg_tibW, leg_tar1L, thorax_length_mm))
                     %>% mutate(value = log2((value)*1000))
)

head(Dprol_long_dummy)
head(Dprol_long_dummy$units)

#Creating data frame for condition dependence comparison between sexually dimorphic (legs) and non-sexually dimorphic (wings)
Dprol_size$wing_area_mm_sq <- Dprol_size$wing_area_mm_sq*1000
Dprol_leg_wing_mcm <- (Dprol_size
                     %>%select(leg_tibL, leg_tibW, leg_tar1L, thorax_length_mm, wing_area_mm_sq, species_full, sex, condition)
                     %>% mutate_if(is.character, as.factor)
                     %>% mutate_if(is.double, (~.* 1000), round, 4)
                     %>% mutate_if(is.double, log2))
Dprol_leg_wing_mcm <- plyr::rename(Dprol_leg_wing_mcm, c("thorax_length_mm"="thorax_length_µm", "wing_area_mm_sq"="wing_area_µm_sq"))


saveRDS(Dprol_long_dummy, "Dprol_long_dummy.rds") # long D. prolongata data frame - leg and thorax measurements only
saveRDS(Dprol_wide_dummy, "Dprol_wide_dummy.rds") #wide D. prolongata data frame - leg and thorax measurements only 
saveRDS(Dprol_leg_wing_mcm,"Dprol_wing.rds") #D. prolongata data frame for lm adjusted to micrometers and log transformed

