#Packages
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
#Dprol_size <- (Dprol_size %>% mutate_if(is.character, as.factor)
               #%>% mutate(cohort=as.factor(cohort)))


#Removing variables: transformed values, cohort_num
#Dprol_size_full <- select(Dprol_size, -leg_log_tibL, -leg_log_tibW, -leg_log_tar1L, -thorax_log_length_mm, -wing_log_area_mm_sq, -wing_log_sqroot_area_mm_sq, -wing_sqroot_area_mm_sq,  -cohort_num)
#str(Dprol_size_full)

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

<<<<<<< HEAD
pairs( Dprol_size[Dprol_size$sex == "M",c(8:10, 15)])
pairs( Dprol_size[Dprol_size$sex == "F",c(8:10, 15)])

=======
>>>>>>> 4b26b0949cddc728c73110082a1a8f265c3eb183

#For multivariate leg model: converting data frame to long format

#Removing variables: transformed values, cohort_num, wing values
#Dprol_size_full <- select(Dprol_size, -leg_log_tibL, -leg_log_tibW, -leg_log_tar1L, -thorax_log_length_mm, -wing_area_mm_sq, -wing_log_area_mm_sq, -wing_log_sqroot_area_mm_sq, -wing_sqroot_area_mm_sq,  -cohort_num)
#str(Dprol_size_full)

#Converting leg measurements to micrometers, and log2 transforming

#DO NOT RUN 
#Dprol_trait_full <- select(Dprol_size, leg_tibL, leg_tibW, leg_tar1L, thorax_length_mm, species_full, sex, specimen, condition) 

#dim(Dprol_trait_full)
#log2(Dprol_size$leg_tibL*1000)

#DO NOT RUN 
#Dprol_trait_full$tibL_log2 <- (log2((Dprol_trait_full[,"leg_tibL"])*1000))
#Dprol_trait_full$tibW_log2 <- (log2((Dprol_trait_full[,"leg_tibW"])*1000))
#Dprol_trait_full$tar1L_log2 <- (log2((Dprol_trait_full[,"leg_tar1L"])*1000))
#Dprol_trait_full$thoraxl_log2 <- (log2((Dprol_trait_full[,"thorax_length_mm"])*1000))



#DO NOT RUN
#Dprol_long <- (Dprol_size
               #%>% select(leg_tibL, leg_tibW, leg_tar1L, thorax_length_mm, species_full, sex, specimen, condition) 
               #%>% gather(trait,value, c(leg_tibL , leg_tibW, leg_tar1L, thorax_length_mm)))


#DO NOT RUN
#Dprol_long_dummy <- (Dprol_trait_full[,5:12] 
                    # %>% mutate(units=factor(1:n()))
                    # %>% gather(trait,value, c(tibL_log2, tibW_log2, tar1L_log2, thoraxl_log2)))

#leg and body size wide 
Dprol_wide_dummy <- (Dprol_size
                     %>%select(leg_tibL, leg_tibW, leg_tar1L, thorax_length_mm, species_full, sex, specimen, condition)
                     %>% mutate_if(is.character, as.factor))

Dprol_wide_dummy$tibL_log2 <- (log2((Dprol_wide_dummy[,"leg_tibL"])*1000))
Dprol_wide_dummy$tibWL_log2<- (log2((Dprol_wide_dummy[,"leg_tibW"])*1000))
Dprol_wide_dummy$tar1L_log2 <- (log2((Dprol_wide_dummy[,"leg_tar1L"])*1000))
Dprol_wide_dummy$thoraxl_log2 <- (log2((Dprol_wide_dummy[,"thorax_length_mm"])*1000))

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
Dprol_wing_leg <- select(Dprol_size, leg_tibL, leg_tibW, leg_tar1L, thorax_length_mm, wing_area_mm_sq, wing_log_area_mm_sq, wing_log_sqroot_area_mm_sq, species_full, cohort, sex, specimen, condition)

#Converting length and area measurements to micrometers and log2 transforming
Dprol_wing_leg$tibL_log2 <- (log2((Dprol_wing_leg[,"leg_tibL"])*1000))
Dprol_wing_leg$tibW_log2 <- (log2((Dprol_wing_leg[,"leg_tibW"])*1000))
Dprol_wing_leg$tar1L_log2 <- (log2((Dprol_wing_leg[,"leg_tar1L"])*1000))
Dprol_wing_leg$thoraxl_log2 <- (log2((Dprol_wing_leg[,"thorax_length_mm"])*1000))
Dprol_wing_leg$wing_area_mcm_sq <- ((Dprol_wing_leg[,"wing_area_mm_sq"])*1000000)

<<<<<<< HEAD
saveRDS(Dprol_size, "Dprol_size.rds") # full D. prolongata data 
saveRDS(Dprol_long_dummy, "Dprol_long_dummy.rds") # long D. prolongata data frame - leg and thorax measurements only
saveRDS(Dprol_wide_dummy, "Dprol_wide_dummy.rds") #wide D. prolongata data frame - leg and thorax measurements only 
=======
saveRDS(Dprol_wing_leg, "Dprol_wing_leg.rds") #D. prolongata data frame - leg, thorax, and wing measurements




>>>>>>> 4b26b0949cddc728c73110082a1a8f265c3eb183
