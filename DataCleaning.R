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
               %>% mutate(cohort=as.factor(cohort)))
str(Dprol_size)
levels(Dprol_size$sex)
levels(Dprol_size$condition)

#Number of observations per sex and condition level
print(Dprol_size %>% count(sex, condition))

#Removing variables: transformed values, cohort_num
#Dprol_size_full <- select(Dprol_size, -leg_log_tibL, -leg_log_tibW, -leg_log_tar1L, -thorax_log_length_mm, -wing_log_area_mm_sq, -wing_log_sqroot_area_mm_sq, -wing_sqroot_area_mm_sq,  -cohort_num)
#str(Dprol_size_full)

##Exploratory plots

#tibia length
print(ggplot(data = Dprol_size, mapping = aes(thorax_length_mm, leg_tibL, colour = sex)) + 
  geom_point() +
  facet_wrap(~condition) + 
  ylab(" Log2 Tibia length (um)"))

#tibia width
print(ggplot(data = Dprol_size, mapping = aes(thorax_length_mm, leg_tibW, colour = sex)) + 
        geom_point() +
        facet_wrap(~condition) + 
        ylab("Tibia width (mm"))

#tarsus length
print(ggplot(data = Dprol_size, mapping = aes(thorax_length_mm, leg_tar1L, colour = sex)) + 
        geom_point() +
        facet_wrap(~condition) + 
        ylab("Tarsus length (mm"))
#wing area 
print(ggplot(data = Dprol_size, mapping = aes(thorax_length_mm, wing_area_mm_sq, colour = sex)) + 
        geom_point() +
        facet_wrap(~condition) + 
        ylab("√wing area"))

#covariance among traits

cov(Dprol_size_full[Dprol_size_full$sex == "M",8:12]) #variance covariance matrix 
cov(Dprol_size_full[Dprol_size_full$sex == "F",8:12])

pairs( Dprol_size_full[Dprol_size_full$sex == "M",8:12])
pairs( Dprol_size_full[Dprol_size_full$sex == "F",8:12])

#For multivariate leg model: converting data frame to long format

#Removing variables: transformed values, cohort_num, wing values
#Dprol_size_full <- select(Dprol_size, -leg_log_tibL, -leg_log_tibW, -leg_log_tar1L, -thorax_log_length_mm, -wing_area_mm_sq, -wing_log_area_mm_sq, -wing_log_sqroot_area_mm_sq, -wing_sqroot_area_mm_sq,  -cohort_num)
#str(Dprol_size_full)

#Converting leg measurements to micrometers, and log2 transforming

Dprol_trait_full <- select(Dprol_size, leg_tibL, leg_tibW, leg_tar1L, thorax_length_mm, species_full, cohort, sex, specimen, condition) 

#log2(Dprol_size$leg_tibL*1000)

Dprol_trait_full$tibL_log2 <- (log2((Dprol_trait_full[,"leg_tibL"])*1000))
Dprol_trait_full$tibW_log2 <- (log2((Dprol_trait_full[,"leg_tibW"])*1000))
Dprol_trait_full$tar1L_log2 <- (log2((Dprol_trait_full[,"leg_tar1L"])*1000))
Dprol_trait_full$thoraxl_log2 <- (log2((Dprol_trait_full[,"thorax_length_mm"])*1000))



Dprol_long <- (Dprol_trait_full[,5:13] 
               %>% gather(trait,value, c(tibL_log2 , tibW_log2, tar1L_log2, thoraxl_log2))
)


Dprol_long_dummy <- (Dprol_trait_full[,5:13] 
                     %>% mutate(units=factor(1:n()))
                     %>% gather(trait,value, c(tibL_log2 , tibW_log2, tar1L_log2, thoraxl_log2))
)

head(Dprol_long_dummy)
Dprol_long_dummy$units

head(Dprol_long)
str(Dprol_long)


saveRDS(Dprol_size, "Dprol_size.rds") # full D. prolongata data frame
saveRDS(Dprol_long, "Dprol_long.rds") # long D. prolongata data frame - leg and thorax measurements only
saveRDS(Dprol_long_dummy, "Dprol_long_dummy.rds")
