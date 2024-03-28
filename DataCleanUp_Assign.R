## Load the tidyverse packages 
library(tidyverse)

##loading the CSV file into R and assigning the data frame to variable "size_data"
size_data <- read_csv("MP_SpeciesStarvation_Clean.csv")

## exploring the data set (content, classes, structure, column names)
summary (size_data)
str(size_data)
names(size_data)

#renaming "...1" column to "observationsNum" 
size_data <- (size_data 
              %>% rename("observationsNum" = "...1"))
names(size_data)

##looking for missing values
sum(is.na(size_data))

##converting all character vectors and 'cohort' column to factors
## JD: Worth finding more efficient code for this part
## We had talked about mutate_if, but I think BB has a better method
size_data <- (size_data
              %>% mutate(species_full=as.factor(species_full), 
                         species_strain=as.factor(species_strain), 
                         block=as.factor(block), 
                         sex=as.factor(sex),
                         condition=as.factor(condition), 
                         cohort=as.factor(cohort)))
summary(size_data)

#figuring out the difference between cohort and cohort_num 
head(select(size_data, cohort, cohort_num))
tail(select(size_data, cohort, cohort_num)) #cohort and cohort_num are identical. Cohort_num should be removed from the data frame.              
## JD: YOu can use identical() or all.equal() to confirm something like ths

#removing cohort_num from the data frame 
size_data <- (size_data
              %>% select(-cohort_num))
names(size_data)

## JD is Num_Species really a good name for this variable?
#counting number of observations per species. Looking for species with few observations. 
species_count <- (size_data
      %>% count(species_full, name="Num_Species")
      %>% arrange(Num_Species))
print(species_count, n = 15)
species_mean <- mean(species_count$Num_Species)
ggplot(data = species_count, aes(x=species_full, y =Num_Species)) + 
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 90)) +
  geom_hline(yintercept=species_mean,colour="red")

## JD: Would the plot be easier to read with coord_flip()
## Is there a better way to order the species, instead of alphabetically?

#the number of observations for D_orena is much smaller relative to the other species in the data set
#Because I am interested in comparing observations between species, I think including a species with very few observations would make for non-meaningful comparisons
#Therefore, I will remove D_orena from the data frame due to its small sample size (n = 13)

##Removing D_orena
size_data <- (size_data 
              %>% filter(species_full != "D_orena"))
summary(size_data$species_full) 
#removing D_orena as a level in Species_full 
size_data$species_full <- droplevels(size_data$species_full)
levels(size_data$species_full)

#removing D.orena1.1 and D.orena1.3 as a level in species_strain
summary(size_data$species_strain)
size_data$species_strain <- droplevels(size_data$species_strain)
levels(size_data$species_strain)

#subsetting the data frame to to organize/visualize the data in a manner that corresponds to how I plan to analyze the data   
#making sure that the species cohorts correspond to the correct condition level. 
#cohort one should correspond to high condition (HC); the highest cohort for each species should correspond to low condition(LC)
print(size_data
          %>% select(species_full, cohort, condition)
          %>%count(species_full, cohort, condition))

##Making different tables that report wing, leg, and thorax per species at the different condition levels
#I am interested in looking at how wing, leg, and thorax measurements are affected by condition. I intend to compare the relationship betweeen species.
names(size_data)

#wing area 

Wing_size_mean <- (size_data
      %>% group_by(species_full, condition, sex, observationsNum)
      %>% summarise(m_wing_area_sq = mean(wing_area_mm_sq), 
                    m_wing_log_area_mm_sq = mean(wing_log_area_mm_sq), 
                    m_wing_sqroot_area_mm_sq = mean(wing_sqroot_area_mm_sq), 
                    m_wing_log_sqroot_area_mm_sq = mean(wing_log_sqroot_area_mm_sq)))

#tibia length and width 
TibiaMean <- (size_data
      %>%group_by(species_full, condition, sex)
      %>%summarise(mean_leg_tibL = mean(leg_tibL), mean_leg_tibW = mean(leg_tibW),
                   mean_leg_log_tibL = mean(leg_log_tibL), mean_leg_log_tibW = mean(leg_log_tibW)))


#tarsus length 
TarsusMean <- (size_data
       %>%group_by(species_full, condition, sex)
       %>%summarise(mean_leg_tar1L = mean(leg_tar1L),
                    mean_leg_log_tar1L = mean(leg_log_tar1L)))

#thorax length thorax_length_mm
ThoraxMean <- (size_data
      %>%group_by(species_full, condition,  sex)
      %>%summarise(mean_thorax_length_mm = mean(thorax_length_mm),
                   mean_thorax_log_length_mm = mean(thorax_log_length_mm)))

#save the clean data set as an RDS file 
saveRDS(size_data, file = "SpeciesStarvation_Cleaned_MA.rds")

