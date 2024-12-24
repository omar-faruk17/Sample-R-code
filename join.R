rm(list=ls())

library(tidyverse)
library(haven)
library(gtsummary)
setwd("D:/MICS")

# Load 2006 hh1 file
load("D:/MICS/Bangladesh MICS 2006 SPSS Datasets/hh_r_1.RData")

hh1_2006 <- hh1 %>% 
  select(HH1, HH2, urban = HH6, division = HH7, district = HH7A,
         source_water = WS1, sleeping_room = HC2, material_floor = HC3, 
         material_roof = HC4, material_wall = HC5, electricity = HC9A,
         dwelling_ownership = HC15A, sex_of_head = hhsex, 
         education_of_head = helevel, wealth_score = wlthscor, 
         wealth_quantile = wlthind5, religion_of_head = HC1A) %>% 
  mutate(year = "2006")

# Load hl dataset (2006)
load("D:/MICS/Bangladesh MICS 2006 SPSS Datasets/hl.RData")

hl_2006 <- hl %>% 
  select(HH1, HH2, age_of_head = HL5, HL3, HL1) %>%
  group_by(HH1, HH2) %>% 
  mutate(HH_size = length(unique(HL1))) %>% 
  ungroup()

hl_2006 <- hl_2006 %>% 
  filter(HL3 ==1) %>% 
  select(-HL3, -HL1)

# Adding the age_of_head and HH_size variable in the hh1_2006 dataset

hh_2006 <- merge(hh1_2006, hl_2006, by = c("HH1", "HH2"), all.x = TRUE, all.y = FALSE)

# Load 2012-13 hh1 file
load("D:/MICS/Bangladesh_MICS_12_13_Datasets/hh_r_1.RData")

hh1_2012 <- hh1 %>% 
  select(HH1, HH2, urban = HH6, division = HH7, district = HH7A,
         source_water = WS1, sleeping_room = HC2, material_floor = HC3, 
         material_roof = HC4, material_wall = HC5, electricity = HC8A,
         dwelling_ownership = HC10, sex_of_head = HHSEX, 
         education_of_head = helevel, wealth_score = wscore, 
         wealth_quantile = windex5, religion_of_head = religion) %>% 
  mutate(year = "2012")

# Load hl dataset (2012)
load("D:/MICS/Bangladesh_MICS_12_13_Datasets/hl_r.RData")

hl_2012 <- hl %>% 
  select(HH1, HH2, age_of_head = HL6, HL3, HL1) %>%
  group_by(HH1, HH2) %>% 
  mutate(HH_size = length(unique(HL1))) %>% 
  ungroup()

hl_2012 <- hl_2012 %>% 
  filter(HL3 ==1) %>% 
  select(-HL3, -HL1)

# Adding the age_of_head and HH_size variable in the hh1_2006 dataset

hh_2012 <- merge(hh1_2012, hl_2012, by = c("HH1", "HH2"), all.x = TRUE, all.y = FALSE)


# Convert labelled variables to regular numeric variables
hh_2006_numeric <- data.frame(lapply(hh_2006, as.numeric))
hh_2012_numeric <- data.frame(lapply(hh_2012, as.numeric))

# Combine the datasets
combined_data <- rbind(hh_2006_numeric, hh_2012_numeric)

# Creating a new variable affected which has two values.
# 1 refers to the districts are affected by either sidr or aila. Otherwise zero.
combined_data$affected <- ifelse(combined_data$district %in% c(4,
              42, 1, 78, 79, 35, 6, 86, 47, 9, 54, 87, 75, 15, 51), 1, 0)

combined_data$year <- as.character(combined_data$year)
combined_data$material_roof <- as.character(combined_data$material_roof)
combined_data$material_floor <- as.character(combined_data$material_floor)
combined_data$material_wall <- as.character(combined_data$material_wall)
combined_data$sex_of_head <- as.character(combined_data$sex_of_head)
combined_data$electricity <- as.character(combined_data$electricity)
combined_data$source_water <- as.character(combined_data$source_water)
combined_data$district <- as.character(combined_data$district)
combined_data$religion_of_head <- as.character(combined_data$religion_of_head)
combined_data$religion_of_head <- as.character(combined_data$religion_of_head)

str(combined_data)

combined_data <- combined_data %>% 
  mutate(year = as.factor(year)) 

#save(combined_data, file = "combined_data.RData")


rm(list=ls())

library(tidyverse)
library(haven)
library(gtsummary)
# Loading combined data
load("D:/MICS/combined_data.RData")

str(combined_data)

combined_data <- combined_data %>% 
  mutate(affected = as.factor(affected)) 

combined_data <- combined_data %>% 
  mutate(sleeping_room_per_HHsize = sleeping_room/HH_size)

model <- lm(sleeping_room_per_HHsize ~ year + affected + year*affected, data = combined_data)

summary(model)

model1 <- lm(age_of_head ~ year + affected + year*affected, data = combined_data)

summary(model1)

model2 <- lm(HH_size ~ year + affected + year*affected, data = combined_data)

summary(model2)

model3 <- lm(wealth_score ~ year + affected + year*affected, data = combined_data)

summary(model3)



