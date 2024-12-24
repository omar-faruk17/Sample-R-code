rm(list=ls())

# set directory
setwd("D:/MICS/Thesis")

library(tidyverse)
library(haven)
library(gtsummary)



# Load wm 2012 file
load("D:/MICS/Thesis/wm_2012_r.RData")


wm_2012_1 <- wm_2012 %>% 
  select(HH1, HH2, WM_line = LN, WM_birthyear = WB1Y, interview_year = WM6Y, WM_birthmonth = WB1M,
         WM_age = WB2, WM_first_union_year = MA8Y, WM_first_union_month = MA8M,
         WM_marriage_age = WAGEM, WM_district = HH7A, WM_urban = HH6, WDOB, WAGE,
         WM_ever_school = WB3, WM_edlevel = WB4, WM_religion = religion, WM_wealth_score = wscore,
         WM_wealth_level = welevel)


wm_2012_1 <- wm_2012_1 %>% 
  mutate(UID = paste(HH1, HH2, sep ="_"))

wm_2012_1 <- wm_2012_1 %>% 
  mutate(UID_1 = paste(UID,WM_line, sep ="_"))

wm_2012_1 <- wm_2012_1 %>% 
  select(UID_1, UID, everything())
# 59,599 Observations


#...Creating new variable ever_married (1 = yes, 2 = No)
wm_2012_1 <- wm_2012_1%>% 
  mutate(R_ever_married = ifelse(is.na(WM_marriage_age), 2, 1))

wm_2012_1 %>% 
  select(R_ever_married, WM_marriage_age) %>% 
  View()

sum(is.na(wm_2012_1$WM_marriage_age))
# 15276 girls are unmarried, 44,323 are married women.

wm_2012_1 %>% 
  select(WM_edlevel,WM_ever_school) %>% 
  tbl_summary(by = WM_ever_school)


wm_2012_1 <- wm_2012_1 %>%
  mutate(R_WM_edlevel = case_when(
      WM_edlevel == 0 | is.na(WM_edlevel) ~ "Pre Primary or None",
      WM_edlevel == 1 ~ "Primary",
      WM_edlevel == 2 ~ "Secondary/Higher Secondary",
      WM_edlevel == 3 ~ "Higher",
      TRUE ~ NA))

wm_2012_1 %>% 
  select(R_WM_edlevel) %>% 
  tbl_summary()


wm_2012_1 <- wm_2012_1 %>% 
  mutate(R_treatment = case_when(
    WM_district %in% c(1, 4, 6, 47, 54, 78, 86) ~ 1, 
    WM_district %in% c(65, 41, 29, 55,18, 82, 57, 50, 76, 88) ~ 2,        
    TRUE ~ NA                                           
  ))

#child_marriage_2012 <- wm_2012_1 %>% 
  filter(WAGE==1) %>%
  filter(WDOB > 1140)
  

#child_marriage_2012 %>%
  filter(R_treatment==2| is.na(R_treatment)) %>%
  mutate(marriage_age_group = case_when(
    is.na(WM_marriage_age) ~ "Never Married",
    WM_marriage_age <= 18 ~ "Child Marriage"
  )) %>%
  select(marriage_age_group) %>%
  tbl_summary()

cohort_2012 <- wm_2012_1 %>% 
  filter(WAGE ==1 | WAGE==2)
  

# Load wm 2006 file
load("D:/MICS/Thesis/wm_2006_r.RData")

wm_2006_1 <- wm_2006 %>% 
  select(HH1, HH2, WM_line = LN, WM_birthyear = WM8Y, interview_year = WM6Y, WM_birthmonth = WM8M,
         WM_age = WM9, WM_first_union_year = MA6Y, WM_first_union_month = MA6M,
         WM_marriage_age = agem,
         WM_district = HH7A, WM_urban = HH6, wage, wdob,
         WM_ever_school = WM10, WM_edlevel = WM11, WM_religion = HC1A,
         WM_wealth_score = wlthscor,
         WM_wealth_level = wlthind5)

wm_2006_1 <- wm_2006_1 %>% 
  mutate(UID = paste(HH1, HH2, sep ="_"))

wm_2006_1 <- wm_2006_1 %>% 
  mutate(UID_1 = paste(UID,WM_line, sep ="_"))

wm_2006_1 <- wm_2006_1 %>% 
  select(UID_1, UID, everything())
# 78260 Observations


#...Creating new variable ever_married (1 = yes, 2 = No)
wm_2006_1 <- wm_2006_1%>% 
  mutate(R_ever_married = ifelse(is.na(WM_marriage_age), 2, 1))

wm_2006_1 %>% 
  select(R_ever_married, WM_marriage_age) %>% 
  View()

sum(is.na(wm_2006_1$WM_marriage_age))
# 19583 girls are unmarried, 58,677 are married women.

wm_2006_1 %>% 
  select(WM_edlevel,WM_ever_school) %>% 
  tbl_summary(by = WM_ever_school)

wm_2006_1 <- wm_2006_1 %>%
  mutate(R_WM_edlevel = case_when(
    WM_edlevel == 0 | is.na(WM_edlevel) | WM_edlevel ==6 | WM_edlevel==9  ~ "Pre Primary or None",
    WM_edlevel == 1 ~ "Primary",
    WM_edlevel == 2 ~ "Secondary/Higher Secondary",
    WM_edlevel == 3 ~ "Higher",
    TRUE ~ NA))

wm_2006_1 %>% 
  select(R_WM_edlevel) %>% 
  tbl_summary()

wm_2006_1 <- wm_2006_1 %>% 
  mutate(R_treatment = case_when(
    WM_district %in% c(1, 4, 6, 47, 54, 78, 86) ~ 1, 
    WM_district %in% c(65, 41, 29, 55,18, 82, 57, 50, 76, 88) ~ 2,        
    TRUE ~ NA                                           
  ))

cohort_2006 <- wm_2006_1 %>% 
  filter(wage ==1 | wage==2)

#child_marriage_2006 <- wm_2006_1 %>% 
  filter(wage==1) %>% 
  filter(wdob > 1063)


#child_marriage_2006 %>%
  filter(R_treatment==2 | is.na(R_treatment)) %>%
  mutate(marriage_age_group = case_when(
    is.na(WM_marriage_age) ~ "Never Married",
    WM_marriage_age <= 18 ~ "Child Marriage"
  )) %>%
  select(marriage_age_group) %>%
  tbl_summary()



# Combing wm_2006_1 and wm_2012_1  datasets
library(dplyr)
wm_combined <- bind_rows(cohort_2006, cohort_2012)

#...Creating Year variable where 1 = 2006 and 2 = 2012 and 2013
#.......................................................................
wm_combined <- wm_combined %>% 
  mutate(before_after = ifelse(interview_year == 2006, 1, 2))

wm_combined %>% 
  select(before_after) %>% 
  tbl_summary()

# Save the wm_combined file
#save(wm_combined, file = "wm_combined_r.RData")

rm(list=ls())
#Load the wm_combined table
load("D:/MICS/Thesis/wm_combined_r.RData")

library(tidyverse)
library(haven)
library(gtsummary)

# Creating affected and partially or not affected dummy variable; 1 = treatment and 2 =control
#........................................
# Treatment Group : Barguna (4), Bagerhat (1), Barisal (6), Khulna (47), Madaripur (54),
# Patuakhali (78), Shariatpur (86)
# Control Group: Bhola (9), Gopalganj (35), Jalokhati (42), Laxmipur (51), Pirojpur (79)


# 1 = urban and 2 = rural
wm_combined <- wm_combined %>%
  mutate(R_location = case_when(
    before_after == 1 & WM_urban %in% c(2, 3, 4) ~ 1,
    before_after == 1 & WM_urban %in% c(1, 5) ~ 2,
    before_after == 2 & WM_urban == 1 ~ 1,
    before_after == 2 & WM_urban == 2 ~ 2))


wm_combined %>% 
  select(R_location,WM_urban,interview_year) %>% 
  View()


wm_combined <- wm_combined %>% 
  mutate(before_after = as.factor(before_after)) 
wm_combined$before_after <- relevel(wm_combined$before_after,ref="1")

wm_combined <- wm_combined %>% 
  mutate(R_location = as.factor(R_location)) 
wm_combined$R_location <- relevel(wm_combined$R_location,ref="1")

wm_combined <- wm_combined %>% 
  mutate(R_treatment = as.factor(R_treatment)) 
wm_combined$R_treatment <- relevel(wm_combined$R_treatment,ref="2")

wm_combined <- wm_combined %>% 
  mutate(R_WM_edlevel = as.factor(R_WM_edlevel))

wm_combined$R_WM_edlevel <- relevel(wm_combined$R_WM_edlevel,ref="Pre Primary or None")

wm_combined <- wm_combined %>% 
  mutate(WM_wealth_level = as.factor(WM_wealth_level))

wm_combined$WM_wealth_level <- relevel(wm_combined$WM_wealth_level,ref="1")


wm_combined <- wm_combined %>% 
  mutate(event_age = ifelse(R_ever_married == 1, WM_marriage_age, WM_age))

wm_combined %>% 
  select(WM_marriage_age, WM_age, R_ever_married, event_age) %>% 
  View()

wm_combined <- wm_combined %>% 
  mutate(event = ifelse(R_ever_married == 2, 0, R_ever_married))

wm_combined %>% 
  select(R_ever_married, event) %>% 
  View()


library(survival)

# Fit Cox proportional hazards model
cox_model <- coxph(Surv(event_age, event) ~ before_after + R_treatment + before_after*R_treatment, data = wm_combined)

summary(cox_model)

cox_model1 <- coxph(Surv(event_age, event) ~ before_after + R_treatment + before_after*R_treatment + WM_wealth_level, data = wm_combined)

summary(cox_model1)

cox_model2 <- coxph(Surv(event_age, event) ~ before_after + R_treatment + before_after*R_treatment + WM_wealth_level + R_WM_edlevel, data = wm_combined)

summary(cox_model2)

cox_model3 <- coxph(Surv(event_age, event) ~ before_after + R_treatment + before_after*R_treatment + WM_wealth_level + R_WM_edlevel+ R_location, data = wm_combined)

summary(cox_model3)


wm_combined

model <- glm(event ~ before_after + R_treatment + before_after*R_treatment + WM_wealth_level + R_WM_edlevel+ R_location, 
             data = wm_combined, family = binomial)


summary(model)


#..........................................................................................
#..............................................................................................

wm_combined <- wm_combined %>% 
  mutate(R_treatment_1 = case_when(
             WM_district %in% c(1, 4, 6, 47, 54, 78, 86) ~ 1, 
             WM_district %in% c(65, 41, 29, 55,18, 82, 57, 50, 76, 88) ~ 0,        
             TRUE ~ 0                                           
           )) 

wm_combined <- wm_combined %>% 
  mutate(R_treatment_1 = as.factor(R_treatment_1)) 
wm_combined$R_treatment_1 <- relevel(wm_combined$R_treatment_1,ref="0")

# Fit Cox proportional hazards model
cox_model_1 <- coxph(Surv(event_age, event) ~ before_after + R_treatment_1 + before_after*R_treatment_1 + WM_wealth_level, data = wm_combined)

summary(cox_model_1)

cox_model_2 <- coxph(Surv(event_age, event) ~ before_after + R_treatment_1 + before_after*R_treatment_1 + WM_wealth_level+R_WM_edlevel + R_location, data = wm_combined)

summary(cox_model_2)

#..........................................................................................
#..............................................................................................


library(dplyr)

test <- wm_combined %>%
  filter(!is.na(WM_marriage_age)) 

test%>% 
  filter(before_after == 2) %>%
  group_by(R_treatment) %>%
  summarize(mean_marriage_age = mean(WM_marriage_age))






# Child Marriage

marriage_table %>%
  filter(R_treatment==2 | is.na(R_treatment)) %>% 
  filter(RA_age_of_women >=15 & RA_age_of_women <18) %>%  
  mutate(marriage_age_group = case_when(
    is.na(RA_marriage_age) ~ "Never Married",
    RA_marriage_age <= 18 ~ "Child Marriage"
  )) %>%
  select(marriage_age_group) %>%
  tbl_summary()





child_marriage_2006 %>%
  filter(R_treatment==2 | is.na(R_treatment)) %>%
  mutate(marriage_age_group = case_when(
    is.na(WM_marriage_age) ~ "Never Married",
    WM_marriage_age <= 18 ~ "Child Marriage"
  )) %>%
  select(marriage_age_group) %>%
  tbl_summary()






library(ggplot2)

# Plot histogram with facet_wrap
ggplot(wm_combined %>% filter(!is.na(WM_marriage_age)), aes(x = WM_marriage_age)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of WM_marriage_age",
       x = "WM_marriage_age",
       y = "Frequency") +
  facet_wrap(~ before_after)

library(ggplot2)

library(ggplot2)
histo <- wm_combined %>% 
  filter(!is.na(R_treatment))

# Define custom labels for the facets
facet_labels <- c(`1` = "affected", `2` = "Not Affected")

# Create the plot
ggplot(histo %>% 
         filter(!is.na(WM_marriage_age)) %>% 
         filter(before_after == 2),
       aes(x = WM_marriage_age)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Marriage Age for 2012",
       x = "Marriage Age",
       y = "Frequency") +
  facet_wrap(~R_treatment, labeller = as_labeller(facet_labels))


  
