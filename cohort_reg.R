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

cohort <- wm_2012_1 %>% 
  mutate(affected = case_when(
    WM_age >= 20 & WM_age <= 23 ~ 1,
    WM_age > 23 & WM_age <= 27 ~ 2,
    TRUE ~ NA_real_
  )) %>%
  mutate(time = case_when(
    WM_age >= 20 & WM_age <= 23 ~ 2,
    WM_age > 23 & WM_age <= 27 ~ 1,
    TRUE ~ NA_real_
  )) %>%
  filter(!is.na(affected))

cohort <- cohort %>% 
  mutate(affected = as.factor(affected)) 
cohort$affected <- relevel(cohort$affected,ref="2")

cohort <- cohort %>% 
  mutate(time = as.factor(time)) 
cohort$time <- relevel(cohort$time,ref="2")

cohort <- cohort %>% 
  mutate(WM_urban = as.factor(WM_urban)) 
cohort$WM_urban <- relevel(cohort$WM_urban,ref="1")


cohort <- cohort %>% 
  mutate(R_WM_edlevel = as.factor(R_WM_edlevel))

cohort$R_WM_edlevel <- relevel(cohort$R_WM_edlevel,ref="Pre Primary or None")

cohort <- cohort %>% 
  mutate(WM_wealth_level = as.factor(WM_wealth_level))

cohort$WM_wealth_level <- relevel(cohort$WM_wealth_level,ref="1")


cohort <- cohort %>% 
  mutate(event_age = ifelse(R_ever_married == 1, WM_marriage_age, WM_age))

cohort %>% 
  select(WM_marriage_age, WM_age, R_ever_married, event_age) %>% 
  View()

cohort <- cohort %>% 
  mutate(event = ifelse(R_ever_married == 2, 0, R_ever_married))

cohort %>% 
  select(R_ever_married, event) %>% 
  View()

library(survival)

# Fit Cox proportional hazards model
cox_model_1 <- coxph(Surv(event_age, event) ~  affected + time +affected*time + WM_wealth_level + WM_urban, data = cohort)

summary(cox_model_1)

cohort_1 <- cohort %>% 
  filter(!is.na(WM_marriage_age))


ols_model <- lm(WM_marriage_age ~ affected +R_WM_edlevel +WM_wealth_level+ WM_urban, data = cohort_1)
summary(ols_model)




cohort2 <- cohort %>%
  mutate(marriage_dummy = ifelse(is.na(WM_marriage_age), 0, 1)) %>%
  mutate(marriage_dummy = as.factor(marriage_dummy))

logistic_model <- glm(marriage_dummy ~ affected + R_WM_edlevel + WM_wealth_level + WM_urban, 
                      data = cohort2,
                      family = binomial)

summary(logistic_model)
