rm(list=ls())

# set directory
setwd("D:/MICS/Thesis")

library(tidyverse)
library(haven)
library(gtsummary)

# Load 2012 table
load("D:/MICS/Thesis/wm_hh_2012.RData")
wm_hh_2012 <- wm_hh_2012 %>% 
  mutate(R_treatment = case_when(
    WM_district %in% c(1, 4, 6, 47, 54, 78, 86) ~ 1, 
    WM_district %in% c(65, 41, 29, 55,18, 82, 57, 50, 76, 88) ~ 2,        
    TRUE ~ NA                                           
  ))

wm_hh_2012_1 <- wm_hh_2012 %>% 
  filter(!is.na(R_treatment))

# CMC of 10 June of 2010 is 1326
wm_hh_2012_1 <- wm_hh_2012_1 %>% 
  mutate(June_2010_to_birthcmc = round((1326 - WDOB) / 12, 1))

wm_hh_2012_1 %>% 
  select(June_2010_to_birthcmc, WM_age) %>% 
  View()

wm_hh_2012_1 %>% 
  select(June_2010_to_birthcmc, WDOB) %>% 
  View()

# Creating affected and not affected flag where 1 means affected and 0 means not affected.
wm_hh_2012_1 <- wm_hh_2012_1 %>%
  filter(!WDOB<968) %>% 
  filter(!WDOB>1158) %>% 
  mutate(flag_affected = case_when(
    WDOB >= 1063 & WDOB <= 1158 ~ 1,
    WDOB >= 968 & WDOB < 1063 ~ 0,
    TRUE ~NA))

wm_hh_2012_1 %>%
  select(flag_affected) %>%
  mutate(flag_affected = factor(flag_affected)) %>%
  tbl_summary()
# 3,893 (52%) are not affected and 3,536 (48%) affected

wm_hh_2012_1 <- wm_hh_2012_1 %>%
  mutate(marriage_age_cmc = WM_marriage_cmc - WDOB) %>% 
  mutate(R_marriage_age = round(marriage_age_cmc / 12, 1)) %>% 
  mutate(child_union_flag_18 = ifelse(R_marriage_age < 18, 1, 0)) %>% 
  mutate(child_union_flag_18 = ifelse(is.na(child_union_flag_18), 0, child_union_flag_18))
# If the women is unmarried then it fall into the zero in child union flag.

wm_hh_2012_1 <- wm_hh_2012_1 %>%
  mutate(child_union_flag_19 = ifelse(R_marriage_age < 19, 1, 0)) %>% 
  mutate(child_union_flag_19 = ifelse(is.na(child_union_flag_19), 0, child_union_flag_19))

wm_hh_2012_1 %>% 
  select(R_marriage_age, child_union_flag_18, child_union_flag_19, WM_marriage_age) %>% 
  View()


patuakhali <- wm_hh_2012_1 %>%
  filter(WM_district==78) 

patuakhali %>% 
  group_by(flag_affected) %>%
  mutate(flag_affected =factor(flag_affected)) %>% 
  mutate(child_union_flag_18 =factor(child_union_flag_18)) %>% 
  select(flag_affected, child_union_flag_18) %>%
  tbl_summary(by = flag_affected)

patuakhali %>% 
  group_by(flag_affected) %>%
  mutate(flag_affected =factor(flag_affected)) %>% 
  mutate(child_union_flag_19 =factor(child_union_flag_19)) %>% 
  select(flag_affected, child_union_flag_19) %>%
  tbl_summary(by = flag_affected)



kushtia <- wm_hh_2012_1 %>%
  filter(WM_district==50) 

kushtia %>% 
  group_by(flag_affected) %>%
  mutate(flag_affected =factor(flag_affected)) %>% 
  mutate(child_union_flag_18 =factor(child_union_flag_18)) %>% 
  select(flag_affected, child_union_flag_18) %>%
  tbl_summary(by = flag_affected)

kushtia %>% 
  group_by(flag_affected) %>%
  mutate(flag_affected =factor(flag_affected)) %>% 
  mutate(child_union_flag_19 =factor(child_union_flag_19)) %>% 
  select(flag_affected, child_union_flag_19) %>%
  tbl_summary(by = flag_affected)



Barguna <- wm_hh_2012_1 %>%
  filter(WM_district==4) 

Barguna %>% 
  group_by(flag_affected) %>%
  mutate(flag_affected =factor(flag_affected)) %>% 
  mutate(child_union_flag_18 =factor(child_union_flag_18)) %>% 
  select(flag_affected, child_union_flag_18) %>%
  tbl_summary(by = flag_affected)

Barguna %>% 
  group_by(flag_affected) %>%
  mutate(flag_affected =factor(flag_affected)) %>% 
  mutate(child_union_flag_19 =factor(child_union_flag_19)) %>% 
  select(flag_affected, child_union_flag_19) %>%
  tbl_summary(by = flag_affected)



jessore <- wm_hh_2012_1 %>%
  filter(WM_district==41) 

jessore %>% 
  group_by(flag_affected) %>%
  mutate(flag_affected =factor(flag_affected)) %>% 
  mutate(child_union_flag_18 =factor(child_union_flag_18)) %>% 
  select(flag_affected, child_union_flag_18) %>%
  tbl_summary(by = flag_affected)

jessore %>% 
  group_by(flag_affected) %>%
  mutate(flag_affected =factor(flag_affected)) %>% 
  mutate(child_union_flag_19 =factor(child_union_flag_19)) %>% 
  select(flag_affected, child_union_flag_19) %>%
  tbl_summary(by = flag_affected)


# 7 affected districts  
wm_hh_2012_1 %>% 
  filter(R_treatment==1) %>% 
  group_by(flag_affected) %>%
  mutate(flag_affected =factor(flag_affected)) %>% 
  mutate(child_union_flag_18 =factor(child_union_flag_18)) %>% 
  select(flag_affected, child_union_flag_18) %>%
  tbl_summary(by = flag_affected)

wm_hh_2012_1 %>% 
  filter(R_treatment==1) %>% 
  group_by(flag_affected) %>%
  mutate(flag_affected =factor(flag_affected)) %>% 
  mutate(child_union_flag_19 =factor(child_union_flag_19)) %>% 
  select(flag_affected, child_union_flag_19) %>%
  tbl_summary(by = flag_affected)



# 10 not affected (control) districts  
wm_hh_2012_1 %>% 
  filter(R_treatment==2) %>% 
  group_by(flag_affected) %>%
  mutate(flag_affected =factor(flag_affected)) %>% 
  mutate(child_union_flag_18 =factor(child_union_flag_18)) %>% 
  select(flag_affected, child_union_flag_18) %>%
  tbl_summary(by = flag_affected)

wm_hh_2012_1 %>% 
  filter(R_treatment==2) %>% 
  group_by(flag_affected) %>%
  mutate(flag_affected =factor(flag_affected)) %>% 
  mutate(child_union_flag_19 =factor(child_union_flag_19)) %>% 
  select(flag_affected, child_union_flag_19) %>%
  tbl_summary(by = flag_affected)



wm_hh_2012_1 <- wm_hh_2012_1 %>% 
  mutate(age_cmc = 1356-WDOB) %>% 
  mutate(R_current_age_cmc = round(age_cmc / 12, 0))


wm_hh_2012_1 %>% 
  select(June_2010_to_birthcmc, child_union_flag_18) %>%
  mutate(June_2010_to_birthcmc = round(June_2010_to_birthcmc)) %>% 
  mutate(June_2010_to_birthcmc = factor(June_2010_to_birthcmc)) %>%
  mutate(child_union_flag_18 = factor(child_union_flag_18)) %>%
  tbl_summary(by = June_2010_to_birthcmc)
  


temp <- table(wm_hh_2012_1$flag_affected, wm_hh_2012_1$child_union_flag_18	)

print(temp)
result <- chisq.test(temp)
print(result)


wm_hh_2012_1 <- wm_hh_2012_1 %>% 
  mutate(child_union_flag_18 = as.factor(child_union_flag_18)) %>% 
  mutate(June_2010_to_birthcmc = as.numeric(as.character(June_2010_to_birthcmc))) %>%
  mutate(June_2010_to_birthcmc = round(June_2010_to_birthcmc)) %>%
  mutate(June_2010_to_birthcmc = as.factor(June_2010_to_birthcmc))

wm_hh_2012_1 <- wm_hh_2012_1 %>% 
  mutate(R_WM_edlevel = as.factor(R_WM_edlevel))
wm_hh_2012_1$R_WM_edlevel <- relevel(wm_hh_2012_1$R_WM_edlevel,ref="Pre Primary or None")

wm_hh_2012_1 <- wm_hh_2012_1 %>% 
  mutate(WM_wealth_level = as.factor(WM_wealth_level))
wm_hh_2012_1$WM_wealth_level <- relevel(wm_hh_2012_1$WM_wealth_level,ref="1")

wm_hh_2012_1 <- wm_hh_2012_1 %>% 
  mutate(R_HH_edu = as.factor(R_HH_edu))
wm_hh_2012_1$R_HH_edu <- relevel(wm_hh_2012_1$R_HH_edu,ref="Pre Primary or None")

wm_hh_2012_1 <- wm_hh_2012_1 %>% 
  mutate(R_WM_religion = as.factor(R_WM_religion))
wm_hh_2012_1$R_WM_religion <- relevel(wm_hh_2012_1$R_WM_religion,ref="Islam")

wm_hh_2012_1 <- wm_hh_2012_1 %>%
  mutate(R_location = ifelse(WM_urban == 1, "Urban", 
                             ifelse(WM_urban == 2, "Rural", NA)))
sum(is.na(wm_hh_2012_1$R_location))

wm_hh_2012_1 <- wm_hh_2012_1 %>% 
  mutate(R_location = as.factor(R_location)) 
wm_hh_2012_1$R_location <- relevel(wm_hh_2012_1$R_location,ref="Urban")


wm_hh_2012_1 <- wm_hh_2012_1 %>% 
  mutate(child_union_flag_18 = as.factor(child_union_flag_18)) 

wm_hh_2012_1 <- wm_hh_2012_1 %>% 
  mutate(R_treatment = as.factor(R_treatment)) 
wm_hh_2012_1$R_location <- relevel(wm_hh_2012_1$R_location,ref="Urban")


wm_hh_2012_1 <- wm_hh_2012_1 %>% 
  mutate(treated = ifelse(R_treatment==1, 1, 0))

wm_hh_2012_1 <- wm_hh_2012_1 %>% 
  mutate(treated = as.factor(treated))
wm_hh_2012_1$treated <- relevel(wm_hh_2012_1$treated,ref="0")

wm_hh_2012_1$time_period = wm_hh_2012_1$flag_affected

wm_hh_2012_1 <- wm_hh_2012_1 %>% 
  mutate(time_period = as.factor(time_period))
wm_hh_2012_1$time_period <- relevel(wm_hh_2012_1$time_period,ref="0")

wm_hh_2012_1 <- wm_hh_2012_1 %>% 
  mutate(June_2010_to_birthcmc = as.factor(June_2010_to_birthcmc))

wm_hh_2012_1 <- wm_hh_2012_1 %>%
  mutate(wealth_index = case_when(
    WM_wealth_level %in% c(1, 2) ~ "Poor",
    WM_wealth_level == 3 ~ "Medium",
    TRUE ~ "Rich"
  ))
wm_hh_2012_1 <- wm_hh_2012_1 %>% 
  mutate(wealth_index = as.factor(wealth_index))
wm_hh_2012_1$wealth_index <- relevel(wm_hh_2012_1$wealth_index,ref="Poor")

model <- glm(child_union_flag_18 ~  treated + time_period+
                treated * time_period+June_2010_to_birthcmc+
                wealth_index+R_WM_edlevel+R_HH_edu+R_location+R_WM_religion, data = wm_hh_2012_1, family = binomial)

summary(model)

model2 <- glm(child_union_flag_18 ~  treated + time_period+
                treated * time_period +June_2010_to_birthcmc+
                WM_wealth_score+R_WM_edlevel+R_HH_edu+R_location+R_WM_religion, data = wm_hh_2012_1, family = binomial)

summary(model2)


wm_hh_2012_1 <- wm_hh_2012_1 %>%
  mutate(women_education = case_when(
    R_WM_edlevel %in% c("Pre Primary or None", "Primary") ~ "Primary or none",
    R_WM_edlevel == "Secondary/Higher Secondary" ~ "Secondary/Higher Secondary",
    TRUE ~ "Higher"
  ))
wm_hh_2012_1 <- wm_hh_2012_1 %>% 
  mutate(women_education = as.factor(women_education))
wm_hh_2012_1$women_education <- relevel(wm_hh_2012_1$women_education,ref="Primary or none")


wm_hh_2012_1 <- wm_hh_2012_1 %>%
  mutate(wealth_index = case_when(
    WM_wealth_level %in% c(1, 2) ~ "Poor",
    WM_wealth_level == 3 ~ "Medium",
    TRUE ~ "Rich"
  ))
wm_hh_2012_1 <- wm_hh_2012_1 %>% 
  mutate(wealth_index = as.factor(wealth_index))
wm_hh_2012_1$wealth_index <- relevel(wm_hh_2012_1$wealth_index,ref="Poor")

wm_hh_2012_1 <- wm_hh_2012_1 %>% 
  mutate(R_HH_wall = as.factor(R_HH_wall))
wm_hh_2012_1$R_HH_wall <- relevel(wm_hh_2012_1$R_HH_wall,ref="Other Materials")

wm_hh_2012_1 <- wm_hh_2012_1 %>% 
  mutate(HH_sleep_room = ifelse(HH_sleep_room ==99, 1, HH_sleep_room))

model3 <- glm(child_union_flag_18 ~  treated + time_period+
                treated * time_period +June_2010_to_birthcmc+
                WM_wealth_score+women_education+R_HH_edu+R_location+R_WM_religion+HH_sleep_room, data = wm_hh_2012_1, family = binomial)

summary(model3)



temp <- table(wm_hh_2012_1$wealth_index, wm_hh_2012_1$women_education	)

print(temp)
result <- chisq.test(temp)
print(result)


summary_df <- wm_hh_2012_1 %>%
  group_by(treated, time_period) %>%
  summarize(mean_HH_sleep_room = mean(HH_sleep_room, na.rm = TRUE), .groups = 'drop')

print(summary_df)

wm_hh_2012_1 %>%
  filter(time_period ==0) %>% 
  select(R_HH_wall, treated) %>% 
  tbl_summary(by = R_HH_wall)
