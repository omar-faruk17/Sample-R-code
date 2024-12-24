rm(list=ls())

# set directory
setwd("D:/MICS/Thesis")

library(tidyverse)
library(haven)
library(gtsummary)

# Load 2012 table
load("D:/MICS/Thesis/wm_hh_2012.RData")

wm_hh_2012 <- wm_hh_2012 %>% 
  mutate(roof_and_wall = case_when(
    R_HH_roof == "Other Materials" & R_HH_wall == "Other Materials" ~ 1,
    R_HH_roof == "Other Materials" & R_HH_wall == "Cement or Bricks" ~ 2,
    R_HH_roof == "Cement" & R_HH_wall == "Cement or Bricks" ~ 3,
    TRUE ~ 2
  ))

wm_hh_2012 <- wm_hh_2012 %>% 
  mutate(roof_and_wall = as.factor(roof_and_wall))
wm_hh_2012$roof_and_wall <- relevel(wm_hh_2012$roof_and_wall,ref="1")

wm_hh_2012_1 <- wm_hh_2012 %>% 
  filter(!is.na(R_treatment))

# Contraception Status
#......................................
wm_hh_2012_1 %>% 
  filter(!is.na(WM_ever_contraception)) %>% 
  select(WM_current_contraception, WM_ever_contraception) %>% 
  View()

wm_hh_2012_1 <- wm_hh_2012_1 %>% 
  mutate(R_ever_contraception = case_when(
    (is.na(WM_ever_contraception) & WM_current_contraception == 1) ~ "Yes",
    WM_current_contraception == 1 ~ "Yes",
    WM_ever_contraception == 1 ~ "Yes",
    TRUE ~ "No"
  ))

wm_hh_2012_1 %>%  
  select(R_ever_contraception, WM_current_contraception, WM_ever_contraception) %>% 
  View()


# Marriage and first birth CMC
wm_hh_2012_1 %>% 
  select(WM_marriage_cmc, WM_1st_birth_cmc) %>% 
  View()
# Women giving birth have all marriage cmc.

wm_hh_2012_1 %>% 
  select(WM_marriage_cmc, WM_1st_birth_cmc, WM_ever_birth) %>% 
  filter(WM_1st_birth_cmc < WM_marriage_cmc) %>% 
  View()
# 350 entries are incorrect as the marriage age greater the the year of first birth.

# Creating flag variable for identifying the incorrect entries
wm_hh_2012_1 <- wm_hh_2012_1 %>% 
  mutate(flag = ifelse(WM_1st_birth_cmc < WM_marriage_cmc, 0, 1))
sum(wm_hh_2012_1$flag == 0, na.rm = TRUE)

# birth spacing (Marriage to first birth)
wm_hh_2012_1 <- wm_hh_2012_1 %>% 
  mutate(birth_spacing_cmc = WM_1st_birth_cmc - WM_marriage_cmc)

wm_hh_2012_1 %>% 
  select(birth_spacing_cmc, WM_1st_birth_cmc, WM_marriage_cmc, flag) %>% 
  View()

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


#...Birth Spacing
#...................................................

#birth_spacing_1 <- wm_hh_2012_1 %>% 
  #filter(child_union_flag_18 ==1)


birth_spacing_1 <- wm_hh_2012_1

birth_spacing_1 <- birth_spacing_1 %>% 
  filter(!is.na(R_marriage_age))
  
birth_spacing_1 %>% 
  filter(birth_spacing_cmc <7) %>% 
  select(R_treatment) %>%
  tbl_summary()

# Filtering out the women whose birth cmc is less the 7.
birth_spacing_1 <- birth_spacing_1 %>%
  mutate(birth_spacing_flag = ifelse(birth_spacing_cmc < 7, 1, 0)) 

birth_spacing_1 <- birth_spacing_1 %>%
  filter(is.na(birth_spacing_flag) | birth_spacing_flag == 0)


# leveling the urban/rural variables
birth_spacing_1 <- birth_spacing_1 %>%
  mutate(R_location = ifelse(WM_urban == 1, "Urban", 
                             ifelse(WM_urban == 2, "Rural", NA)))
sum(is.na(birth_spacing_1$R_location))

birth_spacing_1 <- birth_spacing_1 %>% 
  mutate(R_location = as.factor(R_location)) 
birth_spacing_1$R_location <- relevel(birth_spacing_1$R_location,ref="Urban")

birth_spacing_1 <- birth_spacing_1 %>% 
  mutate(flag_affected = as.factor(flag_affected)) 
birth_spacing_1$flag_affected <- relevel(birth_spacing_1$flag_affected,ref="0")

birth_spacing_1 <- birth_spacing_1 %>% 
  mutate(R_treatment = as.factor(R_treatment)) 
birth_spacing_1$R_treatment <- relevel(birth_spacing_1$R_treatment,ref="2")

birth_spacing_1 <- birth_spacing_1 %>% 
  mutate(R_WM_edlevel = as.factor(R_WM_edlevel))
birth_spacing_1$R_WM_edlevel <- relevel(birth_spacing_1$R_WM_edlevel,ref="Pre Primary or None")

birth_spacing_1 <- birth_spacing_1 %>% 
  mutate(WM_wealth_level = as.factor(WM_wealth_level))
birth_spacing_1$WM_wealth_level <- relevel(birth_spacing_1$WM_wealth_level,ref="1")


birth_spacing_1 <- birth_spacing_1 %>% 
  mutate(R_HH_wall = as.factor(R_HH_wall))
birth_spacing_1$R_HH_wall <- relevel(birth_spacing_1$R_HH_wall,ref="Other Materials")

birth_spacing_1 <- birth_spacing_1 %>% 
  mutate(R_HH_edu = as.factor(R_HH_edu))
birth_spacing_1$R_HH_edu <- relevel(birth_spacing_1$R_HH_edu,ref="Pre Primary or None")

birth_spacing_1 <- birth_spacing_1 %>% 
  mutate(R_WM_religion = as.factor(R_WM_religion))
birth_spacing_1$R_WM_religion <- relevel(birth_spacing_1$R_WM_religion,ref="Islam")


#birth_spacing_1 <- birth_spacing_1 %>% 
  #filter(!is.na(birth_spacing_cmc))

# filtering
birth_spacing_1 <- birth_spacing_1 %>% 
  mutate(event_cohort = ifelse(birth_spacing_cmc <=18, 1, 0))

birth_spacing_1 <- birth_spacing_1 %>% 
  mutate(event_cohort = as.factor(event_cohort))

birth_spacing_1 <- birth_spacing_1 %>% 
  mutate(R_ever_contraception = as.factor(R_ever_contraception))
birth_spacing_1$R_ever_contraception <- relevel(birth_spacing_1$R_ever_contraception,ref="No")

regression <- birth_spacing_1 %>% 
  select(UID_1, birth_spacing_cmc, marriage_age_cmc, birth_spacing_flag, R_marriage_age,roof_and_wall,event_cohort, flag_affected, R_treatment, WM_age, R_ever_contraception, WM_wealth_level,R_WM_edlevel, 
         R_location,R_WM_religion,R_HH_edu, WDOB,June_2010_to_birthcmc, WM_first_union_year, R_ever_contraception)


regression <- regression %>% 
  mutate(treated = ifelse(R_treatment==1, 1, 0))

regression <- regression %>% 
  mutate(treated = as.factor(treated))
regression$treated <- relevel(regression$treated,ref="0")

regression$time_period = regression$flag_affected

regression <- regression %>% 
  mutate(time_period = as.factor(time_period))
regression$time_period <- relevel(regression$time_period,ref="0")

regression <- regression %>% 
  mutate(age_cmc = 1356-WDOB) %>% 
  mutate(R_current_age_cmc = round(age_cmc / 12, 1))

regression %>% 
  select(WM_age, R_current_age_cmc) %>% 
  View()

regression %>% 
  select(time_period, June_2010_to_birthcmc, R_current_age_cmc,WM_age) %>%
  View()

regression <- regression %>%
  mutate(June_2010_to_birthcmc = as.numeric(as.character(June_2010_to_birthcmc))) %>%
  mutate(June_2010_to_birthcmc = round(June_2010_to_birthcmc)) %>%
  mutate(June_2010_to_birthcmc = as.factor(June_2010_to_birthcmc))


model <- glm(event_cohort ~  treated + time_period+
               treated * time_period, data = regression, family = binomial)

summary(model)


regression <- regression %>%
  mutate(wealth_index = case_when(
    WM_wealth_level %in% c(1, 2) ~ "Poor",
    WM_wealth_level == 3 ~ "Medium",
    TRUE ~ "Rich"
  ))

regression <- regression %>% 
  mutate(wealth_index = as.factor(wealth_index))
regression$wealth_index <- relevel(regression$wealth_index,ref="Poor")

model1 <- glm(event_cohort ~  treated + time_period+
                treated * time_period + wealth_index+R_HH_edu+R_WM_religion+June_2010_to_birthcmc, data = regression, family = binomial)

summary(model1)


interval_ols <- lm(birth_spacing_cmc ~ treated + time_period+
                     treated * time_period+wealth_index+R_WM_edlevel+R_HH_edu+R_WM_religion, data = regression)

summary(interval_ols)



#load the required package for tobit model
library(AER)

# Assume `regression` is your data frame and `birth_spacing_cmc` is the variable of interest

# Step 1: Replace NA values with the right censoring threshold (195 months)
regression$birth_spacing_cmc[is.na(regression$birth_spacing_cmc)] <- 195

# Step 2: Create an indicator for right censoring
regression$censored <- ifelse(regression$birth_spacing_cmc == 195, 1, 0)

# Step 3: Fit the Tobit model
tobit_model <- tobit(birth_spacing_cmc ~ treated + time_period +
                       treated * time_period + wealth_index + 
                       R_WM_edlevel + R_HH_edu + R_WM_religion + R_ever_contraception, 
                     data = regression, right = 195)  # Using 195 months as the right censoring point

# Summarize the model
summary(tobit_model)





temp <- table(regression$treated, regression$time_period)

print(temp)
result <- chisq.test(temp)


regression <- regression %>% 
  mutate(R_ever_given_birth = ifelse(is.na(birth_spacing_flag), 0, 1))

#regression <- regression %>% 
  #mutate(event_age = ifelse(R_ever_given_birth == 1, birth_spacing_cmc, 1360-marriage_age_cmc))

regression <- regression %>% 
  mutate(event_age = ifelse(R_ever_given_birth == 1, birth_spacing_cmc, (2013-WM_first_union_year)*12))

regression %>% 
  select(birth_spacing_cmc,WM_first_union_year,  R_marriage_age, R_ever_given_birth, event_age) %>% 
  View()

regression <- regression %>% 
  filter(!(WM_first_union_year == 9999 | WM_first_union_year == 9998))


regression <- regression %>% 
  mutate(R_ever_given_birth = as.factor(R_ever_given_birth))


library(survival)

# Fit Cox proportional hazards model
cox_model <- coxph(Surv(event_age, R_ever_given_birth) ~ treated + time_period + 
                     treated * time_period + wealth_index + R_WM_edlevel + 
                     R_location + R_WM_religion + R_HH_edu, data = regression)

summary(cox_model)




###################
# Create the summary table with mean, standard deviation, and p-value for t-test
birth_spacing_1 %>%
  filter(flag_affected == 1) %>%
  select(R_treatment, birth_spacing_cmc) %>%
  tbl_summary(
    by = R_treatment, 
    statistic = list(all_continuous() ~ "{mean} ({sd})")
  ) %>%
  add_p(test = all_continuous() ~ "t.test")
