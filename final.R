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
         WM_ever_school = WB3, WM_edlevel = WB4, WM_religion = religion,
         WM_ever_birth = CM1, WM_1st_birth_M = CM2M, WM_1st_birth_Y = CM2Y,
         WM_current_contraception = CP2, WM_ever_contraception = CP2A,
         WM_marriage_cmc = WDOM, WM_1st_birth_cmc = WDOBFC)


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

# Load hh_2012 file
load("D:/MICS/Thesis/hh_2012_r.RData")

hh_2012 <- hh_2012 %>% 
  select(HH1, HH2, HH_roof = HC4, HH_wall = HC5, HH_sleep_room = HC2, HH_sex = HHSEX, 
         HH_size = HH11, HH_edu = helevel,WM_wealth_score = wscore, WM_wealth_level = windex5)

hh_2012 <- hh_2012 %>% 
  mutate(UID = paste(HH1, HH2, sep ="_"))

# Joining hh and wm table
wm_hh_2012 <- merge(wm_2012_1, hh_2012, by = "UID", all.x = TRUE, all.y = FALSE)

wm_hh_2012 <- wm_hh_2012 %>%
  mutate(R_HH_edu = case_when(
    HH_edu == 1 |HH_edu == 6 |HH_edu == 9 | is.na(HH_edu) ~ "Pre Primary or None",
    HH_edu == 2 | HH_edu == 3 ~ "Primary",
    HH_edu == 4 | HH_edu == 5  ~ "Secondary/Higher Secondary",
    TRUE ~ NA))

wm_hh_2012 %>% 
  select(R_HH_edu) %>% 
  tbl_summary()

wm_hh_2012 <- wm_hh_2012 %>%
  mutate(R_HH_wall = case_when(
    HH_wall %in% c(31, 32, 33, 34) ~ "Cement or Bricks",
    is.na(HH_wall) ~ "Other Materials",
    TRUE ~ "Other Materials"
  ))

wm_hh_2012 <- wm_hh_2012 %>%
  mutate(R_HH_roof = case_when(
    HH_roof %in% c(33, 34, 35, 36) ~ "Cement",
    is.na(HH_roof) ~ "Other Materials",
    TRUE ~ "Other Materials"
  ))

wm_hh_2012 <- wm_hh_2012 %>%
  mutate(R_WM_religion = case_when(
    WM_religion == 1 ~ "Islam",
    WM_religion == 2 ~ "Hindu",
    is.na(WM_religion) ~ "Other Religion",
    TRUE ~ "Other Religion"
  ))

wm_hh_2012 %>% 
  select(R_HH_wall) %>% 
  tbl_summary()

wm_hh_2012 %>% 
  select(R_HH_roof) %>% 
  tbl_summary()

#save(wm_hh_2012, file = "wm_hh_2012.RData")

rm(list=ls())
# Load wm 2006 file
load("D:/MICS/Thesis/wm_2006_r.RData")

wm_2006_1 <- wm_2006 %>% 
  select(HH1, HH2, WM_line = LN, WM_birthyear = WM8Y, interview_year = WM6Y, WM_birthmonth = WM8M,
         WM_age = WM9, WM_first_union_year = MA6Y, WM_first_union_month = MA6M,
         WM_marriage_age = agem,
         WM_district = HH7A, WM_urban = HH6, wage, wdob,
         WM_ever_school = WM10, WM_edlevel = WM11,
         HH_size = HH11
         )

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


# Load hh 2006 table
load("D:/MICS/Thesis/hh_2006_r.RData")
hh_2006 <- hh_2006 %>% 
  select (HH1, HH2, HH_sex = hhsex, HH_edu = helevel,WM_wealth_score = wlthscor,
          WM_wealth_level = wlthind5, WM_religion = HC1A, HH_roof = HC4,
          HH_wall = HC5, HH_sleep_room = HC2)

hh_2006 <- hh_2006 %>% 
  mutate(UID = paste(HH1, HH2, sep ="_"))

hh_2006 <- hh_2006 %>% 
  mutate(R_WM_religion = case_when(
    WM_religion == 1 ~ "Islam",
    WM_religion == 2 ~ "Hindu",
    is.na(WM_religion) ~ "Other Religion",
    TRUE ~ "Other Religion"
  ))

hh_2006 %>% 
  select(R_WM_religion) %>% 
  tbl_summary()

hh_2006 <- hh_2006 %>%
  mutate(R_HH_edu = case_when(
    HH_edu == 1 |HH_edu == 6 |HH_edu == 9 | is.na(HH_edu) ~ "Pre Primary or None",
    HH_edu == 2 | HH_edu == 3 ~ "Primary",
    HH_edu == 4 | HH_edu == 5  ~ "Secondary/Higher Secondary",
    TRUE ~ NA))

hh_2006 %>% 
  select(R_HH_edu) %>% 
  tbl_summary()

hh_2006 <- hh_2006 %>%
  mutate(R_HH_wall = case_when(
    HH_wall %in% c(31, 33) ~ "Cement or Bricks",
    is.na(HH_wall) ~ "Other Materials",
    TRUE ~ "Other Materials"
  ))

hh_2006 <- hh_2006 %>%
  mutate(R_HH_roof = case_when(
    HH_roof %in% c(34, 35) ~ "Cement",
    is.na(HH_roof) ~ "Other Materials",
    TRUE ~ "Other Materials"
  ))



# Joining hh and wm table
wm_hh_2006 <- merge(wm_2006_1, hh_2006, by = "UID", all.x = TRUE, all.y = FALSE)

#save(wm_hh_2006, file = "wm_hh_2006.RData")





rm(list=ls())
# set directory
setwd("D:/MICS/Thesis")

library(tidyverse)
library(haven)
library(gtsummary)
# Load wm_hh 2006 file
load("D:/MICS/Thesis/wm_hh_2006.RData")

wm_hh_2006 <- wm_hh_2006 %>% 
  filter(wage ==1 | wage==2)

# Load wm_hh 2012 file
load("D:/MICS/Thesis/wm_hh_2012.RData")
wm_hh_2012 <- wm_hh_2012 %>%
  rename(wage = WAGE)

wm_hh_2012 <- wm_hh_2012 %>% 
  filter(wage ==1 | wage==2)

# Combing  datasets
library(dplyr)
combined <- bind_rows(wm_hh_2006, wm_hh_2012)

#...Creating Year variable where 1 = 2006 and 2 = 2012 and 2013
#.......................................................................
combined <- combined %>% 
  mutate(before_after = ifelse(interview_year == 2006, 1, 2))

combined %>% 
  select(before_after) %>% 
  tbl_summary()

# 1 = urban and 2 = rural
combined <- combined %>%
  mutate(R_location = case_when(
    before_after == 1 & WM_urban %in% c(2, 3, 4) ~ 1,
    before_after == 1 & WM_urban %in% c(1, 5) ~ 2,
    before_after == 2 & WM_urban == 1 ~ 1,
    before_after == 2 & WM_urban == 2 ~ 2))


combined %>% 
  select(R_location,WM_urban,interview_year) %>% 
  View()

combined <- combined %>% 
  filter(!is.na(R_treatment))

combined <- combined %>% 
  mutate(before_after = as.factor(before_after)) 
combined$before_after <- relevel(combined$before_after,ref="1")

combined <- combined %>% 
  mutate(R_location = as.factor(R_location)) 
combined$R_location <- relevel(combined$R_location,ref="1")

combined <- combined %>% 
  mutate(R_treatment = as.factor(R_treatment)) 
combined$R_treatment <- relevel(combined$R_treatment,ref="2")

combined <- combined %>% 
  mutate(R_WM_edlevel = as.factor(R_WM_edlevel))

combined$R_WM_edlevel <- relevel(combined$R_WM_edlevel,ref="Pre Primary or None")

combined <- combined %>% 
  mutate(WM_wealth_level = as.factor(WM_wealth_level))

combined$WM_wealth_level <- relevel(combined$WM_wealth_level,ref="1")


combined <- combined %>% 
  mutate(event_age = ifelse(R_ever_married == 1, WM_marriage_age, WM_age))

combined %>% 
  select(WM_marriage_age, WM_age, R_ever_married, event_age) %>% 
  View()

combined <- combined %>% 
  mutate(event = ifelse(R_ever_married == 2, 0, R_ever_married))

combined %>% 
  select(R_ever_married, event) %>% 
  View()

combined <- combined %>% 
  mutate(R_HH_wall = as.factor(R_HH_wall))
combined$R_HH_wall <- relevel(combined$R_HH_wall,ref="Other Materials")

combined <- combined %>% 
  mutate(R_HH_roof = as.factor(R_HH_roof))
combined$R_HH_roof <- relevel(combined$R_HH_roof,ref="Other Materials")

combined <- combined %>% 
  mutate(R_HH_edu = as.factor(R_HH_edu))
combined$R_HH_edu <- relevel(combined$R_HH_edu,ref="Pre Primary or None")

combined <- combined %>% 
  mutate(R_WM_religion = as.factor(R_WM_religion))
combined$R_WM_religion <- relevel(combined$R_WM_religion,ref="Islam")

combined <- combined %>% 
  mutate(HH_sleep_room = ifelse(HH_sleep_room ==99, 1, HH_sleep_room))

combined <- combined %>%
  mutate(wealth_index = case_when(
    WM_wealth_level %in% c(1, 2) ~ "Poor",
    WM_wealth_level == 3 ~ "Medium",
    TRUE ~ "Rich"
  ))
combined <- combined %>% 
  mutate(wealth_index = as.factor(wealth_index))
combined$wealth_index <- relevel(combined$wealth_index,ref="Poor")

combined <- combined %>% 
  mutate(roof_and_wall = case_when(
    R_HH_roof == "Other Materials" & R_HH_wall == "Other Materials" ~ 1,
    R_HH_roof == "Other Materials" & R_HH_wall == "Cement or Bricks" ~ 2,
    R_HH_roof == "Cement" & R_HH_wall == "Cement or Bricks" ~ 3,
    TRUE ~ 2
  ))

combined %>% 
  select(roof_and_wall, R_HH_roof, R_HH_wall) %>% 
  View()


combined <- combined %>% 
  mutate(roof_and_wall = as.factor(roof_and_wall))
combined$roof_and_wall <- relevel(combined$roof_and_wall,ref="1")

library(survival)
# Fit Cox proportional hazards model
cox_model <- coxph(Surv(event_age, event) ~ before_after + R_treatment + 
                      before_after * R_treatment,data = combined)

summary(cox_model)

cox_model1 <- coxph(Surv(event_age, event) ~ before_after + R_treatment + 
                     before_after * R_treatment+ wealth_index+ R_WM_edlevel+ 
                     R_HH_edu+ R_WM_religion,data = combined)

summary(cox_model1)

cox_model2 <- coxph(Surv(event_age, event) ~ before_after + R_treatment + 
                      before_after * R_treatment+ R_WM_edlevel+ 
                      R_HH_edu+ R_WM_religion+roof_and_wall,data = combined)

summary(cox_model2)

library(stargazer)
stargazer(cox_model, cox_model1, cox_model2, 
          type = "text", 
          title = "Cox Proportional Hazards Models")
# Logit

combined1 <- combined %>%
  mutate(logit_m_1 = case_when(
    is.na(WM_marriage_age) ~ 0,
    WM_marriage_age <= 18 ~ 1,
    TRUE ~ 0))
combined1 %>% 
  select(WM_marriage_age, logit_m_1) %>% 
  View()

combined1 <- combined1 %>% 
  mutate(logit_m_1 = as.factor(logit_m_1))


# Logit  model
logit <- glm(logit_m_1 ~ before_after + R_treatment + before_after * R_treatment, 
                    data = combined1, family = binomial)

# Summary of the model
summary(logit)


logit1 <- glm(logit_m_1 ~ before_after + R_treatment + before_after * R_treatment + wealth_index+
                R_WM_edlevel+ 
                R_HH_edu+ R_WM_religion, 
             data = combined1, family = binomial)

# Summary of the model
summary(logit1)


# Create a summary table of the two models
stargazer(logit, logit1, 
          type = "text", 
          title = "1st Logistic Regression Models",
          column.labels = c("Model 1", "Model 2"),
          omit.stat = c("LL", "ser", "f"))


#
combined2 <- combined %>%
  filter(!is.na(WM_marriage_age)) %>% 
  mutate(logit_m_2 = ifelse( WM_marriage_age < 18, 1, 0))

combined2 <- combined2 %>% 
  mutate(logit_m_2 = as.factor(logit_m_2))

m2 <- glm(logit_m_2 ~ before_after + R_treatment + before_after * R_treatment, 
             data = combined2, family = binomial)

# Summary of the model
summary(m2)

m2_a <- glm(logit_m_2 ~ before_after + R_treatment + before_after*R_treatment+ wealth_index
              +R_WM_edlevel+R_WM_religion+R_HH_edu, data = combined2, family = binomial)

summary(m2_a)

# Create a summary table of the two models
stargazer(m2, m2_a, 
          type = "text", 
          title = "2nd Logistic Regression Models",
          column.labels = c("Model 1", "Model 2"),
          omit.stat = c("LL", "ser", "f"))


# OLS: For household size

model_ols <- lm(HH_size ~ before_after + R_treatment + before_after * R_treatment, data = combined)

summary(model_ols)

model_ols_1 <- lm(HH_size ~ before_after + R_treatment + before_after * R_treatment+wealth_index
                  +R_WM_religion+R_HH_edu+R_location, data = combined)

summary(model_ols_1)


# Ols: For sleeping Room
model_ols_s <- lm(HH_sleep_room ~ before_after + R_treatment + before_after * R_treatment, data = combined)
summary(model_ols_s)

model_ols_s_1 <- lm(HH_sleep_room ~ before_after + R_treatment + before_after * R_treatment+
                      HH_size + roof_and_wall+R_location+R_HH_edu, data = combined)
summary(model_ols_s_1)

model_ols_s2 <- lm(HH_sleep_room ~ before_after + R_treatment + before_after * R_treatment+
                      HH_size + wealth_index+R_location+R_HH_edu, data = combined)
summary(model_ols_s2)

# Create a summary table of the two models
stargazer(model_ols_s_1, model_ols_s2, 
          type = "text", 
          title = "OLS Regression Models",
          column.labels = c("Model 1", "Model 2"),
          omit.stat = c("LL", "ser", "f"))

#Wealth_score
wealth <- lm(WM_wealth_score ~ before_after + R_treatment + before_after * R_treatment +
               R_WM_religion + R_HH_edu + R_location, data = combined)

summary(wealth)

#Education
combined <- combined %>% 
  mutate(R_WM_edlevel = as.factor(R_WM_edlevel))

library(nnet)
model <- multinom(R_WM_edlevel ~ before_after + R_treatment + before_after * R_treatment + wealth_index
                  + R_HH_edu+R_location, data = combined)

summary(model)

library(stargazer)
stargazer(model, type = "text", 
          title = "Multinomial Logistic Regression Results",
          out = "model_results.txt")

model2 <- multinom(R_WM_edlevel ~ before_after + R_treatment + before_after * R_treatment + roof_and_wall
                  + R_HH_edu+R_location, data = combined)

summary(model2)

library(stargazer)
stargazer(model2, type = "text", 
          title = "Multinomial Logistic Regression Results",
          out = "model_results.txt")

# Wealth
model1 <- multinom(wealth_index ~ before_after + R_treatment + before_after * R_treatment
                  + R_WM_religion+ R_HH_edu+R_location, data = combined)

model2 <- multinom(wealth_index ~ before_after + R_treatment + before_after * R_treatment
                   + R_HH_edu+R_location, data = combined)

summary(model1)
stargazer(model1, model2, type = "text", 
          title = "(Wealth) Multinomial Logistic Regression Results",
          out = "model_results.txt")



# Roof and wall

model5 <- multinom(roof_and_wall ~ before_after + R_treatment + before_after * R_treatment
                    +R_HH_edu, data = combined)

summary(model5)


stargazer( model5,type = "text", 
          title = "(Roof and wall) Multinomial Logistic Regression Results",
          out = "model_results.txt")




##########################
women_age_cohort1 <- combined %>% 
  filter(wage==1)

# Fit Cox proportional hazards model
cox_model <- coxph(Surv(event_age, event) ~ before_after + R_treatment + 
                     before_after * R_treatment,data = women_age_cohort1)

summary(cox_model)

cox_model1 <- coxph(Surv(event_age, event) ~ before_after + R_treatment + 
                      before_after * R_treatment + wealth_index + R_WM_edlevel,data = women_age_cohort1)

summary(cox_model1)

cox_model2 <- coxph(Surv(event_age, event) ~ before_after + R_treatment + 
                      before_after * R_treatment+ wealth_index+ R_WM_edlevel+ 
                      R_HH_edu,data = women_age_cohort1)

summary(cox_model2)

cox_model2 <- coxph(Surv(event_age, event) ~ before_after + R_treatment + 
                      before_after * R_treatment+ roof_and_wall+ R_WM_edlevel+ 
                      R_HH_edu,data = women_age_cohort1)

summary(cox_model2)


combined1 <- combined1 %>%
  filter(wage==1)

combined2 <- combined2 %>%
  filter(wage==1)

# Logit  model
logit_cohort <- glm(logit_m_1 ~ before_after + R_treatment + before_after * R_treatment, 
             data = combined1, family = binomial)

# Summary of the model
summary(logit_cohort)


logit1_cohort <- glm(logit_m_1 ~ before_after + R_treatment + before_after * R_treatment + WM_wealth_level+
                R_WM_edlevel+ 
                R_HH_edu+ R_WM_religion, 
              data = combined1, family = binomial)

# Summary of the model
summary(logit1_cohort)

# Logit  model
logit_cohort <- glm(logit_m_2 ~ before_after + R_treatment + before_after * R_treatment, 
                    data = combined2, family = binomial)

# Summary of the model
summary(logit_cohort)


logit1_cohort <- glm(logit_m_2 ~ before_after + R_treatment + before_after * R_treatment + roof_and_wall+
                       R_WM_edlevel+ 
                       R_HH_edu, 
                     data = combined2, family = binomial)

# Summary of the model
summary(logit1_cohort)
