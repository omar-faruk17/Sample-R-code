combined %>% 
  select(before_after, R_treatment) %>% 
  tbl_summary(by = before_after)

combined %>% 
  filter(before_after==2) %>% 
  select(R_treatment, WM_district) %>% 
  mutate(WM_district= factor(WM_district)) %>% 
  tbl_summary(by = R_treatment)

combined %>% 
  filter(before_after==2) %>% 
  select(R_treatment, R_WM_religion) %>% 
  mutate(R_WM_religion= factor(R_WM_religion)) %>% 
  tbl_summary(by = R_treatment)

combined %>% 
  filter(before_after==2) %>% 
  select(R_treatment, wealth_index) %>% 
  mutate(wealth_index= factor(wealth_index)) %>% 
  tbl_summary(by = R_treatment)

combined %>% 
  filter(before_after==2) %>% 
  select(R_treatment, R_WM_edlevel) %>% 
  mutate(R_WM_edlevel= factor(R_WM_edlevel)) %>% 
  tbl_summary(by = R_treatment)


combined %>% 
  filter(before_after==2) %>% 
  select(R_treatment, roof_and_wall) %>% 
  mutate(roof_and_wall= factor(roof_and_wall)) %>% 
  tbl_summary(by = R_treatment)

marriage_table_marriage_age_cat <- combined %>% 
  filter(before_after==2) %>% 
  select(WM_marriage_age, R_WM_edlevel,R_treatment) %>% 
  mutate(marriage_age_group = case_when(
    is.na(WM_marriage_age) ~ "Never Married",
    WM_marriage_age < 15 ~ "Less than 15 years",
    WM_marriage_age >= 15 & WM_marriage_age <= 17 ~ "15 to 17 years",
    WM_marriage_age > 17 & WM_marriage_age <= 24 ~ "18 to 24 years",
  ))

#...Calculating Distribution  of women for different marriage age group. 
marriage_table_marriage_age_cat %>%
  filter(R_treatment==2) %>% 
  select(marriage_age_group, R_WM_edlevel) %>%
  tbl_summary(by = marriage_age_group)

# Create a contingency table
contingency_table <- table(marriage_table_marriage_age_cat$marriage_age_group, marriage_table_marriage_age_cat$R_WM_edlevel)

# Perform chi-squared test
chi_squared_test <- chisq.test(contingency_table)

# Print the chi-squared test results
print(chi_squared_test)



# Create the summary table with mean, standard deviation, and p-value for t-test
combined %>%
  filter(before_after == 2) %>% 
  select(R_treatment, HH_size) %>%
  tbl_summary(
    by = R_treatment, 
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    type = all_continuous() ~ "continuous"
  ) %>%
  add_p(test = list(all_continuous() ~ "t.test"))

# Create the summary table with mean, standard deviation, and p-value for t-test
combined %>%
  filter(before_after == 2) %>% 
  select(R_treatment, HH_sleep_room) %>%
  tbl_summary(
    by = R_treatment, 
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    type = all_continuous() ~ "continuous"
  ) %>%
  add_p(test = list(all_continuous() ~ "t.test"))


# mean marriaze age 
combined %>%
  filter(before_after == 1) %>% 
  filter(!is.na(WM_marriage_age)) %>% 
  select(R_treatment, WM_marriage_age) %>%
  tbl_summary(
    by = R_treatment, 
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    type = all_continuous() ~ "continuous"
  ) %>%
  add_p(test = list(all_continuous() ~ "t.test"))


# Child marriaze percentage 
combined %>%
  filter(before_after == 2) %>% 
  filter(!is.na(WM_marriage_age)) %>%
  select(R_treatment, WM_marriage_age) %>%
  mutate(marriage_age_group = ifelse(WM_marriage_age < 18, "Child Marriage", "Not Child Marriage")) %>%
  tbl_summary(by = R_treatment)


# wealth vs roof and wall

combined_1 <- combined %>% 
  filter(before_after ==2) %>% 
  filter(R_treatment==2)

# Create a contingency table
contingency_table <- table(combined_1$wealth_index, combined_1$roof_and_wall)

# Perform chi-squared test
chi_squared_test <- chisq.test(contingency_table)

# Print the chi-squared test results
print(chi_squared_test)
  