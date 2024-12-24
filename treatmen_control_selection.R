load("D:/MICS/Thesis/wm_hh_2006.RData")

wm_hh_2006 %>% 
  select(WM_wealth_level, R_treatment) %>% 
  tbl_summary(by = R_treatment)

wm_hh_2006 <- wm_hh_2006 %>%
  mutate(R_location = case_when(
    WM_urban %in% c(2, 3, 4) ~ 1,
    WM_urban %in% c(1, 5) ~ 2,
    TRUE ~ NA_integer_  # Ensures any unmatched values are set to NA
  ))

wm_hh_2006_1 <- wm_hh_2006 %>% 
  filter(wage==1|wage==2)

wm_hh_2006_1 %>% 
  select(R_WM_edlevel, R_treatment) %>% 
  tbl_summary(by = R_treatment)


wm_hh_2006_1 %>% 
  select(R_HH_edu, R_treatment) %>% 
  tbl_summary(by = R_treatment)

wm_hh_2006 %>% 
  select(R_WM_religion, R_treatment) %>% 
  tbl_summary(by = R_treatment)

wm_hh_2006 %>% 
  select(HH_sex, R_treatment) %>% 
  tbl_summary(by = R_treatment)

wm_hh_2006 %>% 
  select(R_HH_wall, R_treatment) %>% 
  tbl_summary(by = R_treatment)

chi_square_result <- wm_hh_2006_1 %>%
  select(R_HH_wall, R_treatment) %>%
  table() %>%
  chisq.test()

# Print chi-square test result
print(chi_square_result)

wm_hh_2006 %>% 
  select(R_HH_roof, R_treatment) %>% 
  tbl_summary(by = R_treatment)

chi_square_result <- wm_hh_2006 %>%
  select(R_HH_edu, R_treatment) %>%
  table() %>%
  chisq.test()
print(chi_square_result)

mean_marriage_age <- wm_hh_2006_1 %>% 
  select(WM_marriage_age, R_treatment) %>%
  group_by(R_treatment) %>%
  summarize(mean_marriage_age = mean(WM_marriage_age, na.rm = TRUE))

t_test_result <- t.test(HH_sleep_room ~ R_treatment, data = wm_hh_2006_1, na.rm = TRUE)

print(t_test_result)

sleep <- wm_hh_2006 %>% 
  select(HH_sleep_room, R_treatment) %>%
  group_by(R_treatment) %>%
  summarize(mean_sleep = mean(HH_sleep_room, na.rm = TRUE))

size <- wm_hh_2006 %>% 
  select(HH_size, R_treatment) %>%
  group_by(R_treatment) %>%
  summarize(mean_size = mean(HH_size, na.rm = TRUE))
