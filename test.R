rm(list=ls())

library(tidyverse)
library(haven)
library(gtsummary)

# Load 2012-13 hh1 file
load("D:/MICS/Bangladesh_MICS_12_13_Datasets/hh_r_1.RData")

hh1_2012 <- hh1

# Load 2006 hh1 file
load("D:/MICS/Bangladesh MICS 2006 SPSS Datasets/hh_r_1.RData")

hh1_2006 <- hh1

# figure out the z value of quantile5 for 2012-13 dataset
hh1_2012 <- hh1_2012 %>% 
  select(HH1, HH2, wscore) 

hh1_2012$z_score <- scale(hh1_2012$wscore)


# figure out the z value of quantile5 for 2012-13 dataset
hh1_2006 <- hh1_2006 %>% 
  select(HH1, HH2, wlthscor) 

hh1_2006$z_score <- scale(hh1_2006$wlthscor)



# Combine datasets
combined_data <- rbind(
  transform(hh1_2012$z_score , dataset = "hh1_2012"),
  transform(hh1_2006$z_score , dataset = "hh1_2006")
)

# Plot z-scores for each dataset separately
ggplot(combined_data, aes(x = dataset, y = X_data, fill = dataset)) +
  geom_boxplot() +
  labs(title = "Z-Scores of Common Variable",
       x = "Dataset",
       y = "Z-Score")


# Plot histograms of z-scores for each dataset separately
ggplot(combined_data, aes(x = X_data, fill = dataset)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  facet_wrap(~ dataset, ncol = 1) +
  labs(title = "Distribution of Z-Scores",
       x = "Z-Score",
       y = "Frequency")



# quantile observation calculating
#..................................................

rm(list=ls())

library(tidyverse)
library(haven)
library(gtsummary)

# Load 2012-13 hh1 file
load("D:/MICS/Bangladesh_MICS_12_13_Datasets/hh_r_1.RData")

hh1_2012 <- hh1

# Load 2006 hh1 file
load("D:/MICS/Bangladesh MICS 2006 SPSS Datasets/hh_r_1.RData")

hh1_2006 <- hh1

hh1_2012 <- hh1_2012 %>% 
  select(HH1, HH2, windex5)

hh1_2006 <- hh1_2006 %>% 
  select(HH1, HH2, wlthind5)


# Poor Observation
#................................................
poor_counts_2012 <- hh1_2012 %>%
  filter(windex5 ==1 | windex5 == 2) %>% 
  count(windex5)
# poor = 25007 in 2012 dataset, total observation = 51895

poor_counts_2006 <- hh1_2006 %>%
  filter(wlthind5 ==1 | wlthind5 == 2) %>% 
  count(wlthind5)
# poor = 26443 in 2006 dataset, total observation = 62463


# middle Observation
#................................................
middle_counts_2012 <- hh1_2012 %>%
  filter(windex5 == 3) %>% 
  count(windex5)
# middle = 10155 in 2012 dataset

middle_counts_2006 <- hh1_2006 %>%
  filter(wlthind5 ==3) %>% 
  count(wlthind5)
# middle = 12854 in 2006 dataset


# quantile_4 Observation
#................................................
quantile_4_counts_2012 <- hh1_2012 %>%
  filter(windex5 == 4) %>% 
  count(windex5)
# quantile_4 = 9258 in 2012 dataset

quantile_4_counts_2006 <- hh1_2006 %>%
  filter(wlthind5 ==4) %>% 
  count(wlthind5)
# quantile_4 = 11954 in 2006 dataset

# quantile_5 Observation
#................................................
quantile_5_counts_2012 <- hh1_2012 %>%
  filter(windex5 == 5) %>% 
  count(windex5)
# quantile_5 = 7475 in 2012 dataset

quantile_5_counts_2006 <- hh1_2006 %>%
  filter(wlthind5 ==5) %>% 
  count(wlthind5)
# quantile_5 = 11212 in 2006 dataset




# Electricity observation calculating
#..................................................

rm(list=ls())

library(tidyverse)
library(haven)
library(gtsummary)

# Load 2012-13 hh1 file
load("D:/MICS/Bangladesh_MICS_12_13_Datasets/hh_r_1.RData")

hh1_2012 <- hh1

# Load 2006 hh1 file
load("D:/MICS/Bangladesh MICS 2006 SPSS Datasets/hh_r_1.RData")

hh1_2006 <- hh1

# Electricity connection in 2006 
#....................................................
hh1_2006 <- hh1_2006 %>% 
  select(HH1, HH2, HC9A)
sum(is.na(hh1_2006$HC9A)) #no NA entries.
sum(hh1_2006$HC9A == 9) #5 entries are missing

sum(hh1_2006$HC9A == 1)
# 30504 entries have electricity connection in 2006.
sum(hh1_2006$HC9A == 2)
# 31954 entries dont have electricity connection in 2006.
# 48.8% household have the electricity.


# Electricity connection in 2012 
#....................................................
hh1_2012 <- hh1_2012 %>% 
  select(HH1, HH2, HC8A)
sum(is.na(hh1_2012$HC8A)) #no NA entries.
sum(hh1_2012$HC8A==9) #5 entries are missing

sum(hh1_2012$HC8A == 1)
# 28941 entries have electricity connection in 2012
sum(hh1_2012$HC8A == 2)
# 22952 entries dont have electricity connection in 2012
# 55.8% entries have the electricity.



# Electricity for treatment variable.
#..........................................................

##......................................................
#...........TREATMENT GROUP (BAGERHAT, BARGUNA, PATUAKHALI)
#...................................................................

rm(list=ls())

library(tidyverse)
library(haven)
library(gtsummary)

# Load 2012-13 hh1 file
load("D:/MICS/Bangladesh_MICS_12_13_Datasets/hh_r_1.RData")

hh1_2012 <- hh1

# Load 2006 hh1 file
load("D:/MICS/Bangladesh MICS 2006 SPSS Datasets/hh_r_1.RData")


#.....................................................
#........Bagerhat..............................
#........................................................
hh1_2006 <- hh1

hh1_2006 <- hh1_2006 %>% 
  select(HH1, HH2, HC9A, HH7A)

electricity_bagerhat_2006 <- sum(ifelse(hh1_2006$HH7A == 1, hh1_2006$HC9A == 1, 0))
total_obsevation_bagerhat_2006 <- sum(hh1_2006$HH7A == 1)
# Bagerhat 44% have the electricity in 2006


hh1_2012 <- hh1_2012 %>% 
  select(HH1, HH2, HC8A, HH7A)

electricity_bagerhat_2012 <- sum(ifelse(hh1_2012$HH7A == 1, hh1_2012$HC8A == 1, 0))
total_obsevation_bagerhat_2012 <- sum(hh1_2012$HH7A == 1)
# Bagerhat 53% have the electricity in 2013



#.....................................................
#........Barguna..............................
#........................................................

electricity_barguna_2006 <- sum(ifelse(hh1_2006$HH7A == 4, hh1_2006$HC9A == 1, 0))
total_obsevation_barguna_2006 <- sum(hh1_2006$HH7A == 4)
# Barguna 25% have the electricity in 2006


electricity_barguna_2012 <- sum(ifelse(hh1_2012$HH7A == 4, hh1_2012$HC8A == 1, 0))
total_obsevation_barguna_2012 <- sum(hh1_2012$HH7A == 4)
# Barguna 48% have the electricity in 2013


#.....................................................
#........Patuakhali..............................
#........................................................

electricity_patuakhali_2006 <- sum(ifelse(hh1_2006$HH7A == 78, hh1_2006$HC9A == 1, 0))
total_obsevation_patuakhali_2006 <- sum(hh1_2006$HH7A == 78)
# Patuakhali 28% have the electricity in 2006


electricity_patuakhali_2012 <- sum(ifelse(hh1_2012$HH7A == 78, hh1_2012$HC8A == 1, 0))
total_obsevation_patuakhali_2012 <- sum(hh1_2012$HH7A == 78)
# Patuakhali 46% have the electricity in 2013






##......................................................
#..CONTROL GROUP (SATKHIRA, KHULNA, GOPALGONJ, BHOLA, JALOKHATI, PIROJPUR, BARISAL)
#...................................................................

# SATKHIRA = 87, KHULNA = 47, PIRUJPUR = 79, GOPALGONJ = 35,
# Bhola = 9, JALOKHATI = 42, BARISAL = 6


#.....................................................
#........SATKHIRA..............................
#........................................................

electricity_satkhira_2006 <- sum(ifelse(hh1_2006$HH7A == 87, hh1_2006$HC9A == 1, 0))
total_obsevation_satkhira_2006 <- sum(hh1_2006$HH7A == 87)
# SATKHIRA 32% have the electricity in 2006


electricity_satkhira_2012 <- sum(ifelse(hh1_2012$HH7A == 87, hh1_2012$HC8A == 1, 0))
total_obsevation_satkhira_2012 <- sum(hh1_2012$HH7A == 87)
# SATKHIRA 49% have the electricity in 2012


#.....................................................
#........KHULNA..............................
#........................................................

electricity_khulna_2006 <- sum(ifelse(hh1_2006$HH7A == 47, hh1_2006$HC9A == 1, 0))
total_obsevation_khulna_2006 <- sum(hh1_2006$HH7A == 47)
# KHULNA 67.6% have the electricity in 2006


electricity_khulna_2012 <- sum(ifelse(hh1_2012$HH7A == 47, hh1_2012$HC8A == 1, 0))
total_obsevation_khulna_2012 <- sum(hh1_2012$HH7A == 47)
# KHULNA 70% have the electricity in 2012


#.....................................................
#........PIRUJPUR..............................
#........................................................

electricity_pirujpur_2006 <- sum(ifelse(hh1_2006$HH7A == 79, hh1_2006$HC9A == 1, 0))
total_obsevation_pirujpur_2006 <- sum(hh1_2006$HH7A == 79)
# PIRUJPUR 43.3% have the electricity in 2006


electricity_pirujpur_2012 <- sum(ifelse(hh1_2012$HH7A == 79, hh1_2012$HC8A == 1, 0))
total_obsevation_pirujpur_2012 <- sum(hh1_2012$HH7A == 79)
# PIRUJPUR 55.5% have the electricity in 2012


#.....................................................
#........GOPALGONJ..............................
#........................................................

electricity_gopalganj_2006 <- sum(ifelse(hh1_2006$HH7A == 35, hh1_2006$HC9A == 1, 0))
total_obsevation_gopalganj_2006 <- sum(hh1_2006$HH7A == 35)
percentage_electricity_gopalganj_2006 <- (electricity_gopalganj_2006 / total_obsevation_gopalganj_2006) * 100

print(percentage_electricity_gopalganj_2006)
# GOPALGONJ 45.25% have the electricity in 2006


electricity_gopalganj_2012 <- sum(ifelse(hh1_2012$HH7A == 35, hh1_2012$HC8A == 1, 0))
total_obsevation_gopalganj_2012 <- sum(hh1_2012$HH7A == 35)
percentage_electricity_gopalganj_2012 <- (electricity_gopalganj_2012 / total_obsevation_gopalganj_2012) * 100

print(percentage_electricity_gopalganj_2012)
# GOPALGONJ 51.96% have the electricity in 2012



#.....................................................
#........Bhola..............................
#........................................................

electricity_Bhola_2006 <- sum(ifelse(hh1_2006$HH7A == 9, hh1_2006$HC9A == 1, 0))
total_obsevation_Bhola_2006 <- sum(hh1_2006$HH7A == 9)
percentage_electricity_Bhola_2006 <- (electricity_Bhola_2006 / total_obsevation_Bhola_2006) * 100

print(percentage_electricity_Bhola_2006)
# Bhola 31.16% have the electricity in 2006


electricity_Bhola_2012 <- sum(ifelse(hh1_2012$HH7A == 9, hh1_2012$HC8A == 1, 0))
total_obsevation_Bhola_2012 <- sum(hh1_2012$HH7A == 9)
percentage_electricity_Bhola_2012 <- (electricity_Bhola_2012 / total_obsevation_Bhola_2012) * 100

print(percentage_electricity_Bhola_2012)
# Bhola 35.21% have the electricity in 2012



#.....................................................
#........JALOKHATI..............................
#........................................................

electricity_JALOKHATI_2006 <- sum(ifelse(hh1_2006$HH7A ==42, hh1_2006$HC9A == 1, 0))
total_obsevation_JALOKHATI_2006 <- sum(hh1_2006$HH7A == 42)
percentage_electricity_JALOKHATI_2006 <- (electricity_JALOKHATI_2006 / total_obsevation_JALOKHATI_2006) * 100

print(percentage_electricity_JALOKHATI_2006)
# JALOKHATI 44.32% have the electricity in 2006


electricity_JALOKHATI_2012 <- sum(ifelse(hh1_2012$HH7A == 42, hh1_2012$HC8A == 1, 0))
total_obsevation_JALOKHATI_2012 <- sum(hh1_2012$HH7A == 42)
percentage_electricity_JALOKHATI_2012 <- (electricity_JALOKHATI_2012 / total_obsevation_JALOKHATI_2012) * 100

print(percentage_electricity_JALOKHATI_2012)
# JALOKHATI 64.08% have the electricity in 2012



#.....................................................
#........BARISAL..............................
#........................................................

electricity_BARISAL_2006 <- sum(ifelse(hh1_2006$HH7A == 6, hh1_2006$HC9A == 1, 0))
total_obsevation_BARISAL_2006 <- sum(hh1_2006$HH7A == 6)
percentage_electricity_BARISAL_2006 <- (electricity_BARISAL_2006 / total_obsevation_BARISAL_2006) * 100

print(percentage_electricity_BARISAL_2006)
# BARISAL 67.61% have the electricity in 2006


electricity_BARISAL_2012 <- sum(ifelse(hh1_2012$HH7A == 6, hh1_2012$HC8A == 1, 0))
total_obsevation_BARISAL_2012 <- sum(hh1_2012$HH7A == 6)
percentage_electricity_BARISAL_2012 <- (electricity_BARISAL_2012 / total_obsevation_BARISAL_2012) * 100

print(percentage_electricity_BARISAL_2012)
# BARISAL 63.19% have the electricity in 2012



##...........................................................................................
# Calculating average room for sleeping.
#..................................................

rm(list=ls())

library(tidyverse)
library(haven)
library(gtsummary)

# Load 2012-13 hh1 file
load("D:/MICS/Bangladesh_MICS_12_13_Datasets/hh_r_1.RData")

hh1_2012 <- hh1 %>% 
  select(HH1, HH2, HC2, HH7A)

# Load 2006 hh1 file
load("D:/MICS/Bangladesh MICS 2006 SPSS Datasets/hh_r_1.RData")

hh1_2006 <- hh1 %>% 
  select(HH1, HH2, HC2, HH7A)



### Treatment Group###
#......................................
barguna_2006 <- hh1_2006 %>% 
  filter(HH7A == 4) %>% 
  summarise(mean_HC2 = mean(HC2))

barguna_2012 <- hh1_2012 %>% 
  filter(HH7A == 4) %>% 
  summarise(mean_HC2 = mean(HC2))


barguna_2006 <- hh1_2006 %>% 
  filter(HH7A == 4) 

barguna_2012 <- hh1_2012 %>% 
  filter(HH7A == 4) 
t_test_result <- t.test(barguna_2006$HC2, barguna_2012$HC2)
t_test_result



bagerhat_2006 <- hh1_2006 %>% 
  filter(HH7A == 1) 

bagerhat_2012 <- hh1_2012 %>% 
  filter(HH7A == 1) 
t_test_result <- t.test(bagerhat_2006$HC2, bagerhat_2012$HC2)
t_test_result

patuakhali_2006 <- hh1_2006 %>% 
  filter(HH7A == 78) 

patuakhali_2012 <- hh1_2012 %>% 
  filter(HH7A == 78) 
t_test_result <- t.test(patuakhali_2006$HC2, patuakhali_2012$HC2)
t_test_result


### Control Group###
#......................................

satkhira_2006 <- hh1_2006 %>% 
  filter(HH7A == 87)

satkhira_2012 <- hh1_2012 %>% 
  filter(HH7A == 87) 
t_test_result <- t.test(satkhira_2006$HC2, satkhira_2012$HC2)
t_test_result


khulna_2006 <- hh1_2006 %>% 
  filter(HH7A == 47) 

khulna_2012 <- hh1_2012 %>% 
  filter(HH7A == 47)
t_test_result <- t.test(khulna_2006$HC2, khulna_2012$HC2)
t_test_result

pirujpur_2006 <- hh1_2006 %>% 
  filter(HH7A == 79)

pirujpur_2012 <- hh1_2012 %>% 
  filter(HH7A == 79)
t_test_result <- t.test(pirujpur_2006$HC2, pirujpur_2012$HC2)
t_test_result


jalokhati_2006 <- hh1_2006 %>% 
  filter(HH7A == 42) 

jalokhati_2012 <- hh1_2012 %>% 
  filter(HH7A == 42) 
t_test_result <- t.test(jalokhati_2006$HC2, jalokhati_2012$HC2)
t_test_result


bhola_2006 <- hh1_2006 %>% 
  filter(HH7A == 9) 

bhola_2012 <- hh1_2012 %>% 
  filter(HH7A == 9) 
t_test_result <- t.test(bhola_2006$HC2, bhola_2012$HC2)
t_test_result

goplaganj_2006 <- hh1_2006 %>% 
  filter(HH7A == 35) 
goplaganj_2012 <- hh1_2012 %>% 
  filter(HH7A == 35) 

t_test_result <- t.test(goplaganj_2006$HC2, goplaganj_2012$HC2)
t_test_result

barisal_2006 <- hh1_2006 %>% 
  filter(HH7A == 6) 
barisal_2012 <- hh1_2012 %>% 
  filter(HH7A == 6)
t_test_result <- t.test(barisal_2006$HC2, barisal_2012$HC2)
t_test_result

#### Neither sidr nor aila ##
#...................................................

madiripur_2006 <- hh1_2006 %>% 
  filter(HH7A == 54) 
madiripur_2012 <- hh1_2012 %>% 
  filter(HH7A == 54) 
t_test_result <- t.test(madiripur_2006$HC2, madiripur_2012$HC2)
t_test_result


narail_2006 <- hh1_2006 %>% 
  filter(HH7A == 65) 

narail_2012 <- hh1_2012 %>% 
  filter(HH7A == 65) 
t_test_result <- t.test(narail_2006$HC2, narail_2012$HC2)
t_test_result


jessore_2006 <- hh1_2006 %>% 
  filter(HH7A == 41) 

jessore_2012 <- hh1_2012 %>% 
  filter(HH7A == 41) 
t_test_result <- t.test(jessore_2006$HC2, jessore_2012$HC2)
t_test_result







#### Main Material for floor (HC3) /roop (HC4) /wall (HC5)


rm(list=ls())

library(tidyverse)
library(haven)
library(gtsummary)

# Load 2012-13 hh1 file
load("D:/MICS/Bangladesh_MICS_12_13_Datasets/hh_r_1.RData")

hh1_2012 <- hh1 %>% 
  select(HH1, HH2, HC2, HH7A, HC3, HC4, HC5)

# Load 2006 hh1 file
load("D:/MICS/Bangladesh MICS 2006 SPSS Datasets/hh_r_1.RData")

hh1_2006 <- hh1 %>% 
  select(HH1, HH2, HC2, HH7A, HC3, HC4, HC5)

# Barguna

hh1_2006 %>% 
  filter(HH7A == 4) %>% 
  tbl_summary()

hh1_2012 %>% 
  filter(HH7A == 4) %>% 
  tbl_summary()


# Barguna

hh1_2006 %>% 
  filter(HH7A == 4) %>% 
  tbl_summary()

hh1_2012 %>% 
  filter(HH7A == 4) %>% 
  tbl_summary()



# Bagerhat

hh1_2006 %>% 
  filter(HH7A == 4) %>% 
  tbl_summary()

hh1_2012 %>% 
  filter(HH7A == 4) %>% 
  tbl_summary()


# Barguna

hh1_2006 %>% 
  filter(HH7A == 1) %>% 
  tbl_summary()

hh1_2012 %>% 
  filter(HH7A == 1) %>% 
  tbl_summary()



# Patuakhali

hh1_2006 %>% 
  filter(HH7A == 78) %>% 
  tbl_summary()

hh1_2012 %>% 
  filter(HH7A == 78) %>% 
  tbl_summary()


# Narail

hh1_2006 %>% 
  filter(HH7A == 65) %>% 
  tbl_summary()

hh1_2012 %>% 
  filter(HH7A == 65) %>% 
  tbl_summary()



# Bagerhat

hh1_2006 %>% 
  filter(HH7A == 4) %>% 
  select(HC4) %>% 
  tbl_summary()

# Barguna

hh1_2006 %>% 
  filter(HH7A == 1) %>% 
  select(HC4) %>% 
  tbl_summary()

# Patuakhali

hh1_2006 %>% 
  filter(HH7A == 78) %>% 
  select(HC4) %>% 
  tbl_summary()


##HC4##

hh1_2006 %>%
  select(HC4, HH7A) %>% 
  filter(HH7A %in% c(4, 1, 78, 87, 47, 79, 42, 9, 35, 6, 54, 65, 41)) %>%
  mutate(HH7A = as.factor(HH7A),
         HC4 = as.factor(HC4)) %>%
  tbl_summary(by = HH7A)


hh1_2012 %>%
  select(HC4, HH7A) %>% 
  filter(HH7A %in% c(4, 1, 78, 87, 47, 79, 42, 9, 35, 6, 54, 65, 41)) %>%
  mutate(HH7A = as.factor(HH7A),
         HC4 = as.factor(HC4)) %>%
  tbl_summary(by = HH7A)


##HC5##

hh1_2006 %>%
  select(HC5, HH7A) %>% 
  filter(HH7A %in% c(4, 1, 78, 87, 47, 79, 42, 9, 35, 6, 54, 65, 41)) %>%
  mutate(HH7A = as.factor(HH7A),
         HC5 = as.factor(HC5)) %>%
  tbl_summary(by = HH7A)


hh1_2012 %>%
  select(HC5, HH7A) %>% 
  filter(HH7A %in% c(4, 1, 78, 87, 47, 79, 42, 9, 35, 6, 54, 65, 41)) %>%
  mutate(HH7A = as.factor(HH7A),
         HC5 = as.factor(HC5)) %>%
  tbl_summary(by = HH7A)


##Number of room for sleeping (HC2)##

hh1_2006 %>%
  select(HC2, HH7A) %>% 
  filter(HH7A %in% c(4, 1, 78, 87, 47, 79, 42, 9, 35, 6, 54, 65, 41)) %>%
  mutate(HH7A = as.factor(HH7A),
         HC2 = as.factor(HC2)) %>%
  tbl_summary(by = HH7A)

