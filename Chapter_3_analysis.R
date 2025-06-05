#### Honours Chapter 3 #### 

#### Descriptive statistics of sawfish rescues ####

setwd("C:/Users/samue/Desktop/Honours")

sawfish_all <- read.csv("Kyne_Daly River Pristis pristis data_SamAminiHonours.csv")

rescues <- sawfish_all %>%
  dplyr::filter(Sawfish_rescue == "Yes")

# Abundance #

abundance <- table(rescues$Rescue_year); abundance
avg_abund <- mean(abundance); avg_abund
std_abund <- sd(abundance); std_abund

# Length # 

summary(rescues$TL_mm)
avg_length <- mean(rescues$TL_mm, na.rm = T); avg_length
std_length <- sd(rescues$TL_mm, na.rm = T); std_length

# Sex Ratio #

Sex_data <- sawfish_all %>%
  group_by(Rescue_year, Sex) %>%
  count(Sex)

sawfish_all %>%
  summarise(
    male_count = sum(Sex == "M", na.rm = TRUE),
    female_count = sum(Sex == "F", na.rm = TRUE),
    sex_ratio = female_count/ male_count
  )

## 2024 SR 

sex_yr <- sawfish_all %>%
  filter(Rescue_year == "2024") %>%
  summarise(
    male_count = sum(Sex == "M", na.rm = TRUE),
    female_count = sum(Sex == "F", na.rm = TRUE),
    sex_ratio =  female_count/male_count
  )

sex_yr

#### Wet Season Descriptive Statistics ####

setwd("C:/Users/samue/Desktop/Honours/Chapter_3_Sawfish_rescue_modelling/Daly_Env_Vars")

df <- read.csv("seasonal_predictors_silo.csv", stringsAsFactors = T)

glimpse(df)

# Rainfall #

tot_rain <- df %>%
  dplyr::select(Wet_Season, sum_wetseason_rainfall_mm)

summary(tot_rain)
sd(tot_rain$sum_wetseason_rainfall_mm)
