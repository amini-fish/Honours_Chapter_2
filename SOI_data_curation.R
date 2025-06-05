#### Load Southern Oscilation Index ####

SOI_full <- read.table("C:/Users/samue/Desktop/SOI_data.txt", header = TRUE, fill = TRUE, strip.white = TRUE)

# Check the data 

print(SOI_full)

# Convert from wide format to long format 

SOI_long <- SOI_full %>%
  pivot_longer(
    cols = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"), 
    names_to = "Month",
    values_to = "SOI_value")

# Save the long format data to csv - add in Wet Season manually

#write.csv(SOI_long, "SOI_clean.csv")

# Calculate the average SOI from the clean data

SOI_average <- SOI_long %>%
  group_by(Seaon) %>% ## typo lol 
  summarise(
    wet_season_SOI = mean(SOI_value, na.rm = T))

