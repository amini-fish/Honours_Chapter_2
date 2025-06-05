#### Discharge #### 

library(dplyr)
library(lubridate)
library(zoo)

setwd("C:/Users/samue/Desktop/Honours/Chapter_3_Sawfish_rescue_modelling/Daly_Env_Vars")

discharge <- read.csv("C:/Users/samue/Desktop/G8140040_daily_ts_DI.csv", stringsAsFactors = T, header = T)

#discharge <- read.csv("G8140003_Interp_Discharge.csv", stringsAsFactors = T, header = T) ## Data from NTG - no good needs gap fill

print(discharge)

discharge <- discharge %>%
  dplyr::select(Date, Flow..ML.)

#colnames(discharge) <- c("date", "Discharge_ML_day")

discharge

# Ensure Date is in correct format
#discharge$date <- as.POSIXct(discharge$date, format = "%d/%m/%Y %H:%M")

#### Calculate a daily average - do in R ####

av_day_discharge <- discharge %>%
  mutate(date = floor_date(date, unit = "day")) %>%
  group_by(date) %>%
  summarize(mean_discharge = mean(Discharge_ML_day, na.rm = T))


av_day_discharge

## once we have the average daily discharge we then want to interpolate between the missing points

## First we need to add in the missing dates, then order them correctly, and then interpolate using the na.approx function from the R package "zoo"

discharge_full <- av_day_discharge %>%
  complete(date = seq(min(.$date), max(.$date), by = "day")) %>%
  arrange(date) %>%
  mutate(Discharge_ML_day = na.approx(mean_discharge, x = date, na.rm = FALSE))

head(discharge_full)

## Sum these daily averages into wets season totals 

total_discharge_phases_with_gaps <- function(data, date_col, discharge_col) {
  # Convert date column
  data[[date_col]] <- as.Date(data[[date_col]], format = "%d/%m/%Y")  # Adjust format if needed
  
  results <- data.frame(
    wet_season = character(),
    total_discharge = numeric(),
    phase_1_discharge = numeric(),
    phase_2_discharge = numeric(),
    missing_days_total = integer(),
    missing_days_p1 = integer(),
    missing_days_p2 = integer(),
    stringsAsFactors = FALSE
  )
  
  for (year in 2011:2023) {
    # Date ranges
    start_total <- as.Date(paste0("01/11/", year), format="%d/%m/%Y")
    end_total   <- as.Date(paste0("30/04/", year + 1), format="%d/%m/%Y")
    
    start_p1 <- start_total
    end_p1   <- as.Date(paste0("31/01/", year + 1), format="%d/%m/%Y")
    
    start_p2 <- as.Date(paste0("01/02/", year + 1), format="%d/%m/%Y")
    end_p2   <- end_total
    
    # Expected sequences
    full_seq <- seq(start_total, end_total, by = "day")
    p1_seq   <- seq(start_p1, end_p1, by = "day")
    p2_seq   <- seq(start_p2, end_p2, by = "day")
    
    # Filtered data
    dates_in_range <- data[[date_col]]
    
    season_data <- data[data[[date_col]] %in% full_seq, ]
    phase_1_data <- data[data[[date_col]] %in% p1_seq, ]
    phase_2_data <- data[data[[date_col]] %in% p2_seq, ]
    
    # Missing days
    missing_total <- setdiff(full_seq, unique(season_data[[date_col]]))
    missing_p1 <- setdiff(p1_seq, unique(phase_1_data[[date_col]]))
    missing_p2 <- setdiff(p2_seq, unique(phase_2_data[[date_col]]))
    
    # Sum discharges
    total <- sum(season_data[[discharge_col]], na.rm = TRUE)
    phase_1 <- sum(phase_1_data[[discharge_col]], na.rm = TRUE)
    phase_2 <- sum(phase_2_data[[discharge_col]], na.rm = TRUE)
    
    # Combine
    results <- rbind(results, data.frame(
      wet_season = paste0(year, "/", year + 1),
      total_discharge = total,
      phase_1_discharge = phase_1,
      phase_2_discharge = phase_2,
      missing_days_total = length(missing_total),
      missing_days_p1 = length(missing_p1),
      missing_days_p2 = length(missing_p2)
    ))
  }
  return(results)
}

#### Run it 

av_day_discharge$mean_discharge <- as.numeric(av_day_discharge$mean_discharge)

str(av_day_discharge)

total_discharge_phases_with_gaps(av_day_discharge, "date", "Discharge_ML_day")


discharge_calcs <- total_discharge_phases_with_gaps(discharge_full, "date", "Discharge_ML_day")

print(discharge_calcs)

## Sum the daily averages into months - do in Excel

write.csv(discharge_calcs, "discharge_interp.csv")

#### ####

total_discharge_filled <- function(data, date_col, discharge_col) {
  # Convert to Date (remove time)
  data[[date_col]] <- as.Date(data[[date_col]], format = "%d/%m/%Y")
  
  results <- data.frame(
    wet_season = character(),
    total_discharge = numeric(),
    filled_days = integer(),
    stringsAsFactors = FALSE
  )
  
  for (year in 2011:2023) {
    # Define wet season date range
    start_date <- as.Date(paste0("01/12/", year), format = "%d/%m/%Y")
    end_date   <- as.Date(paste0("31/05/", year + 1), format = "%d/%m/%Y")
    
    expected_dates <- seq(start_date, end_date, by = "day")
    
    # Subset for actual data in season
    season_data <- data %>%
      filter(.data[[date_col]] >= start_date & .data[[date_col]] <= end_date)
    
    # Find missing dates
    missing_dates <- setdiff(expected_dates, unique(season_data[[date_col]]))
    
    # Mean discharge for the season (excluding NAs)
    mean_discharge <- mean(season_data[[discharge_col]], na.rm = TRUE)
    
    # Create fake rows for missing dates
    if (length(missing_dates) > 0) {
      filled_rows <- data.frame(
        dummy_date = missing_dates,
        dummy_discharge = mean_discharge
      )
      colnames(filled_rows) <- c(date_col, discharge_col)
    } else {
      filled_rows <- NULL
    }
    
    # Combine real and filled data
    combined <- rbind(season_data[, c(date_col, discharge_col)], filled_rows)
    
    # Total discharge including filled
    total <- sum(combined[[discharge_col]], na.rm = TRUE)
    
    results <- rbind(results, data.frame(
      wet_season = paste0(year, "/", year + 1),
      total_discharge = total,
      filled_days = length(missing_dates)
    ))
  }
  return(results)
}

#dicahrge_filled <- total_discharge_filled(av_day_discharge, "date", "mean_discharge")
#write.csv(dicahrge_filled, "discharge_filled.csv")

wetseason_discharge <- total_discharge_filled(data = discharge, date_col = "Date", discharge_col = "Flow..ML.")

#write.csv(wetseason_discharge, "discharge_DI.csv")


avg_daily <- function(data, date_col, discharge_col) {
  # Convert date column
  data[[date_col]] <- as.Date(data[[date_col]], format = "%d/%m/%Y")  # Adjust format if needed
  
  results <- data.frame(
    wet_season = character(),
    total_discharge = numeric(),
    phase_1_discharge = numeric(),
    phase_2_discharge = numeric(),
    missing_days_total = integer(),
    missing_days_p1 = integer(),
    missing_days_p2 = integer(),
    stringsAsFactors = FALSE
  )
  
  for (year in 2011:2023) {
    # Date ranges
    start_total <- as.Date(paste0("01/12/", year), format="%d/%m/%Y")
    end_total   <- as.Date(paste0("31/05/", year + 1), format="%d/%m/%Y")
    
    start_p1 <- start_total
    end_p1   <- as.Date(paste0("31/01/", year + 1), format="%d/%m/%Y")
    
    start_p2 <- as.Date(paste0("01/02/", year + 1), format="%d/%m/%Y")
    end_p2   <- end_total
    
    # Expected sequences
    full_seq <- seq(start_total, end_total, by = "day")
    p1_seq   <- seq(start_p1, end_p1, by = "day")
    p2_seq   <- seq(start_p2, end_p2, by = "day")
    
    # Filtered data
    dates_in_range <- data[[date_col]]
    
    season_data <- data[data[[date_col]] %in% full_seq, ]
    phase_1_data <- data[data[[date_col]] %in% p1_seq, ]
    phase_2_data <- data[data[[date_col]] %in% p2_seq, ]
    
    # Missing days
    missing_total <- setdiff(full_seq, unique(season_data[[date_col]]))
    missing_p1 <- setdiff(p1_seq, unique(phase_1_data[[date_col]]))
    missing_p2 <- setdiff(p2_seq, unique(phase_2_data[[date_col]]))
    
    # Sum discharges
    total <- mean(season_data[[discharge_col]], na.rm = TRUE)
    phase_1 <- mean(phase_1_data[[discharge_col]], na.rm = TRUE)
    phase_2 <- mean(phase_2_data[[discharge_col]], na.rm = TRUE)
    
    # Combine1
    results <- rbind(results, data.frame(
      wet_season = paste0(year, "/", year + 1),
      total_discharge = total,
      phase_1_discharge = phase_1,
      phase_2_discharge = phase_2,
      missing_days_total = length(missing_total),
      missing_days_p1 = length(missing_p1),
      missing_days_p2 = length(missing_p2)
    ))
  }
  return(results)
}

avg_daily(data = av_day_discharge, date_col = "date", discharge_col = "mean_discharge")

#### Some basic discharge visualisation ####

discharge$Date <- as.Date(discharge$Date)

sawfish_pres <- data.frame(
  year = c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024,2025),
  present= c(0,0,1,0,0,0,0,1,1,0,0,1,0,0,1,0))

sawfish_pres$Date <- as.Date(paste0(sawfish_pres$year, "-06-01"))

sawfish_pres

ggplot() +
  geom_point(data = subset(sawfish_pres, present == 1), 
             aes(x = Date, y = 0), 
             color = "red", shape = 21, fill = "red", size = 3) +
  geom_line(data = discharge, aes(x = Date, y = Discharge_ML_day)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b_%y") +
  labs(x = "Month_Year", y = "Discharge (ML/day)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()




pres_data <- data.frame(
  year = c(2010:2025),
  pres = c(0,0,1,0,0,0,0,1,1,0,0,1,0,0,1,0)
)   


pres_data$Date <- as.Date(paste0(pres_data$year, "-06-01"))

pres_data

discharge_full$date <- as.Date(discharge_full$date)
head(discharge_full)

ggplot() +
  geom_line(data = discharge_full, aes(x = date, y = Discharge_ML_day)) +
  geom_point(data = subset(pres_data, pres == 1),
             mapping = aes(x = Date, y = 0),
             color = "red", size = 3) +
  scale_x_date(date_breaks = "1 year", date_labels = "%b_%y") +
  labs(x = "Month_Year", y = "Discharge (ML/day)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))













