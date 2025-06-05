#### Madden-Julian Oscillation Index #### 

MJO<- read.table("C:/Users/samue/Desktop/Honours/Chapter_3_Sawfish_rescue_modelling/Daly_Env_Vars/Real_time_OLR_MJO_index.txt", 
                       header = TRUE, 
                       fill = TRUE, 
                       strip.white = TRUE
)

MJO 

get_mjo_phase <- function(PC1, PC2) {
  # Calculate angle in degrees
  angle <- atan2(PC2, PC1) * 180 / pi
  
  # Normalize to 0–360
  if (angle < 0) {
    angle <- angle + 360
  }
  
  # Assign phase
  if (angle >= 337.5 || angle < 22.5) {
    return(1)
  } else if (angle < 67.5) {
    return(2)
  } else if (angle < 112.5) {
    return(3)
  } else if (angle < 157.5) {
    return(4)
  } else if (angle < 202.5) {
    return(5)
  } else if (angle < 247.5) {
    return(6)
  } else if (angle < 292.5) {
    return(7)
  } else {
    return(8)
  }
}

MJO$Phase <- mapply(get_mjo_phase, MJO$PC1, MJO$PC2)

print(MJO)

MJO$date <- paste(MJO$Day, MJO$Month, MJO$Year, sep = "/")


MJO <- MJO %>%
  dplyr::select(date, Phase, Amplitude, PC1, PC2)

MJO

#### Function building ####

average_amplitude <- function(data, date_col, amp_col) {
  # Convert date column to POSIXct
  data[[date_col]] <- as.POSIXct(data[[date_col]], format = "%d/%m/%Y")
  
  # Initialize results dataframe
  results <- data.frame(
    wet_season = character(),
    average_amp = numeric(),
    missing_days = integer(),
    stringsAsFactors = FALSE
  )
  
  for (year in 2011:2023) {
    # Define date range
    start_date <- as.POSIXct(paste0("01/11/", year, " 00:00"), format="%d/%m/%Y")
    end_date   <- as.POSIXct(paste0("30/04/", year + 1, " 23:59"), format="%d/%m/%Y")
    
    # Filter data for this wet season
    season_data <- data %>%
      filter(.data[[date_col]] >= start_date & .data[[date_col]] <= end_date)
    
    # Extract just the date part for missing day check
    season_dates <- as.Date(season_data[[date_col]])
    
    # Expected daily dates in the season
    expected_dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")
    
    # Identify missing days
    missing_days <- setdiff(expected_dates, unique(season_dates))
    
    # Calculate average river level (excluding NAs)
    avg_amp <- mean(season_data[[amp_col]], na.rm = TRUE)
    
    # Store result
    results <- rbind(results, data.frame(
      wet_season = paste0(year, "/", year + 1),
      average_amp = avg_amp,
      missing_days = length(missing_days)
    ))
  }
  
  return(results)
}

MJO_mean <- average_amplitude(MJO, "date", "Amplitude")

##### Most common phase ####

mode_phase <- function(data, date_col, phase_col) {
  # Convert date column to POSIXct
  data[[date_col]] <- as.POSIXct(data[[date_col]], format = "%d/%m/%Y")
  
  # Initialize results dataframe
  results <- data.frame(
    wet_season = character(),
    average_phase = numeric(),
    missing_days = integer(),
    stringsAsFactors = FALSE
  )
  
  # Add a statistical mode function from the internet
  
  Mode <- function(x, na.rm = FALSE) {
    if(na.rm){
      x = x[!is.na(x)]
    }
    
    ux <- unique(x)
    return(ux[which.max(tabulate(match(x, ux)))])
  }
  
  for (year in 2011:2023) {
    # Define date range
    start_date <- as.POSIXct(paste0("01/11/", year, " 00:00"), format="%d/%m/%Y")
    end_date   <- as.POSIXct(paste0("30/04/", year + 1, " 23:59"), format="%d/%m/%Y")
    
    # Filter data for this wet season
    season_data <- data %>%
      filter(.data[[date_col]] >= start_date & .data[[date_col]] <= end_date)
    
    # Extract just the date part for missing day check
    season_dates <- as.Date(season_data[[date_col]])
    
    # Expected daily dates in the season
    expected_dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")
    
    # Identify missing days
    missing_days <- setdiff(expected_dates, unique(season_dates))
    
    # Calculate average river level (excluding NAs)
    avg_phase <- Mode(season_data[[phase_col]])
    
    # Store result
    results <- rbind(results, data.frame(
      wet_season = paste0(year, "/", year + 1),
      mode_phase = avg_phase,
      missing_days = length(missing_days)
    ))
  }
  
  return(results)
}

MJO_mode <- mode_phase(MJO, "date", "Phase")

MJO_seasonal <- merge(MJO_mean, MJO_mode, by = "wet_season")

MJO_seasonal

write.csv(MJO_seasonal, "MJO_calculated.csv")
