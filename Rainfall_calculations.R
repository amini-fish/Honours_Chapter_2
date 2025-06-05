#### Rainfall data for modelling ####

## Calculate rainfall for: 
  # Entire wetseason - defined as November to April each year 
  # Part 1 - November to January 
  # Part 2 - February to April 

#### Load the rainfall data into R ####

rainfall <- read.table("C:/Users/samue/Desktop/Honours/Chapter_3_Sawfish_rescue_modelling/Daly_Env_Vars/SILO_precipitation.txt", 
                       header = TRUE, 
                       fill = TRUE, 
                       strip.white = TRUE
                       )

rainfall <- rainfall[-1,]

head(rainfall) # Looks good 

rainfall <- rainfall %>%
  dplyr::select(Date2, Rain)

rainfall$Rain <- as.numeric(rainfall$Rain)


## Okay, the data is good to go. 

#### Load in Rainfall AVERAGE function ####

average_rainfall <- function(data, date_col, rain_col) {
  # Convert date column to POSIXct (date-time) format
  data[[date_col]] <- as.POSIXct(data[[date_col]], format = "%d/%m/%Y")
  
  # List to store results
  avg_rain_per_season <- list()
  
  # Loop over the years from 2011 to 2024
  for (year in 2011:2023) {
    # Define start and end dates for the wet season
    start_date <- as.POSIXct(paste0("01/11/", year, " 00:00"), format="%d/%m/%Y")
    end_date <- as.POSIXct(paste0("30/04/", year + 1, " 23:59"), format="%d/%m/%Y")
    
    # Subset data for the wet season using dynamic column names
    season_data <- data %>%
      filter(.data[[date_col]] >= start_date & .data[[date_col]] <= end_date)
    
    # Calculate the average river level in the wet season
    avg_rain_per_season[[paste0("avg_mm_", year)]] <- mean(season_data[[rain_col]], na.rm = TRUE)
  }
  
  # Return the list of average heights for each wet season
  return(avg_rain_per_season)
}

Wet_Season_Rain <- average_rainfall(rainfall, "Date2", "Rain")

print(Wet_Season_Rain)

#### Load in Rainfall SUM function ####

total_rainfall <- function(data, date_col, rain_col) {
  # Convert date column to POSIXct (date-time) format
  data[[date_col]] <- as.POSIXct(data[[date_col]], format = "%d/%m/%Y")
  
  # List to store results
  total_rain_per_season <- list()
  
  # Loop over the years from 2011 to 2024
  for (year in 2011:2023) {
    # Define start and end dates for the wet season
    start_date <- as.POSIXct(paste0("01/11/", year, " 00:00"), format="%d/%m/%Y")
    end_date <- as.POSIXct(paste0("30/04/", year + 1, " 23:59"), format="%d/%m/%Y")
    
    # Subset data for the wet season using dynamic column names
    season_data <- data %>%
      filter(.data[[date_col]] >= start_date & .data[[date_col]] <= end_date)
    
    # Calculate the average river level in the wet season
    total_rain_per_season[[paste0("avg_mm_", year)]] <- sum(season_data[[rain_col]], na.rm = TRUE)
  }
  
  # Return the list of average heights for each wet season
  return(total_rain_per_season)
}

#### Calculate rainfall for the entire wet season #### 

Total_Rain <- total_rainfall(rainfall, "Date2", "Rain")

print(Total_Rain)

#### Function for first half of wet season #### 

part1_total_rainfall <- function(data, date_col, rain_col) {
  # Convert date column to POSIXct (date-time) format
  data[[date_col]] <- as.POSIXct(data[[date_col]], format = "%d/%m/%Y")
  
  # List to store results
  total_rain_per_season <- list()
  
  # Loop over the years from 2011 to 2024
  for (year in 2011:2023) {
    # Define start and end dates for the wet season
    start_date <- as.POSIXct(paste0("01/11/", year, " 00:00"), format="%d/%m/%Y")
    end_date <- as.POSIXct(paste0("31/01/", year + 1, " 23:59"), format="%d/%m/%Y")
    
    # Subset data for the wet season using dynamic column names
    season_data <- data %>%
      filter(.data[[date_col]] >= start_date & .data[[date_col]] <= end_date)
    
    # Calculate the average river level in the wet season
    total_rain_per_season[[paste0("avg_mm_", year)]] <- sum(season_data[[rain_col]], na.rm = TRUE)
  }
  
  # Return the list of average heights for each wet season
  return(total_rain_per_season)
}

#### Total part 1 rain #### 

Rain_pt1 <- part1_total_rainfall(rainfall, "Date2", "Rain")

#### Function for second half of wet season #### 

part2_total_rainfall <- function(data, date_col, rain_col) {
  # Convert date column to POSIXct (date-time) format
  data[[date_col]] <- as.POSIXct(data[[date_col]], format = "%d/%m/%Y")
  
  # List to store results
  total_rain_per_season <- list()
  
  # Loop over the years from 2011 to 2024
  for (year in 2011:2023) {
    # Define start and end dates for the wet season
    start_date <- as.POSIXct(paste0("01/02/", year + 1, " 00:00"), format="%d/%m/%Y")
    end_date <- as.POSIXct(paste0("30/04/", year + 1, " 23:59"), format="%d/%m/%Y")
    
    # Subset data for the wet season using dynamic column names
    season_data <- data %>%
      filter(.data[[date_col]] >= start_date & .data[[date_col]] <= end_date)
    
    # Calculate the average river level in the wet season
    total_rain_per_season[[paste0("avg_mm_", year)]] <- sum(season_data[[rain_col]], na.rm = TRUE)
  }
  
  # Return the list of average heights for each wet season
  return(total_rain_per_season)
}

### Estimate total rain for part 2 ### 

Rain_pt2 <- part2_total_rainfall(rainfall, "Date2", "Rain")

Rain_pt2

#### Merge rainfall data together #### 

Rainfall_calc <- cbind(Total_Rain, Wet_Season_Rain, Rain_pt1, Rain_pt2)

write.csv(Rainfall_calc, "Rainfall_data.csv")
