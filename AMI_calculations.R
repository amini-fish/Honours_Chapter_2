#### Load in the COMPLETE AMI data #### 

AMI_full <- read.table("C:/Users/samue/Desktop/AMI_full.txt", header = TRUE, fill = TRUE, strip.white = TRUE)
print(AMI_full)

# Combine the day, month and year columns using paste and then seperate each value with /

AMI_full$date <- paste(AMI_full$Day, AMI_full$Month, AMI_full$Year, sep = "/")
AMI_full

# Select key columns  

AMI_full <- AMI_full %>%
  dplyr::select(date, Year, Index, X.stand.)

## Note that Index is actually the standardised AMI - could rename but am lazy right now

#### FUNCTION to calculate the average standardised AMI acorss the entire wet season - just need to specify the date range. Operates by adding 1 to the year in the date range

AMI_Wet_Season <- function(data, date_col, ami_col) {
  # Convert date column to POSIXct (date-time) format
  data[[date_col]] <- as.POSIXct(data[[date_col]], format = "%d/%m/%Y")
  
  # List to store results
  avg_ami_per_season <- list()
  
  # Loop over the years from 2011 to 2024
  for (year in 2011:2023) {
    # Define start and end dates for the wet season
    start_date <- as.POSIXct(paste0("01/11/", year), format="%d/%m/%Y")
    end_date <- as.POSIXct(paste0("30/04/", year + 1), format="%d/%m/%Y")
    
    # Subset data for the wet season using dynamic column names
    season_data <- data %>%
      filter(.data[[date_col]] >= start_date & .data[[date_col]] <= end_date)
    
    # Calculate the average river level in the wet season
    avg_ami_per_season[[paste0("ami_", year)]] <- mean(season_data[[ami_col]], na.rm = TRUE)
  }
  
  # Return the list of average heights for each wet season
  return(avg_ami_per_season)
}

AMI_Wet_Season(AMI_full, date_col = "date", ami_col = "Index")

## Transcribe to the main environmental metadata file

## Save as a csv exclusively for the AMI data
