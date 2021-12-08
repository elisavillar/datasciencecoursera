#                          ****************************
#                          * ASSIGMENT: AIR POLLUTION *
#                          *      PART: 1             *
#                          ****************************
#
#
# Write a function named 'pollutantmean' that calculates the mean of a pollutant
# (sulfate or nitrate) across a specified list of monitors. The function 
# 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. Given
# a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate 
# matter data from the directory specified in the 'directory' argument and returns
# the mean of the pollutant across all of the monitors, ignoring any missing 
# values coded as NA. 
#
####################################################################################


pollutantmean <- function(directory, pollutant, id = 1:332){
  
  # List all the files inside directory
  
  all_files <- list.files(directory, pattern = "*.csv", full.names = TRUE)
 
  
  # Initialize the data frame to add the data
  
  pollutant_data<- data.frame()
  
  
  # Loop for each file, read it and bind it to the data frame
  
  for (i in id) {
    
    individual_file <- read.csv(all_files[i])
    
    pollutant_data <- rbind(pollutant_data, individual_file)
    
  }
  
  # Calculate the mean (ignoring NA values)
  
  return(mean(pollutant_data[[pollutant]],na.rm = TRUE))
  
}


############## Test ######################

pollutantmean("specdata", "sulfate", 1:10)

pollutantmean("specdata", "nitrate", 70:72)

pollutantmean("specdata", "nitrate", 23)
