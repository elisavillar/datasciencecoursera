#                          ****************************
#                          * ASSIGMENT: AIR POLLUTION *
#                          *      PART: 3             *
#                          ****************************
#
#
# Write a function that takes a directory of data files and a threshold for complete 
# cases and calculates the correlation between sulfate and nitrate for monitor locations 
# where the number of completely observed cases (on all variables) is greater than the 
# threshold. The function should return a vector of correlations for the monitors that 
# meet the threshold requirement. If no monitors meet the threshold requirement, then 
# the function should return a numeric vector of length 0. 
#
####################################################################################

corr <- function(directory, threshold = 0) {
  
  # List all the files inside directory
  
  all_files <- list.files(directory, pattern = "*.csv", full.names = TRUE)
  
  # Initialize the numeric vector
  
  corr_vector<- vector(mode = "numeric", length = 0)
  
  for (i in 1:length(all_files)) {
    
    # Read each file
    
    individual_file <- read.csv(all_files[i])
    
    # Count the number of complete cases
    
    comp_cases_count <- sum(complete.cases(individual_file))
    
    #Check if complete cases count is greater than threshold
    
    if (comp_cases_count > threshold) {
      
      no_na_sulfate <- individual_file[which(!is.na(individual_file$sulfate)), ]
      
      no_na_pollutant <- no_na_sulfate[which(!is.na(no_na_sulfate$nitrate)), ]
      
      corr_vector <- c(corr_vector, cor(no_na_pollutant$sulfate, no_na_pollutant$nitrate))
      
    }
  }
  
  return(corr_vector)
}


############## Test ######################

cr <- corr("specdata", 150)
head(cr)

summary(cr)

cr <- corr("specdata", 400)
head(cr)

summary(cr)

cr <- corr("specdata", 5000)
summary(cr)

length(cr)

cr <- corr("specdata")
summary(cr)

length(cr)
