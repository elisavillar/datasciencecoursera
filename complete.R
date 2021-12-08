#                          ****************************
#                          * ASSIGMENT: AIR POLLUTION *
#                          *      PART: 2             *
#                          ****************************
#
# Write a function that reads a directory full of files and reports the
# number of completely observed cases in each data file. 
# The function should return a data frame where the first column is the name of 
# the file and the second column is the number of complete cases. 
#
####################################################################################

complete <- function(directory, id = 1:332){
  
  # List all the files inside directory
  
  all_files <- list.files(directory, pattern = "*.csv", full.names = TRUE)
  
  # Initialize the data frame to add the data
  
  complete_data<- data.frame()
  
  # Loop for each file, read it, check for completeness and add it to the data frame
  
  for (i in id) {
    
    individual_file <- read.csv(all_files[i])
    
    id <- i
    
    nobs <- sum(complete.cases(individual_file))
    
    new_line <- cbind(id, nobs)
    
    complete_data <-  rbind (complete_data, new_line)
    
    
  }
  
  return(complete_data)
  
}


############## Test ######################

complete("specdata", 1)

complete("specdata", c(2, 4, 8, 10, 12))

complete("specdata", 30:25)

complete("specdata", 3)