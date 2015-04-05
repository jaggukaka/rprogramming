pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
 
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  all_paths <- paste(directory, .Platform$file.sep, sprintf("%03d", id), ".csv", sep = "")
  filtered_data <- lapply(all_paths, read.csv)
  filtered_dataframe <- do.call(rbind.data.frame, filtered_data)
  as.numeric(format(round(mean(filtered_dataframe[,pollutant], na.rm = TRUE), 3), nsmall = 3))
}