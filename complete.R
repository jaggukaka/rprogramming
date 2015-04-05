complete <- function(directory, id = 1:332) {
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
  nobs <- sapply(filtered_data, function (d) {   
    dim(d[complete.cases(d),])[1]
  })
  
  data.frame(id, nobs)
}