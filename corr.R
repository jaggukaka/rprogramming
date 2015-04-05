corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  all_paths <- dir(directory, pattern = "\\.csv", full.names = TRUE)
  filtered_data <- lapply(all_paths, read.csv)
  cor_v <- sapply(filtered_data, function (d) {   
    complete_cases_n <- dim(d[complete.cases(d),])[1]
    if (complete_cases_n > threshold) {
         cor(d$sulfate, d$nitrate, use = "na.or.complete")
    }
  })
  
  cor_v[!sapply(cor_v, is.null)]
  cor_v <- unlist(cor_v)
  as.numeric(cor_v)
}



