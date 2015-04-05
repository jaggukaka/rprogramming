rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  outcome_d <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  indices_c <- which(outcome_d$State == state)
  if (length(indices_c) <= 0) {
    stop("invalid state")
  } 
  
  names_c <- lapply(names(outcome_d), str_replace_all, fixed("."), " ")
  pattern_c <- paste(".*(Mortality)", ".*(", outcome, ")", sep = "" )
  col_c <- grep( pattern_c , names_c , perl= TRUE, ignore.case = TRUE )
  col_c <- col_c[1]
  
  if (is.na(col_c) | length(col_c) <= 0) {
    stop("invalid outcome")
  }

  filtered_c <- data.frame( x = outcome_d[indices_c, 2], y = as.numeric(outcome_d[indices_c,col_c])) 
  result_c <- filtered_c[order(filtered_c$y, filtered_c$x), ]
  result_c <- result_c[which(!is.na(result_c$y)),]
  
  res_c <- c()
  len_c <- length(result_c$x)
  
  if (num == "best" ) {
    res_c <- result_c$x[1]
  } else if (num == "worst") {
    res_c <-  result_c$x[len_c]
  } else if (num > len_c) {
    return (NA)
  } else {
    res_c <- result_c$x[num]
  }

  return (as.character(res_c))

}