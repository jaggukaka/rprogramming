rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  outcome_d <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  names_c <- lapply(names(outcome_d), str_replace_all, fixed("."), " ")
  pattern_c <- paste(".*(Mortality)", ".*(", outcome, ")", sep = "" )
  col_c <- grep( pattern_c , names_c , perl= TRUE, ignore.case = TRUE )
  col_c <- col_c[1]
  
  if (length(col_c) <= 0) {
    stop("invalid outcome")
  }
  
  filtered_c <- data.frame( hospital = outcome_d[, 2], y = as.numeric(outcome_d[,col_c]), state = outcome_d[, 7] ) 
  result_c <- filtered_c[order(filtered_c$state, filtered_c$y, filtered_c$hospital), ]
  result_c <- result_c[which(!is.na(result_c$y)),]
  
  
#   result_sorted <- aggregate( y ~ z, result_c, function(dat_c, num) {
#     
#     len_c <- length(dat_c)
#   
#     if (num == "best" ) {
#       res_c <- dat_c[1]
#     } else if (num == "worst") {
#       res_c <-  dat_c[len_c]
#     } else if (num > len_c) {
#       return (NA)
#     } else {
#       res_c <- dat_c[num]
#     }
#   }, num)
#   
#   print(result_sorted)
#   merge(result_sorted, result_c)
  
  
  result_sorted <- do.call(rbind, lapply(split(result_c, result_c$state), function(dat_c, num) {
    len_c <- length(dat_c$y)
    
    if (num == "best" ) {
      dat_c[1, ]
    } else if (num == "worst") {
      dat_c[len_c, ]
     
    } else if (num > len_c) {
      dat_na <- data.frame(hospital = NA, y = NA, state = dat_c$state[1])
    } else {
      dat_c[num, ]
    }
  }
    , num))

  result_sorted$y = NULL
   
return (result_sorted)
  
}