best <- function(state, outcome) {
## Read outcome data
## Check that state and outcome are valid
## Return hospital name in that state with lowest 30-day death
## rate
  outcome_d <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  indices_c <- which(outcome_d$State == state)
 if (length(indices_c) <= 0) {
   stop("invalid state")
 } 
  
  names_c <- lapply(names(outcome_d), str_replace_all, fixed("."), " ")
  pattern_c <- paste(".*(Lower)",".*(Mortality)", ".*(", outcome, ")", sep = "" )
  col_c <- grep( pattern_c , names_c , perl= TRUE, ignore.case = TRUE )
 
  if (length(col_c) <= 0) {
    stop("invalid outcome")
  }
  
  filtered_c <- as.numeric(outcome_d[indices_c,col_c])
  
  index_c <- indices_c[which(filtered_c == min(filtered_c, na.rm = TRUE))]
  sort(outcome_d[index_c,2])[1]
}