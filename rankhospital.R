rankhospital <- function(state, outcome, num = "best") {
  df <- read.csv("pa3-data/outcome-of-care-measures.csv", stringsAsFactors = FALSE)
  
  possible_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  try(if(!(state %in% unique(df$State))) stop("invalid state"))
  try(if(!(outcome %in% possible_outcomes)) stop("invalid outcome"))
  
  if(outcome == "heart attack") {
    df_filtered <- df[df[,11] != "Not Available" & (df[,7] == state), c(2, 11)]
  } else if(outcome == "heart failure") {
    df_filtered <- df[df[,17] != "Not Available" & (df[,7] == state), c(2, 17)]
  } else if(outcome == "pneumonia") {
    df_filtered <- df[df[,23] != "Not Available" & (df[,7] == state), c(2, 23)]
  }
  
  df_filtered_sorted <- df_filtered[order(as.numeric(df_filtered[,2]), df_filtered[,1]),]
  
  if(num == "best") {
    return(first(df_filtered_sorted[,1]))
  } else if(num == "worst") {
    return(last(df_filtered_sorted[,1]))
  } else
    return(df_filtered_sorted[num, 1])
}
