rankall <- function(outcome, num = "best") {
  df <- read.csv("pa3-data/outcome-of-care-measures.csv", stringsAsFactors = FALSE)
  
  possible_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  try(if(!(outcome %in% possible_outcomes)) stop("invalid outcome"))
  
  if(outcome == "heart attack") {
    df_filtered <- df[df[,11] != "Not Available", c(2, 7, 11)]
  } else if(outcome == "heart failure") {
    df_filtered <- df[df[,17] != "Not Available", c(2, 7, 17)]
  } else if(outcome == "pneumonia") {
    df_filtered <- df[df[,23] != "Not Available", c(2, 7, 23)]
  }

  df_filtered_sorted <- df_filtered[order(df_filtered[,2], as.numeric(df_filtered[,3])),]
  
  states <- unique(df_filtered_sorted[,2])
  if(num == "best") {
    return(
      bind_rows(
        apply(as.array(states), 1, function(s) head(df_filtered_sorted[df_filtered_sorted['State'] == s,], 1))))
  } else if(num == "worst") {
    return(
      bind_rows(
        apply(as.array(states), 1, function(s) tail(df_filtered_sorted[df_filtered_sorted['State'] == s,], 1))))
  } else
    return(
      bind_rows(
        apply(as.array(states), 1, function(s) slice(df_filtered_sorted[df_filtered_sorted['State'] == s,], num))))
}