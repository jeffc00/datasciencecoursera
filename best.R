best <- function(state, outcome) {
  df <- read.csv("pa3-data/outcome-of-care-measures.csv", stringsAsFactors = FALSE)

   possible_outcomes <- c("heart attack", "heart failure", "pneumonia")

   try(if(!(state %in% unique(df$State))) stop("invalid state"))
   try(if(!(outcome %in% possible_outcomes)) stop("invalid outcome"))

   if(outcome == "heart attack") {
     df_filtered <- df[df[,11] != "Not Available" & (df[,7] == state),]
     outcome_min <- min(as.numeric(df_filtered[,11]))
     output <- sort(df_filtered[as.numeric(df_filtered[,11]) == outcome_min, 2])[1]
   } else if(outcome == "heart failure") {
     df_filtered <- df[df[,17] != "Not Available" & (df[,7] == state),]
     outcome_min <- min(as.numeric(df_filtered[,17]))
     output <- sort(df_filtered[as.numeric(df_filtered[,17]) == outcome_min, 2])[1]
   } else if(outcome == "pneumonia") {
     df_filtered <- df[df[,23] != "Not Available" & (df[,7] == state),]
     outcome_min <- min(as.numeric(df_filtered[,23]))
     output <- sort(df_filtered[as.numeric(df_filtered[,23]) == outcome_min, 2])[1]
   }
  
  return(output)
}