corr <- function(directory, threshold = 0) {
  files <- paste(directory, "/", list.files(path = directory), sep = "")
  df <- bind_rows(lapply(files, read_csv))
  id <- (1:332)[complete(directory)$nobs > threshold]
  return(df %>%
           filter(!is.na(sulfate) & ID %in% id) %>%
           group_by(ID) %>%
           summarise(corr = cor(sulfate, nitrate)) %>%
           select(corr))
}