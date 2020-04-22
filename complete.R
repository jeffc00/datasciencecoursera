complete <- function(directory, id = 1:332) {
  files <- paste(directory, "/", list.files(path = directory), sep = "")
  df <- bind_rows(lapply(files, read_csv))
  return(df %>%
           filter(!is.na(sulfate) & ID %in% id) %>%
           group_by(ID) %>%
           summarise(nobs = n()))
}