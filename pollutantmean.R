pollutantmean <- function(directory, pollutant, id = 1:332) {
  files <- paste(directory, "/", list.files(path = directory), sep = "")
  df <- bind_rows(lapply(files, read_csv))
  return(colMeans(df[df$ID %in% id, pollutant], na.rm = TRUE))
}