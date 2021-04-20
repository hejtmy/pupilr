load_exported_file <- function(path){
  # validate path
  df <- read.table(path, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  return(df)
}

find_single_file <- function(dir, ptr){
  ls <- list.files(dir, ptr, full.names = TRUE)
  if(length(ls) < 1 ) warning("There are no files of pattern ", ptr, " in directory ", dir)
  if(length(ls) > 1 ) warning("There are ", length(ls), " files of pattern ", ptr, " in directory ", dir)
  if(length(ls) == 1) return(ls[1])
  return(NULL)
}
