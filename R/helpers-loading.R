find_single_file <- function(dir, ptr){
  ls <- list.files(dir, ptr, full.names = T)
  if(length(ls) < 1 ) warning(paste0("There are no files of pattern ", ptr, " in directory ", dir))
  if(length(ls) > 1 ) warning(paste0("There are ", length(ls), " files of pattern ", ptr, " in directory ", dir))
  if(length(ls) == 1) return(ls[1])
  return(NULL)
}
