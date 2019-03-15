#' Title
#'
#' @param dir directory to load
#'
#' @return PupilrObject
#' @export
#'
#' @examples
load_folder <- function(dir){
  obj <- PupilrObject()
  return(obj)
}


#' Finds and loads info file from given directory
#'
#' @param dir directory in which to search
#'
#' @return loaded info data.frame
#' @export
#'
#' @examples
open_info_file <- function(dir){
  path <- find_info_file(dir)
  if(!is.null(path)) return(load_info_file(path))
  warning('No info file found, returning NULL')
  return(NULL)
}

#' Loads info file from a given path
#'
#' @param path path to the info file
#'
#' @return loaded info data.frame
#' @export
#'
#' @examples
load_info_file <- function(path){
  # validate path
  df <- read.table(path, sep=",", header=T)
  return(df)
}


## find
find_info_file <- function(dir){
  ptr <- "_info.csv"
  return(find_single_file(dir, ptr))
}


##
