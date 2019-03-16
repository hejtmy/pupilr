#' Loads all important exported objecs from given folder
#'
#' @param dir directory with the exported data
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
#' @param dir directory with the exported data
#'
#' @return loaded info data.frame
#' @export
#'
#' @examples
open_info_file <- function(dir){
  return(open_exported_file(dir,"_info.csv"))
}

#' Retusns gaze positions data frame
#'
#' @param dir directory with the exported data
#'
#' @return data.frame with loaded gaze positions
#' @export
#'
#' @examples
open_gaze_file <- function(dir){
  return(open_exported_file(dir,"gaze_positions.csv"))
}


#' Returns data frame of pupil positions
#'
#' @param dir directory with the exported data
#'
#' @return data.frame with loaded pupil positions
#' @export
#'
#' @examples
open_positions_file <- function(dir){
  return(open_exported_file(dir,"pupil_positions.csv"))
}

#' Returns data frame of world_timestamps
#'
#' @param dir directory with the exported data
#'
#' @return data.frame with loaded timestamps
#' @export
#'
#' @examples
open_timestamps_file <- function(dir){
  return(open_exported_file(dir, "world_timestamps.csv"))
}

#' Searches for the surfaces and tries to load them into a list
#'
#' @param dir directory with the exported data
#'
#' @return list with surfaces and their definitions
#' @export
#'
#' @examples
open_surfaces <- function(dir){
  path <- find_single_file(dir, "surfaces")
  if(!is.null(path)) return(load_surfaces(path))
  return(NULL)
}

load_surfaces <- function(path){
  return(list(a = NULL))
}

#' General function to load preprocessed file based on pattern
#'
#' @param dir where to look for the data.frame csv
#' @param ptr patten to search for
#'
#' @return data.frame
#' @export
#'
#' @examples
open_exported_file <- function(dir, ptr){
  path <- find_single_file(dir, ptr)
  if(!is.null(path)) return(load_exported_file(path))
  return(NULL)
}

