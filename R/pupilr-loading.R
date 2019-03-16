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
  surface_dir <- find_single_file(dir, "surfaces")
  if(!is.null(surface_dir)) return(load_surface_data(surface_dir))
  return(NULL)
}

load_surface_data <- function(dir){
  ls <- list()
  ls$data <- list()
  # loads events
  ls$data$events <- open_exported_file(dir, "surface_events")
  #gets all fixation files
  surfaces <- list.files(dir, "fixations_on_surface")
  #extracts name
  surfaces_names <- sub("fixations_on_surface_(.*?)_(.*)[.]csv", "\\1", surfaces)
  surfaces_times <- sub("fixations_on_surface_(.*?)_(.*)[.]csv", "\\2", surfaces)
  for(i in 1:length(surfaces_names)){
    surface_name <- surfaces_names[i]
    surface_timestamp <- surfaces_times[i]
    fix_filepath <- file.path(dir, paste0("fixations_on_surface_", surface_name, "_", surface_timestamp, ".csv"))
    fixations <- load_exported_file(fix_filepath)
    gaze_filepath <- file.path(dir, paste0("gaze_positions_on_surface_", surface_name, "_", surface_timestamp, ".csv"))
    gaze <- load_exported_file(gaze_filepath)
    surface_filepath <- file.path(dir, paste0("srf_positons_", surface_name, "_", surface_timestamp, ".csv"))
    surface_positions <- load_exported_file(surface_filepath)
    ls[[surface_name]] <- list(fixations=fixations, gaze=gaze, surface_positions=surface_positions, timestamp = surface_timestamp)
    class(ls[[surface_name]]) <- append(class(ls), "surface")
  }
  return(ls)
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

