#' Loads all important exported objecs from given folder
#'
#' @param folder directory with the exported data
#'
#' @return PupilrObject
#' @export
#'
#' @examples
load_folder <- function(folder){
  obj <- PupilrObject()
  return(obj)
}

#' Finds and loads info file from given directory
#'
#' @param folder directory with the exported data
#'
#' @return list with exported_info file contents
#' @export
#'
#' @examples
open_info_file <- function(folder){
  df <- open_exported_file(folder, "_info.csv")
  values <- df$value
  names(values) <- df$key
  ls <- as.list(values)
  return(ls)
}

#' Retusns gaze positions data frame
#'
#' @param folder directory with the exported data
#'
#' @return data.frame with loaded gaze positions
#' @export
#'
#' @examples
open_gaze_file <- function(folder){
  return(open_exported_file(folder, "gaze_positions.csv"))
}

#' Returns data frame of pupil positions
#'
#' @param folder directory with the exported data
#'
#' @return data.frame with loaded pupil positions
#' @export
#'
#' @examples
open_positions_file <- function(folder){
  return(open_exported_file(folder,"pupil_positions.csv"))
}

#' Returns data frame of world_timestamps
#'
#' @param folder directory with the exported data
#'
#' @return data.frame with loaded timestamps
#' @export
#'
#' @examples
open_timestamps_file <- function(folder){
  return(open_exported_file(folder, "world_timestamps.csv"))
}

#' Searches for the surfaces and tries to load them into a list
#'
#' @param folder directory with the exported data
#'
#' @return list with surfaces and their definitions
#' @export
#'
#' @examples
open_surfaces <- function(folder){
  surface_folder <- find_single_file(folder, "surfaces")
  if(!is.null(surface_folder)) return(load_surface_data(surface_folder))
  return(NULL)
}

#' Loads folder with surface data
#'
#' @param folder path to the folder
#'
#' @return
#' @export
#'
#' @examples
load_surface_data <- function(folder){
  ls <- list()
  ls$data <- list()
  # loads events
  ls$data$events <- open_exported_file(folder, "surface_events")
  #gets all fixation files
  surfaces <- list.files(folder, "fixations_on_surface")
  #extracts name
  surfaces_names <- sub("fixations_on_surface_(.*?)_(.*)[.]csv", "\\1", surfaces)
  surfaces_times <- sub("fixations_on_surface_(.*?)_(.*)[.]csv", "\\2", surfaces)
  for(i in 1:length(surfaces_names)){
    surface_name <- surfaces_names[i]
    surface_timestamp <- surfaces_times[i]
    fix_filepath <- file.path(folder, paste0("fixations_on_surface_", surface_name, "_", surface_timestamp, ".csv"))
    fixations <- load_exported_file(fix_filepath)
    gaze_filepath <- file.path(folder, paste0("gaze_positions_on_surface_", surface_name, "_", surface_timestamp, ".csv"))
    gaze <- load_exported_file(gaze_filepath)
    surface_filepath <- file.path(folder, paste0("srf_positons_", surface_name, "_", surface_timestamp, ".csv"))
    surface_positions <- load_exported_file(surface_filepath)
    ls[[surface_name]] <- list(fixations=fixations, gaze=gaze, surface_positions=surface_positions, timestamp = surface_timestamp)
    class(ls[[surface_name]]) <- append(class(ls), "surface")
  }
  return(ls)
}

#' General function to load preprocessed file based on pattern
#'
#' @param folder where to look for the data.frame csv
#' @param ptr patten to search for
#'
#' @return data.frame
#' @export
#'
#' @examples
open_exported_file <- function(folder, ptr){
  path <- find_single_file(folder, ptr)
  if(!is.null(path)) return(load_exported_file(path))
  return(NULL)
}

