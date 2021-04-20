#' Loads all important exported objecs from given folder
#'
#' @param folder directory with the exported data
#'
#' @return \code{\link{PupilrObject}}
#' @export
#'
#' @examples
load_folder <- function(folder){
  obj <- PupilrObject()
  obj$export_info <- open_expoted_info_file(folder)
  info_file <-  open_info_file(folder)
  if(is.null(info_file)){
    warning("The package expects the info.csv from the original file to be present.
            It contains some information about real time recording, dates etc.")
    obj$info <- list()
    } else {
      obj$info <- info_file
  }
  obj$data$gaze <- open_gaze_file(folder)
  obj$data$fixations <- open_fixations_file(folder)
  obj$surfaces <- open_surfaces(folder)
  return(obj)
}

#' Finds and loads `info.csv` file from given directory
#'
#' @param folder directory with the `info.csv`` file
#'
#' @return list with info file contents
#' @export
#'
#' @examples
open_info_file <- function(folder){
  pth <- find_single_file(folder, "^info.csv$")
  res <- load_key_value_file(pth)
  return(res)
}

#' Finds and loads `exported_info.csv` file from given directory
#'
#' @param folder directory with the exported data
#'
#' @return list with exported_info file contents
#' @export
#'
#' @examples
open_expoted_info_file <- function(folder){
  pth <- find_single_file(folder, "*_info.csv")
  res <- load_key_value_file(pth)
  return(res)
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

#' Retusns fixations data.frame
#'
#' @description returns loaded fixations file from the folder,
#' in case you exported it as part of the export
#'
#' @param folder directory with the exported data
#'
#' @return data.frame with loaded fixations positions
#' @export
#'
#' @examples
open_fixations_file <- function(folder){
  return(open_exported_file(folder, "fixations.csv"))
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
#' @param preprocess: if the data should be immediately cleaned. default T. If set to false, some filtering and plotting functions will not work
#'
#' @return
#' @export
#'
#' @examples
load_surface_data <- function(folder, preprocess = T){
  ls <- SurfacesObject()
  # loads events
  ls$data$events <- open_exported_file(folder, "surface_events")
  # gets all fixation files
  surfaces <- list.files(folder, "fixations_on_surface")
  # POssible names have or don't have timestamps in them
  # fixations_on_surface_unnamed_1552524777.3223464.csv
  # fixations_on_surface_unnamed.csv
  surfaces_names <- sub("fixations_on_surface_([^_]+)[_]?([0-9.]*?).csv", "\\1", surfaces, perl = T)
  surfaces_times <- sub("fixations_on_surface_([^_]+)[_]?([0-9.]*?).csv", "\\2", surfaces, perl = T)
  for(i in 1:length(surfaces_names)){
    surface_name <- surfaces_names[i]
    surface_timestamp <- surfaces_times[i]
    if(nchar(surface_timestamp) > 1) surface_timestamp <- paste0("_", surface_timestamp)

    fix_filepath <- file.path(folder, paste0("fixations_on_surface_", surface_name, surface_timestamp, ".csv"))
    fixations <- load_exported_file(fix_filepath)
    gaze_filepath <- file.path(folder, paste0("gaze_positions_on_surface_", surface_name, surface_timestamp, ".csv"))
    gaze <- load_exported_file(gaze_filepath)

     # A bit awkward but the exports changed preposition srf to surf and who knows what it is going to be in the future
    positions_file_ptr <- paste0("_positions_", surface_name, surface_timestamp, ".csv")
    surface_filepath <- list.files(folder, positions_file_ptr, full.names = T)[1]

    if(is.na(surface_filepath)) {
      warning("there isn't a ", positions_file_ptr, " file in the surfaces folder")
      positions <- NULL
    } else {
      positions <- load_exported_file(surface_filepath)
    }

    surface <- SurfaceItemObject()
    surface$data <- list(fixations = fixations,
                         gaze = gaze,
                         positions = positions)
    surface$info$file_timestamp <- surface_timestamp
    if(preprocess) surface <- preprocess.surface.item(surface)

    ls$items[[surface_name]] <- surface
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

#' Function to read pupil key, value style dataframes into named list
#'
#' @description Loads the dataframe and returns named list
#' @param filepath path to the file
#'
#' @return loaded named list
#' @export
#'
#' @examples
load_key_value_file <- function(filepath){
  if(is.null(filepath)) return(NULL)
  df <- load_exported_file(filepath)
  values <- df$value
  names(values) <- df$key
  ls <- as.list(values)
  return(ls)
}
