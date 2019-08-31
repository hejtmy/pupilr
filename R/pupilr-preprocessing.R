#' Allows changing of the world timestamps or real timestamps start to a specific time
#'
#' @details As the world_timestamps are a useful feature but not representative
#' of the real timestamps, you can use this function to recompute their start based
#' of concrete number. Typically, thi is the start as is saved in the info.csv file
#' in the recording. This function therefore takes that formated number a%H:%M:%S
#' and recomputes the times in the wordl_timestamps so they start at given time
#'
#' @param obj object which timestamps should be recomputed
#' @param start_time needs \%H:\%M:\%S parameter to be passed
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
change_timestamps_start <- function(obj, start_time, ...){
  UseMethod("change_timestamps_start")
}

#' Allows changing of the world timestamps or real timestamps start to a specific time
#'
#' @details As the world_timestamps are a useful feature but not representative
#' of the real timestamps, you can use this function to recompute their start based
#' of concrete number. Typically, thi is the start as is saved in the info.csv file
#' in the recording. This function therefore takes that formated number a%H:%M:%S
#' and recomputes the times in the wordl_timestamps so they start at given time
#'
#' @param obj object which timestamps should be recomputed
#' @param start_time needs \%H:\%M:\%OS parameter to be passed. see strptime for details
#'
#' @return
#' @export
#'
#' @examples
change_timestamps_start.surface.item <- function(obj, start_time){
  timestamps <- obj$data$gaze$gaze_timestamp
  first_timestamp <- timestamps[1]
  time_since_midnight <- as.numeric(as.difftime(c(start_time, "0:0:0"), "%H:%M:%OS", units = "secs"))[1]
  new_timestamps <- time_since_midnight + timestamps - first_timestamp
  obj$data$gaze$gaze_timestamp <- new_timestamps
  return(obj)
}

#' Removes measurements which are not on given surface
#'
#' @param obj
#' @return modified object
#' @export
remove_not_on_surface <- function(obj){
  UseMethod("remove_not_on_surface")
}

#' Removes measurements which are not on given surface
#'
#' @param obj surface object
#'
#' @return modified object
#' @export
#'
#' @examples
remove_not_on_surface.surface <- function(obj){
  obj$gaze <- obj$gaze[obj$gaze$on_srf == "True", ]
  return(obj)
}

#' Filters out object with only times between start and end
#'
#' @param obj object to be modified
#' @param start start time in seconds
#' @param end end time in seconds
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
filter_times <- function(obj, start, end, ...){
  UseMethod("filter_times")
}

#' Filters out only times between start and end
#'
#' @param obj surface object
#' @param start start time in seconds
#' @param end end time in seconds
#' @param ... optional parameters just like in get_gaze_timewindow, get_fixations (e.g. on_surface)
#'
#' @return surface object with filtered out times
#' @export
#'
#' @examples
filter_times.surface.item <- function(obj, start, end, ...){
  gaze <- get_gaze_timewindow(obj, start, end, ...)
  obj$data$gaze <- gaze
  return(obj)
}

## EYER CONVERTIONS ----
#' Converts given object to an eyer object
#'
#' @description
#'
#' @param obj Object to be converted
#' @param ...
#'
#' @return valid eyer object, list of eyer objects or NULL if it can't be converted
#' @export
#'
#' @examples
as.eyer <- function(obj, ...){
  UseMethod("as.eyer")
}

#' @export
as.eyer.pupilr <- function(obj, ...){
  eye <- EyerObject()
  eye$info$start_time <- obj$data$gaze$world_timestamp[1]
  # Converts the main data
  eye$data$gaze <- gaze_to_eyer_df(obj$data$gaze, eye$info$start_time)
  eye$data$fixations <- fixations_to_eyer_df(obj$data$fixations, eye$info$start_time)
  # convects the surfaces
  eye$surfaces <- as.eyer.surfaces(obj$surfaces)
  eye$info <- obj$info
  eye$info$eyetracker <- EYETRACKER_NAME
  eye$export_info <- obj$export_info
  return(eye)
}

#' @export
as.eyer.surfaces <- function(obj, ...){
  for (name in names(obj$items)){
    obj$items[[name]] <- as.eyer.surface.item(obj$items[[name]])
  }
  return(obj)
}

#' Converts surface item to an eyer object
#'
#' @param obj surface.item object
#' @param ...
#'
#' @return EyerObject
#' @export
#'
#' @examples
as.eyer.surface.item <- function(obj, ...){
  eye <- EyerObject()
  #TODO - this might have to be redone
  eye$info$start_time <- obj$data$gaze$gaze_timestamp[1]

  eye$data$gaze <- surface_gaze_to_eyer_df(obj$data$gaze, eye$info$start_time)

  eye$data$fixations <- surface_fixations_to_eyer_df(obj$data$fixations, eye$info$start_time)
  eye$info$eyetracker <- EYETRACKER_NAME
  return(eye)
}

### NON PUBLIC -----
preprocess.surface.item <- function(obj, ...){
  prep_d <- function(df){
    df <- rename_column(df, c("on_surf", "on_srf"), "on_surface")
    df$on_surface <- df$on_surface == "True"
    if (all(df$norm_pos_x - df$x_scaled == 0)) df[, c("x_scaled", "y_scaled")] <- NULL
    return(df)
  }
  obj$data$fixations <- prep_d(obj$data$fixations)
  obj$data$gaze <- prep_d(obj$data$gaze)
  return(obj)
}

gaze_to_eyer_df <- function(gaze, start_time){
  if(is.null(gaze) || nrow(gaze) == 0) return(data.frame())
  gaze <- rename_column(gaze, "gaze_timestamp", "time") # in nwe versions this the name
  gaze <- rename_column(gaze, "world_timestamp", "time") # this is the name in old versions
  gaze$time <- gaze$time - start_time
  gaze <- rename_column(gaze, "norm_pos_x", "x")
  gaze <- rename_column(gaze, "norm_pos_y", "y")
  gaze[, c(2, 6:length(colnames(gaze)))] <- NULL
  return(gaze)
}

fixations_to_eyer_df <- function(fixations, start_time){
  if(is.null(fixations) || nrow(fixations) == 0) return(data.frame())
  fixations$time <- fixations$start_timestamp - start_time
  fixations[, c("start_frame_index", "end_frame_index", "method",
                "gaze_point_3d_x",	"gaze_point_3d_y",	"gaze_point_3d_z",
                "base_data")] <- NULL
  fixations <- rename_column(fixations, "norm_pos_x", "x")
  fixations <- rename_column(fixations, "norm_pos_y", "y")
  return(fixations)
}

surface_gaze_to_eyer_df <- function(gaze, start_time){
  if(is.null(gaze) || nrow(gaze) == 0) return(data.frame())
  gaze$time <- gaze$gaze_timestamp - start_time
  gaze <- rename_column(gaze, "x_norm", "x")
  gaze <- rename_column(gaze, "y_norm", "y")
  gaze[, c("world_timestamp", "world_index", "gaze_timestamp")] <- NULL
  return(gaze)
}

surface_fixations_to_eyer_df <- function(fixations, start_time){
  if( is.null(fixations) || nrow(fixations) == 0) return(data.frame())
  fixations$time <- fixations$start_timestamp - start_time
  fixations[, c("fixations_id", "start_timestamp")] <- NULL
  fixations <- rename_column(fixations, "norm_pos_x", "x")
  fixations <- rename_column(fixations, "norm_pos_y", "y")
  return(fixations)
}
