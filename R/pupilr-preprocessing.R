#' Changes timestamps to a given start time
#'
#' @description Zero bases timestamps (substracts `start_time`) and saves new start time information in the `obj$info$start_time`
#'
#' @detailsAs the world_timestamps/gaze timestamps are a useful feature but not representative
#' of the real PC timestamps, you can use this function to recompute their start based
#' to a specific number. This functions zero-bases timestamps in various fields and saves the information inside
#' `obj$info$start_time`. This is to synchronize recordings from other sources. Typical use is
#' transformation of the *Synced Time* and the real *Start Time* from the info file. The `info.csv` is not exported
#' by default and you need to either copy it from the recording folder to the exported folder so the package
#' can load it automatically with `load_folder`, or you can supply the start time yourself.
#' @param obj object which timestamps should be recomputed
#' @param start_time numeric value to be substracted from timestamps
#' @param new_start_time numeric value to be inserted as the new start. Optional, **defaults** to start_time
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
change_timestamps_start <- function(obj, start_time, new_start_time = start_time, ...){
  UseMethod("change_timestamps_start")
}

#' Changes timestamps to a given start time
#' @describeIn change_timestamps_start Changes timestamps in all available data (both main and surface data)
#' @export
change_timestamps_start.pupilr <- function(obj, start_time, new_start_time = start_time){
  obj$data$gaze <- change_gaze_timestamps(obj$data$gaze, start_time)
  obj$data$fixations <- change_fixations_timestamps(obj$data$fixations, start_time)
  obj$surfaces <- change_timestamps_start(obj$surfaces, start_time, new_start_time)
  obj$info$start_time <- new_start_time
  return(obj)
}

#' @describeIn change_timestamps_start Changes timestamps in all sufaces
#' @export
change_timestamps_start.surfaces <- function(obj, start_time, new_start_time = start_time){
  obj$data$events <- change_events_timestamps(obj$events)
  for (name in names(obj$items)){
    obj$items[[name]] <- change_timestamps_start.surface.item(obj$items[[name]], start_time, new_start_time)
  }
  obj$info$start_time <- new_start_time
  return(obj)
}

#' @describeIn change_timestamps_start Changes timestamps in a given surface.item
#' @export
change_timestamps_start.surface.item <- function(obj, start_time, new_start_time = start_time){
  obj$data$gaze <- change_gaze_timestamps(obj$data$gaze, start_time)
  obj$data$fixations <- change_fixations_timestamps(obj$data$fixations, start_time)
  obj$data$positions <- change_gaze_timestamps(obj$data$positions, start_time)
  obj$info$start_time <- new_start_time
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
#' @param ... optional parameters just like in get_gaze_timewindow, get_fixations (e.g. on_surface)
#'
#' @return
#' @export
#'
#' @examples
filter_times <- function(obj, start, end, ...){
  UseMethod("filter_times")
}


#' @describeIn filter_times filters gaze data in a surface.item
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

#' @describeIn as.eyer Converts pupilr object and all nested object to an eyer objects
#' @export
as.eyer.pupilr <- function(obj, ...){
  eye <- list()
  # Converts the main data
  eye$data$gaze <- gaze_to_eyer_df(obj$data$gaze)
  eye$data$fixations <- fixations_to_eyer_df(obj$data$fixations)
  # convects the surfaces
  eye$surfaces <- as.eyer.surfaces(obj$surfaces)
  eye$info <- obj$info
  eye$info$start_time <- ifelse(!is.null(obj$info$start_time), obj$info$start_time, eye$data$gaze$time[1])
  eye$info$eyetracker <- EYETRACKER_NAME
  eye$export_info <- obj$export_info
  class(eye) <- append(class(eye), "eyer")
  return(eye)
}

#' @describeIn as.eyer Converts all nested surfaces objects to eyer objects
#' @export
as.eyer.surfaces <- function(obj, ...){
  for (name in names(obj$items)){
    obj$items[[name]] <- as.eyer.surface.item(obj$items[[name]])
  }
  return(obj)
}

#' @describeIn as.eyer Converts surface item to an eyer object
#' @export
as.eyer.surface.item <- function(obj){
  #TODO - this might have to be redone
  eye <- list()
  eye$data$gaze <- surface_gaze_to_eyer_df(obj$data$gaze)
  eye$data$fixations <- surface_fixations_to_eyer_df(obj$data$fixations)
  eye$info$start_time <- ifelse(!is.null(obj$info$start_time), obj$info$start_time, eye$data$gaze$time[1])
  # TODO - Check is this is ever null?
  if(is.null(obj$info)){
    eye$info <- list()
  } else{
    eye$info <- obj$info
  }
  eye$info$eyetracker <- EYETRACKER_NAME
  class(eye) <- append(class(eye), "eyer")
  return(eye)
}

## NON PUBLIC -----
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

### Timestamp changes----
change_gaze_timestamps <- function(gaze, start_time){
  if(is.null(gaze) || nrow(gaze) == 0) return(gaze)
  if("gaze_timestamp" %in% colnames(gaze)){
    gaze$gaze_timestamp <- gaze$gaze_timestamp - start_time
  }
  if("world_timestamp" %in% colnames(gaze)){
    gaze$world_timestamp <- gaze$world_timestamp - start_time
  }
  return(gaze)
}
change_fixations_timestamps <- function(fixations, start_time){
  if(is.null(fixations) || nrow(fixations) == 0) return(fixations)
  fixations$start_timestamp <- fixations$start_timestamp - start_time
  return(fixations)
}
change_events_timestamps <- function(events, start_time){
  if(is.null(events) || nrow(events) == 0) return(events)
  events$world_timestamp <- events$world_timestamp-start_time
  return(events)
}
### Eyer convetsions ----
gaze_to_eyer_df <- function(gaze){
  if(is.null(gaze) || nrow(gaze) == 0) return(data.frame())
  gaze <- rename_column(gaze, "gaze_timestamp", "time") # in nwe versions this the name
  gaze <- rename_column(gaze, "world_timestamp", "time") # this is the name in old versions
  gaze <- rename_column(gaze, "norm_pos_x", "x")
  gaze <- rename_column(gaze, "norm_pos_y", "y")
  gaze[, c(2, 6:length(colnames(gaze)))] <- NULL
  return(gaze)
}
fixations_to_eyer_df <- function(fixations){
  if(is.null(fixations) || nrow(fixations) == 0) return(data.frame())
  fixations[, c("start_frame_index", "end_frame_index", "method",
                "gaze_point_3d_x",	"gaze_point_3d_y",	"gaze_point_3d_z",
                "base_data", "fixations_id")] <- NULL
  fixations <- rename_column(fixations, "start_timestamp", "time")
  fixations <- rename_column(fixations, "norm_pos_x", "x")
  fixations <- rename_column(fixations, "norm_pos_y", "y")
  return(fixations)
}
surface_gaze_to_eyer_df <- function(gaze){
  if(is.null(gaze) || nrow(gaze) == 0) return(data.frame())
  gaze <- rename_column(gaze, "x_norm", "x")
  gaze <- rename_column(gaze, "y_norm", "y")
  gaze <- rename_column(gaze, "gaze_timestamp", "time")
  gaze[, c("world_index", "world_timestamp",
           "y_scaled", "x_scaled")] <- NULL
  return(gaze)
}
surface_fixations_to_eyer_df <- function(fixations){
  if( is.null(fixations) || nrow(fixations) == 0) return(data.frame())
  fixations[, c("fixations_id", "y_scaled", "x_scaled")] <- NULL
  fixations <- rename_column(fixations, "start_timestamp", "time")
  fixations <- rename_column(fixations, "norm_pos_x", "x")
  fixations <- rename_column(fixations, "norm_pos_y", "y")
  return(fixations)
}
