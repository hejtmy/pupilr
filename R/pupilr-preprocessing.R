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
  timestamps <- obj$data$gaze$world_timestamp
  first_timestamp <- timestamps[1]
  time_since_midnight <- as.numeric(as.difftime(c(start_time, "0:0:0"), "%H:%M:%OS", units = "secs"))[1]
  new_timestamps <- time_since_midnight + timestamps - first_timestamp
  obj$data$gaze$world_timestamp <- new_timestamps
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
  obj$gaze <- gaze
  return(obj)
}

## EYER CONVERTIONS ----
convert_to_eyer <- function(obj, ...){
  UseMethod("convert_to_eyer")
}

convert_to_eyer.surface.item <- function(obj, ...){

}

### NON PUBLIC -----
preprocess.surface.item <- function(obj, ...){
  colnames(obj$data$fixations)[colnames(obj$data$fixations) %in% c("on_surf", "on_srf")] <- "on_surface"
  colnames(obj$data$gaze)[colnames(obj$data$gaze) %in% c("on_surf", "on_srf")] <- "on_surface"
  obj$data$fixations$on_surface <- obj$data$fixations$on_surface == "True"
  obj$data$gaze$on_surface <- obj$data$gaze$on_surface == "True"
  return(obj)
}
