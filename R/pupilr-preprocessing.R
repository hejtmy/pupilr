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
#' @param start_time needs \%H:\%M:\%S parameter to be passed
#'
#' @return
#' @export
#'
#' @examples
change_timestamps_start.surface <- function(obj, start_time){
  timestamps <- obj$gaze$world_timestamp
  first_timestamp <- timestamps[1]
  first_timestamp_ms <- first_timestamp - round(first_timestamp)
  time_since_midnight <- as.numeric(as.difftime(c(start_time, "0:0:0"), "%H:%M:%S", units = "secs"))[1] +
    first_timestamp_ms
  new_timestamps <- time_since_midnight + timestamps - first_timestamp
  obj$gaze$world_timestamp <- new_timestamps
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


