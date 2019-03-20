#' Returns gaze data.frame
#'
#' @param obj
#' @param ... optional parameters
#'
#' @return
#' @export
#'
#' @examples
get_gaze <- function(obj, ...){
  UseMethod("get_gaze")
}

#' Returns gaze data.frame from the surface object
#'
#' @param obj surface object
#' @param on_surface should it only return those measurements on surface?
#' @param ...
#'
#' @return data.frame with gaze
#' @export
#'
#' @examples
get_gaze.surface <- function(obj, on_surface = T, ...){
  gaze <- obj$gaze
  if(on_surface) gaze <- gaze[gaze$on_srf == "True", ]
  return(gaze)
}

#' Returns
#'
#' @param obj
#'
#' @return
#' @export
#'
#' @examples
get_gaze_timewindow <- function(obj, start, end, ...){
  if(any(c(missing(start), missing(end)))) stop("start and end need to be defned")
  UseMethod("get_gaze_timewindow")
}

#' returns gaze data.frame between given times
#'
#' @param obj surface object
#' @param start start time in seconds (numeric)
#' @param end end time in seconds (numeric)
#' @param since_start defaults to false
#' @param ... optional parameters, same as in get_gaze
#'
#' @return filtered data.frame
#' @export
#'
#' @examples
#' gaze <- get_gaze_timewindow(surface_object, 1000, 1200, on_surface = F)
get_gaze_timewindow.surface <- function(obj, start, end, since_start = F, ...){
  if(any(!is.numeric(c(start, end)))) stop('start and end needs to be numeric')
  gaze <- get_gaze.surface(obj, ...)
  #TODO - check for gaze to be present?
  timestamps <- gaze$world_timestamp
  if(since_start) timestamps <- timestamps - timestamps[1]
  gaze <- gaze[timestamps > start & timestamps < end, ]
  return(gaze)
}
