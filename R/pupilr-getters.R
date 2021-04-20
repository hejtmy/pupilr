#' Returns gaze data.frame
#'
#' @param obj pupilr or surface.item object
#' @param ... optional parameters
#'
#' @return data.frame with gaze
#' @export
#'
#' @examples
get_gaze <- function(obj, ...){
  UseMethod("get_gaze")
}

get_gaze.pupilr <- function(obj){
  return(obj$data$gaze)
}

#' Returns gaze data.frame from the surface object
#'
#' @param on_surface should it only return those measurements on surface?
#'
#' @export
#'
#' @describeIn get_gaze Gets gaze data from given surface item
#' @examples
get_gaze.surface.item <- function(obj, on_surface = TRUE){
  gaze <- obj$data$gaze
  if(on_surface) gaze <- gaze[gaze$on_surface, ]
  return(gaze)
}

#' Returns gaze data in a given timewindows
#'
#' @param obj pupilr or surface object
#' @param start start time in seconds (numeric)
#' @param end end time in seconds (numeric)
#' @param ...
#'
#' @return gaze data.frame between given times
#' @export
#'
#' @examples
get_gaze_timewindow <- function(obj, start, end, ...){
  if(any(c(missing(start), missing(end)))) stop("start and end need to be defned")
  UseMethod("get_gaze_timewindow")
}

#' @param since_start defaults to false
#' @export
#' @describeIn get_gaze_timewindow implementation for the surface.item
#'
#' @examples
#' gaze <- get_gaze_timewindow(surface_object, 1000, 1200, on_surface = F)
get_gaze_timewindow.surface.item <- function(obj, start, end, since_start = F, ...){
  if(any(!is.numeric(c(start, end)))) stop('start and end needs to be numeric')
  gaze <- get_gaze.surface.item(obj, ...)
  #TODO - check for gaze to be present?
  timestamps <- gaze$gaze_timestamp
  if(since_start) timestamps <- timestamps - timestamps[1]
  gaze <- gaze[timestamps > start & timestamps < end, ]
  return(gaze)
}

#' Returns names of all surfaces in the object
#'
#' @param obj pupilr object
#'
#' @return character(0) if no surfaces are found or vector of character names
#' @export
#'
#' @examples
get_surface_names <- function(obj){
  if(is.null(obj$surfaces)) return(character(0))
  return(names(obj$surfaces))
}

#' Returns sufface of a given name
#'
#' @param obj pupilr object
#' @param surface_name name of the surface to be extracted
#'
#' @return
#' @export
#'
#' @examples
get_surface <- function(obj, surface_name){
  return(obj$surfaces$items[[surface_name]])
}

#' Returns if the object has class pupilr
#'
#' @param obj object ot be evaluated
#'
#' @return
#' @export
#'
#' @examples
is.pupilr <- function(obj){
  return("pupilr" %in% class(obj))
}
