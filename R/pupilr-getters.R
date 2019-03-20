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
  if(on_surface) gaze <- gaze[gaze$on_srf == "True",]
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
get_gaze_timewindow <- function(obj, ...){
  UseMethod("get_gaze_timewindow")
}

#' Title
#'
#' @param obj
#' @param ... optional parameters, same as in get_gaze
#'
#' @return
#' @export
#'
#' @examples
get_gaze_timewindow.surface <- function(obj, ...){
  gaze <- get_gaze.surface(obj, ...)
  return(gaze)
}
