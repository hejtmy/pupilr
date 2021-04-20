#' Pot gaze data
#'
#' @param obj pupilr object
#' @param downsample_rate rate of downsample (2 = 50 percent, 10 = 10 percent)
#' @param surface_name
#'
#' @return
#' @export
#'
#' @examples
plot_gaze_data <- function(obj, downsample_rate = 0, surface_name = NULL){
  dat <- get_gaze_eyer(obj, surface_name)
  gaze <- downsample(dat$data$gaze, downsample_rate)
  ggplot(gaze, aes(x, y)) + geom_point()
}

get_gaze_eyer <- function(obj, surface_name){
  if(!is.null(surface_name)){
    obj <- get_surface(obj, surface_name)
  }
  dat <- as.eyer(obj)
  return(dat)
}
