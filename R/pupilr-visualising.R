#' Title
#'
#' @return
#' @export
#'
#' @examples
plot_gaze <- function(){
  UseMethod("plot_gaze")
}

#' Plots heatmap of the gaze data
#'
#' @param obj either pupilr object or surface object
#'
#' @return
#' @export
#'
#' @examples
plot_gaze_heatmap <- function(obj){
  UseMethod("plot_gaze_heatmap")
}

#' PLots heatmap of a surface object
#'
#' @param obj object of type surface
#'
#' @return ggplot of stat density type with given limits
#' @export
#'
#' @examples
plot_gaze_heatmap.surface.item <- function(obj){
  #require  ggplot
  gz <- get_gaze.surface.item(obj)
  plot_eye_heatmap(gz$x_norm, gz$y_norm)
}

#' Generic function to plot eye heatmap given passed x and y coordinates
#'
#' @param x x coordinates
#' @param y y coordinates
#'
#' @return ggplot og stat density
#' @export
#'
#' @examples
plot_eye_heatmap <- function(x, y){
  #validate - x and y same length
  df <- data.frame(x, y)
  ggplot(df, aes(x, y)) +
    stat_density2d(aes(fill = ..level..), geom = "polygon") +
    scale_fill_viridis_c() +
    xlim(0,1) + ylim(0,1)
}
