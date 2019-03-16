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

plot_gaze_heatmap.surface <- function(obj){
  gz <- obj$gaze
  gz <- gz[gz$on_srf =="True", ]

  ggplot(gz, aes(x_norm, y_norm)) +
    #geom_bin2d(binwidth = 1/100) +
    stat_density2d(aes(fill = ..level..), geom = "polygon") +
    scale_fill_viridis_c() +
    xlim(0,1) + ylim(0,1)
}
