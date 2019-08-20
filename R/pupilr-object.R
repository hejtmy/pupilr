#' Pupilr object
#' @description
#'
#' @return
#' @export
#'
#' @examples
PupilrObject <- function(){
  ls <- list()
  ls$data <- list()
  ls$info <- list()
  ls$surfaces <- list()
  class(ls) <- append(class(ls), "pupilr")
  return(ls)
}

#' Creates surface object
#'
#' @return
#' @export
#'
#' @examples
SurfaceObject <- function(){
  ls <- list()
  ls$data <- list()
  ls$items <- list()
  class(ls) <-append(class(ls), "surface")
  return(ls)
}
