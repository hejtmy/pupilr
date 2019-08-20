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
SurfacesObject <- function(){
  ls <- list()
  ls$data <- list()
  ls$items <- list()
  class(ls) <-append(class(ls), "surfaces")
  return(ls)
}


#' Creates surface item object
#'
#' @return
#' @export
#'
#' @examples
SurfaceItemObject <- function(){
  ls <- list()
  ls$data <- list()
  class(ls) <-append(class(ls), "surface.item")
  return(ls)
}
