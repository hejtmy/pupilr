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
  class(ls) <- append(class(ls), "pupilr")
  return(ls)
}
