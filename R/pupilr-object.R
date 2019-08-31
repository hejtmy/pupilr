#' Pupilr object definition
#'
#' @description Pupilr object is a list with class pupilr with three main fields
#' - info: contains eyetracker settings embedded in the info.csv. This file is generally not in the export folder, btt in the main recording folder and needs to be moved
#' - export_info: contains list with export information
#' - data: loaded gata from the exported folder. Genrally contains fields fixations, gaze, pupil
#' - surfaces: contains a list with `SurfaceObjects`, one for each exported surface.
#'
#' @return object of class `pupilr`
#' @export
#'
#' @examples
PupilrObject <- function(){
  ls <- list()
  ls$data <- list()
  ls$info <- list()
  ls$export_info <- list()
  ls$surfaces <- list()
  class(ls) <- append(class(ls), "pupilr")
  return(ls)
}

#' Creates surfaces object
#'
#' @description Surfaces object contains basic data from the surfaces folder
#' (generally events table with information about surfaces enter adn exit).
#'
#' SurfaceObject:
#' - data: data about all surfaces together
#' - items: list with named field for each surface containig `SurfaceItemObject``
#' - info: list with extra info, e.g. "start_time"
#' @return object of class `surfaces`
#' @export
#'
#' @examples
SurfacesObject <- function(){
  ls <- list()
  ls$data <- list()
  ls$info <- list(start_time = NA)
  ls$items <- list()
  class(ls) <-append(class(ls), "surfaces")
  return(ls)
}

#' Creates surface.item object
#'
#' @description This object contains basic eye information about the particular surface.
#'
#' SurfaceItemObject:
#' - data: gaze, fixations and events for given surface
#' - info: other relevant info (e.g. start time )
#'
#' @return object of class `surface.item`
#' @export
#'
#' @examples
SurfaceItemObject <- function(){
  ls <- list()
  ls$data <- list()
  ls$info <- list(start_time = NA)
  class(ls) <- append(class(ls), "surface.item")
  return(ls)
}
