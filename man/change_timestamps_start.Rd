% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pupilr-preprocessing.R
\name{change_timestamps_start}
\alias{change_timestamps_start}
\alias{change_timestamps_start.pupilr}
\alias{change_timestamps_start.surfaces}
\alias{change_timestamps_start.surface.item}
\title{Changes timestamps to a given start time}
\usage{
change_timestamps_start(obj, start_time, new_start_time = start_time, ...)

\method{change_timestamps_start}{pupilr}(obj, start_time, new_start_time = start_time)

\method{change_timestamps_start}{surfaces}(obj, start_time, new_start_time = start_time)

\method{change_timestamps_start}{surface.item}(obj, start_time, new_start_time = start_time)
}
\arguments{
\item{obj}{object which timestamps should be recomputed}

\item{start_time}{numeric value to be substracted from timestamps}

\item{new_start_time}{numeric value to be inserted as the new start. Optional, \strong{defaults} to start_time}

\item{...}{}
}
\value{

}
\description{
Zero bases timestamps (substracts \code{start_time}) and saves new start time information in the \code{obj$info$start_time}
}
\section{Methods (by class)}{
\itemize{
\item \code{pupilr}: Changes timestamps in all available data (both main and surface data)

\item \code{surfaces}: Changes timestamps in all sufaces

\item \code{surface.item}: Changes timestamps in a given surface.item
}}

