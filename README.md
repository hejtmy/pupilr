# pupilr
R package to working with Pupil labs eyetracker data in R. The purpose of this package is to load and export the data into [eyer][https://github.com/hejtmy/eyer] package which does all the analysis work, such as ROI analysis or visualisations

## What the package does?
Pupilr loads and preprocesses exported data from the pupil labs eyetracker for easy visualisation and analysis. It expects certain format to be present, as is defined by the pupil labs export functions.

Pupilr offers ways to synchronise world timestamps, clear out surface data,and uses ggplot for quick and easy visualisations.

## Requirements
Pupilr expects the data to be "Exported" through pupil labs player or command line and loads ONLY exported data. Reading and preprocessing raw data is technically possible, but especially as the surface data are concerned, it is just simpler to load already prepared files by pupil pipeline. 

The package expects files to be in their original places with no changes to naming. It searches for particularly named files as the pupil labs exports and any change might lead to loading failure.

For extended functionality, it is recommended that you copy the `info.csv` file from the pupil recording inside your exported folder, as it contains information which is generally lost in the process of export.

## Installation
At this point `pupilr` is not on CRAN, but can be installed using devtools or other tools allowing to install packages from github
```
devtools::install_github("hejtmy/pupilr")
```

## Basic overview
The package uses S3 classes to allow similar handling of surface and non surface data with functions such as `get_gaze`. PLease refer to the documentation.


Pupilr loads the data into a list with a class `pupilr`. The list contains the infromation from exported files, gaze and fixation data etc. In case the surfaces export is present, the object populates the surfaces field as well. The field has `surfaces` class and each individual surface can be found in the `items` field and has class `surface.item`.

```
eye <- load_folder("path to your exported data folder")
sf <- eye$surfaces$items$main_surface
sf$data$fixations
sf$data$gaze
```

## Example code

```
library(pupilr)
eye <- load_folder("path to your exported data folder")
str(eye)
```

```
plot_gaze(eye, downsample = 10)
plot_gaze(eye, downsample = 10, surface = "main_surface")
```

```
eyer_eye <- as.eyer(eye)
eyer::plot_gaze(eyer_eye)
eyer_surface <- as.eyer(eye$surfaces$items$main_surface)
eyer::plot_gaze(eyer_surface)
```

