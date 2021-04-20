# pupilr
R package to working with Pupil labs eyetracker data in R. The purpose of this package is to load and export the data into [eyer][https://github.com/hejtmy/eyer] package which does all the other extra work.

## What the package does?
Pupilr loads and preprocesses exported data from the pupil labs eyetracker for easy visualisation and analysis. It expects certain format to be present, as is defined by the pupil labs export functions.

Pupilr offers ways to synchronise world timestamps, clear out surface data, filter data based on timestamps, and uses ggplot for quick and easy visualisations.

## Requirements
Pupilr expects the data to be "Exported" through pupil labs player or command line and loads ONLY exported data. Reading and preprocessing raw data is technically possible, but especially as the surface data are concerned, it is just simpler to load already prepared files by pupil pipeline. 

The package expects files to be in their original places with no changes to naming. It searches for particularly named files as the pupil labs exports and any change might lead to loading failure.

For extended functionality, it is recommended that you copy the `info.csv` file from the pupil recording inside your exported folder, as it contains information which is generally lost in the process of export.

## Installation
At this point `pupilr` is not on CRAN, but can be installed using devtools or other tools allowing to install packages from github

```
devtools::install_github("hejtmy/pupilr")
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

