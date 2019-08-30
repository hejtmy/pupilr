# pupilr
R package to working with Pupil labs eyetracker data in R.

## Example code
```
library(pupilr)
sf <- open_surfaces("path_to_your_exported_data")
main_surface <- sf$main
plot_gaze_heatmap(main_surface)
filter_times(main_surface, 0, 500, since_start = T, on_surface = T)
plot_gaze_heatmap(main_surface)
```

## What the package does?
Pupilr loads and preprocesses exported data from the pupil labs eyetracker for easy visualisation and analysis. It expects certain format to be present, as is defined by the pupil labs export functions. 

Pupilr offers ways to synchronise world timestamps, clear out surface data, filter data based on timestamps, and uses ggplot for quick and easy visualisations.

## Requirements
The package visualisation at this point is based on ggplot2. Pupilr also at this point expects the data to be "Exported" through pupil labs player or command line and loads ONLY exported data. Reading and preprocessing raw data is technically possible, but especially as the surface data are concerned, it is just simpler to load already prepared files by pupil pipeline. For extended functionality, it is recommended that you copy the `info.csv` file from the pupil recording insode your exported folder, as it contains information which is generally lost in the process of export.

## Installation
At this point `pupilr` is not on CRAN, but can be installed using devtools or other tools allowing to install packages from github

```
devtools::install_github("hejtmy/pupilr")
```
