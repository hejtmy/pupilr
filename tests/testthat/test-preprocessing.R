context("test-preprocessing")
DIR <- "../test-data"
sf <- open_surfaces(DIR)

test_that("reseting times", {
  expect_silent(unnamed_new <- change_timestamps_start(sf$unnamed, "01:54:02"))
  expect_silent(unnamed_new <- change_timestamps_start(sf$unnamed, "01:54:02.100"))
})

test_that("filtering times", {
  filtered <- filter_times(sf$unnamed, 0,1000, on_surface = T, since_start = T)
  expect_gt(nrow(sf$unnamed$gaze), nrow(filtered$gaze))
})
