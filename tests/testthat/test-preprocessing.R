context("test-preprocessing")
DIR <- "../test-data"
test_that("reseting times", {
  sf <- open_surfaces(DIR)
  expect_silent(unnamed_new <- change_timestamps_start(sf$unnamed, "01:54:02"))
})
