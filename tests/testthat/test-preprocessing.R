context("test-preprocessing")
DIR <- "../test-data"
sf <- open_surfaces(DIR)
obj <- load_folder(DIR)

test_that("reseting times", {
  expect_silent(unnamed_new <- change_timestamps_start(sf$items$unnamed, "01:54:02"))
  expect_silent(unnamed_new <- change_timestamps_start(sf$items$unnamed, "01:54:02.100"))
})

test_that("filtering times", {
  filtered <- filter_times(sf$items$unnamed, 0, 10, on_surface = T, since_start = T)
  expect_gt(nrow(sf$items$unnamed$data$gaze), nrow(filtered$data$gaze))
})

test_that("preprocessing surfaces", {

})

test_that("Eyer conversions", {
  eye <- as.eyer(sf$items$unnamed)
  expect_s3_class(eye, "eyer")
  expect_true(all(c("x", "y", "time") %in% names(eye$data$gaze)))
  sf_eye <- as.eyer(sf)
  expect_s3_class(sf_eye$items$unnamed, "eyer")
  eye <- as.eyer(obj)
  expect_s3_class(eye, "eyer")
  expect_s3_class(eye$surfaces$items$unnamed, "eyer")
})
