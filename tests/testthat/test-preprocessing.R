context("test-preprocessing")
DIR <- "../test-data"
sf <- open_surfaces(DIR)
obj <- load_folder(DIR)

test_that("reseting times", {
  expect_silent(unnamed_new <- change_timestamps_start(sf$items$unnamed, 0))
  expect_equal(unnamed_new$data$fixations, sf$items$unnamed$data$fixations)
  expect_silent(unnamed_new <- change_timestamps_start(sf$items$unnamed, 100))
  expect_s3_class(unnamed_new, "surface.item")
  expect_equal(unnamed_new$info$start_time, 100)
  expect_silent(obj_new <- change_timestamps_start(obj, 100))
  expect_s3_class(obj_new, "pupilr")
  expect_equal(obj_new$info$start_time, 100)
  expect_equal(obj_new$surfaces$info$start_time, 100)
  expect_equal(obj_new$surfaces$items$unnamed$info$start_time, 100)

  ## Setting new start time
  expect_silent(obj_new <- change_timestamps_start(obj, obj$data$gaze$world_timestamp[1], 100))
  expect_equal(obj_new$data$gaze$world_timestamp[1], 0)
  expect_equal(obj_new$info$start_time, 100)
  expect_equal(obj_new$surfaces$info$start_time, 100)
  expect_equal(obj_new$surfaces$items$unnamed$info$start_time, 100)
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
