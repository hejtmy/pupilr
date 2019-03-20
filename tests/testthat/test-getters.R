context("getters")

DIR <- "../test-data"
sf <- open_surfaces(DIR)

test_that("getting surface gaze works", {
  df_gaze <- get_gaze(sf$unnamed)
  expect_type(df_gaze, "list")
  df_gaze_full <- get_gaze(sf$unnamed, on_surface = F)
  expect_type(df_gaze_full, "list")
  expect_lt(nrow(df_gaze), nrow(df_gaze_full))
})

test_that("filtering surface timewindow works", {
  expect_error(get_gaze_timewindow(sf$unnamed, 1000))
  gaze <- get_gaze_timewindow(sf$unnamed, 0, 1000)
  expect_equal(nrow(gaze), 0)
  gaze <- get_gaze_timewindow(sf$unnamed, 0, 1000, since_start = T)
  expect_gt(nrow(gaze), 0)
  gaze_on_srf <- get_gaze_timewindow(sf$unnamed, 0, 1000, since_start = T, on_surface = T)
  gaze_out_srf <- get_gaze_timewindow(sf$unnamed, 0, 1000, since_start = T, on_surface = F)
  expect_gt(nrow(gaze_out_srf), nrow(gaze_on_srf))
})
