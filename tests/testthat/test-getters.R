context("getters")

DIR <- "../test-data"

test_that("filtering times works", {
  sf <- open_surfaces(DIR)
  df_gaze <- get_gaze(sf$unnamed)
  expect_type(df_gaze, "list")
  df_gaze_full <- get_gaze(sf$unnamed, on_surface = F)
  expect_type(df_gaze_full, "list")
  expect_lt(nrow(df_gaze), nrow(df_gaze_full))
})
