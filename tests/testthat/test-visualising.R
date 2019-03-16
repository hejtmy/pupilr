context("test-visualising")

DIR <- "../test-data"

test_that("heatmaps work", {
  ls <- open_surfaces(DIR)
  expect_silent(plot_gaze_heatmap(ls$unnamed))
})
