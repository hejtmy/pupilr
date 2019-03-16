context("loading")

DIR <- "../test-data"

test_that("loading of folders work", {
  ls <- load_folder(DIR)
  expect_s3_class(ls, "pupilr")
})

test_that("finding files work", {
  expect_false(is.null(open_exported_file(DIR, "world_timestamps.csv")))
  expect_silent(open_exported_file(DIR, "world_timestamps.csv"))
  expect_warning(open_exported_file(DIR, "_fake_name"))
  expect_null(open_exported_file(DIR, "_fake_name"))
})

test_that("loading surfaces", {

})

test_that("loading exported csv", {
  #gaze
  expect_silent(ls <- open_gaze_file(DIR))
  expect_type(ls, "list")
  #info
  expect_silent(ls <- open_info_file(DIR))
  expect_type(ls, "list")
  #positions
  expect_silent(ls <- open_positions_file(DIR))
  expect_type(ls, "list")
  #timestamps
  expect_silent(ls <- open_timestamps_file(DIR))
  expect_type(ls, "list")
})
