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
  a <- open_exported_file(DIR, "_fake_name")
  expect_null(a)
})
