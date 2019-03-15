context("loading")

DIR <- "test-data"

test_that("loading of folders work", {
  ls <- load_folder(DIR)
  expect_s3_class(ls, "pupilr")
})
