context("loading")

DIR <- "../test-data"

test_that("loading of folders work", {
  ls <- load_folder(DIR)
  expect_s3_class(ls, "pupilr")
})


test_that("finding files work", {
  expect_type(find_info_file(DIR), "character")
})
