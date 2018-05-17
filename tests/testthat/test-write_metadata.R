context("write_metadata")

test_that("empty arguments fail", {
  expect_error(write_metadata(), regexp = "must provide.*list")
  expect_error(write_metadata(list()), regexp = "metadata \\(filename\\)")
})

test_that("wrong filetype fails", {
  expect_warning(write_metadata(list(), "file.xls"), regexp = "either a \\.json")
})

test_that("basic yaml writing works", {
  temp_file <- tempfile(fileext = ".yaml")
  write_metadata(list(a = 123.4, foo = "bar"), temp_file)
  dat <- yaml::read_yaml(temp_file)
  expect_identical(dat$a, 123.4)
  expect_identical(dat$foo, "bar")
  unlink(temp_file)
})

test_that("basic json writing works", {
  temp_file <- tempfile(fileext = ".json")
  write_metadata(list(a = 123.4, foo = "bar"), temp_file)
  dat <- jsonlite::read_json(temp_file, simplifyVector = TRUE)
  expect_identical(dat$a, 123.4)
  expect_identical(dat$foo, "bar")
  unlink(temp_file)
})