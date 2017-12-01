test_that("Properties created", {
  create_properties_file()
  expect_true(check_if_properties_exist())
})

test_that("TMC's home directory is correct", {
  user_home <- Sys.getenv("HOME")
  tmcr_directory <- file.path(user_home, "tmcr")

  expect_equal(tmcr_directory,
      get_tmcr_directory())
})
