test_that("Properties created", {
  backup_properties_file()

  create_properties_file()
  expect_true(check_if_properties_exist())

  restore_properties_file_backup()
})

test_that("TMC's home directory is correct", {
  backup_properties_file()

  user_home <- Sys.getenv("HOME")
  tmcr_directory <- file.path(user_home, "tmcr")

  expect_equal(tmcr_directory,
      get_tmcr_directory())

  restore_properties_file_backup()
})

test_that("Properties are read as expected", {
  backup_properties_file()

  create_properties_file()
  properties <- read_properties()

  expect_equal(properties$`tmcr-dir`,
      paste(tmcrstudioaddin::get_tmcr_directory(), "tmcr-projects",
            sep = .Platform$file.sep))

  restore_properties_file_backup()
})

test_that("get_projects_folder-function works as expected", {
  backup_properties_file()

  create_properties_file(tmcr_projects = "tmcproj")
  tmcr_path <- get_tmcr_directory()
  project_path <- paste(tmcr_path, "tmcproj", sep = .Platform$file.sep)
  expect_equal(get_projects_folder(), project_path)

  restore_properties_file_backup()
})
