#TODO Fix this
test_that("Exercises are downloaded correctly", {
  token <- authenticate("rtest", "asdasdasd", "https://tmc.mooc.fi")
  download_all_exercises(token = token, "242")

  course_name <- "hy-C-Programming-Open"
  user_home <- Sys.getenv("HOME")
  r_home <- file.path(user_home, "tmcr-projects", fsep = .Platform$file.sep)



                     # The path where we want to download the exercises.
  course_directory_path <- file.path(r_home, course_name,
                        fsep = .Platform$file.sep)
  expect_true(dir.exists(course_directory_path))
})
test_that("organizations are fetched from the server", {
  if (file.exists(".credentials.rds")){
    file.remove(".credentials.rds")
  }
  authenticate("rtest", "asdasdasd","https://tmc.mooc.fi")
  organizations<-tmcrstudioaddin::getAllOrganizations()
  expect_true(length(organizations) > 0)
  for (organization in organizations){
    expect_true(is.character(organization))
  }

  if (file.exists(".credentials.rds")){
    file.remove(".credentials.rds")
  }
})
test_that("empty organization list is returned if failed", {
  if (file.exists(".credentials.rds")){
    file.remove(".credentials.rds")
  }
  authenticate("rtest", "asdasdasd", "https://tmc.mooasdc.fi")
  organizations<-tmcrstudioaddin::getAllOrganizations()
  expect_true(length(organizations) == 0)

  if (file.exists(".credentials.rds")){
    file.remove(".credentials.rds")
  }
})
test_that("courses associated with server and organization are fetched from the server", {
  if (file.exists(".credentials.rds")){
    file.remove(".credentials.rds")
  }
  authenticate("rtest", "asdasdasd", "https://tmc.mooc.fi")
  courses<-getAllCourses("hy")
  expect_true(length(courses) > 0)
  for (course in courses){
    expect_true(is.character(course))
  }

  if (file.exists(".credentials.rds")){
    file.remove(".credentials.rds")
  }
})
test_that("empty courses list is returned if failure", {
  if (file.exists(".credentials.rds")){
    file.remove(".credentials.rds")
  }
  authenticate("rtest", "asdasdasd", "https://tmc.moasdoc.fi")
  courses <- getAllCourses("hy")
  expect_true(length(courses) == 0)

  if (file.exists(".credentials.rds")){
    file.remove(".credentials.rds")
  }
})
