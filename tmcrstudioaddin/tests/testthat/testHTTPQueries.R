#TODO Fix this
test_that("Exercises are downloaded correctly", {
  token <- authenticate("rtest", "asdasdasd","https://tmc.mooc.fi")
  download_all_exercises(token = token,
                     exercise_id = "242")

  course_name <- "hy-C-programming-Open"
  user_home <- Sys.getenv("HOME")
  r_home <- file.path(user_home, "tmcr-projects", fsep = .Platform$file.sep)



                     # The path where we want to download the exercises.
  course_directory_path <- file.path(r_home, course_name,
                        fsep = .Platform$file.sep)

  expect_true(dir.exists(course_directory_path))


})
