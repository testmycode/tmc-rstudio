#TODO Fix this
test_that("Exercises are downloaded correctly", {
  token <- authenticate("rtest", "asdasdasd","https://tmc.mooc.fi")
  download_all_exercises(token = token, "242")

  course_name <- "hy-C-Programming-Open"
  user_home <- Sys.getenv("HOME")
  r_home <- file.path(user_home, "tmcr-projects", fsep = .Platform$file.sep)



                     # The path where we want to download the exercises.
  course_directory_path <- file.path(r_home, course_name,
                        fsep = .Platform$file.sep)
  expect_true(dir.exists(course_directory_path))

})

test_that("Submission details are downloaded correctly", {
  token <- authenticate("rtest", "asdasdasd","https://tmc.mooc.fi")
  url <- "https://tmc.mooc.fi/api/v8/core/submissions/1331386"
  response <- get_submission_json(token, url)

  expect_equal(response$test_cases[[1]]$message, "Passed")
  expect_equal(response$points[[1]], "hello")
  expect_equal(response$status, "ok")
  expect_equal(response$exercise_name, "Week0-0_0_helloworld")
})
