test_that("Exercises are downloaded correctly", {
  token <- authenticate("rtest", "asdasdasd")
  download_exercises(token = token,
                     exercise_id = "36463",
                     exercise_directory = "resources/downloaded_exercises/")
  expect_true(dir.exists("resources/downloaded_exercises/Week1"))
})
