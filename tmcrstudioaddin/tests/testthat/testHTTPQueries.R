#TODO Fix this
test_that("Exercises are downloaded correctly", {
  token <- authenticate("rtest", "asdasdasd","https://tmc.mooc.fi")
  download_exercise(token = token,
                     exercise_id = "36463",
                     exercise_directory = "resources/downloaded_exercises/")
  expect_true(TRUE)
})
