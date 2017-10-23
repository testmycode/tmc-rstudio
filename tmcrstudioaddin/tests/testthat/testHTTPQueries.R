
test_that("Exercises are downloaded correctly", {

  token <- authenticate("rtest", "asdasdasd")
  download_exercises(token, "36463",
                    "test.zip",
                    "resources/downloaded_exercises/")

  expect_true(dir.exists("resources/downloaded_exercises/Week1"))
})
