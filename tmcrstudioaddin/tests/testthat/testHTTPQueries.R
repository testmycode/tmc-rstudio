# context("Submitting")
# httptest::with_mock_API({
#   # The needed json is different depending on path of the exercise. Can test locally with the correct json
#   test_that("Submission is uploaded correctly", {
#     token <- login("a", "b", "c", "d", "https://tmc.mooc.fi")
#     ex_id <- 36451
#     resp <- upload_exercise(token, ex_id, "resources/Week0-0_0_helloworld/", "https://tmc.mooc.fi/")
#     content <- httr::content(resp)
#
#     expect_true(is.character(content$submission_url))
#   })
# })

# test_that("Exercises are downloaded correctly", {
#   token <- authenticate("rtest", "asdasdasd", "https://tmc.mooc.fi")
#   download_all_exercises(token = token, "242")
#
#   course_name <- "hy-C-Programming-Open"
#   user_home <- Sys.getenv("HOME")
#   r_home <- file.path(user_home, "tmcr-projects", fsep = .Platform$file.sep)
#
#                      # The path where we want to download the exercises.
#   course_directory_path <- file.path(r_home, course_name,
#                         fsep = .Platform$file.sep)
#   expect_true(dir.exists(course_directory_path))
#
#   if (file.exists(".credentials.rds")){
#     file.remove(".credentials.rds")
#   }
# })

context("Fetching data")

httptest::with_mock_API({
  test_that("courses associated with server and organization are fetched from the server", {

    credentials_path <- paste(get_tmcr_directory(),
        ".credentials.rds", sep = .Platform$file.sep)

    if (file.exists(credentials_path)){
      file.remove(credentials_path)
    }

    # authenticate("rtest", "asdasdasd", "https://tmc.mooc.fi")
    login("a", "b", "c", "d", "tmc.mooc.fi")

    courses <- getAllCourses("hy")
    expect_true(length(courses) > 0)

    for (name in courses$name) {
      expect_true(is.character(name))
    }

    for (id in courses$id) {
      expect_true(is.integer(id))
    }

    if (file.exists(credentials_path)){
      file.remove(credentials_path)
    }
  })

  test_that("organizations are fetched from the server", {
    credentials_path <- paste(get_tmcr_directory(),
        ".credentials.rds", sep = .Platform$file.sep)
    if (file.exists(credentials_path)){
      file.remove(credentials_path)
    }

    # authenticate("rtest", "asdasdasd", "https://tmc.mooc.fi")
    login("a", "b", "c", "d", "tmc.mooc.fi")
    organizations <- tmcrstudioaddin::getAllOrganizations()

    expect_true(length(organizations$name) > 0)

    for (organization in organizations$name){
      expect_true(is.character(organization))
    }

    if (file.exists(credentials_path)){
      file.remove(credentials_path)
    }
  })

  test_that("organizations are fetched from the server", {
    credentials_path <- paste(get_tmcr_directory(),
        ".credentials.rds", sep = .Platform$file.sep)

    if (file.exists(credentials_path)){
      file.remove(credentials_path)
    }

    # authenticate("rtest", "asdasdasd", "https://tmc.mooc.fi")
    login("a", "b", "c", "d", "tmc.mooc.fi")

    organizations <- tmcrstudioaddin::getAllOrganizations()

    expect_true(length(organizations$name) > 0)

    for (organization in organizations$name){
      expect_true(is.character(organization))
    }

    if (file.exists(credentials_path)){
      file.remove(credentials_path)
    }
  })

  test_that("We can get submission details", {
    token <- "asd"
    url <- "https://tmc.mooc.fi/api/v8/core/submissions/1331386"
    response <- get_submission_json(token, url)

    response_content <- httr::content(response)

    expect_equal(response_content$test_cases[[1]]$message, "Passed")
    expect_equal(response_content$points[[1]], "hello")
    expect_equal(response_content$status, "ok")
    expect_equal(response_content$exercise_name, "Week0-0_0_helloworld")
  })

  test_that("Wrong url doesn't give submission details", {
    token <- "asd"
    url <- "https://tmc.mooc.fi/api/v8/core/submissions/136"
    response <- get_submission_json(token, url)

    response_content <- httr::content(response)

    expect_null(response_content$login)
  })

  test_that("empty courses list is returned if failure", {
    if (file.exists(".credentials.rds")){
      file.remove(".credentials.rds")
    }

    authenticate("rtest", "asdasdasd", "https://tmc.moasdoc.fi")
    # login("a", "b", "c", "d", "tmc.moasasddas.fi")

    courses <- getAllCourses("hy")
    expect_true(length(courses$id) == 0)
    expect_true(length(courses$name) == 0)

    if (file.exists(".credentials.rds")){
      file.remove(".credentials.rds")
    }
  })

  test_that("empty organization list is returned if failed", {
    if (file.exists(".credentials.rds")){
      file.remove(".credentials.rds")
    }

    authenticate("rtest", "asdasdasd", "https://tmc.mooasdc.fi")
    organizations <- tmcrstudioaddin::getAllOrganizations()
    expect_true(length(organizations$slug) == 0)
    expect_true(length(organizations$name) == 0)

    if (file.exists(".credentials.rds")){
      file.remove(".credentials.rds")
    }
  })
})
