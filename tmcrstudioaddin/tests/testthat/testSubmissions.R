testResources <- paste(sep = "", getwd(), "/resources")

#data for testing:
noMetadata <- paste0(testResources, "/noMetadata")
simpleProject <- paste0(testResources, "/simpleProject")
submissionJson <- paste0(testResources, "/submission.json")
submissionErrorJson <- paste0(testResources, "/submissionError.json")
submissionProcessingJson <- paste0(testResources, "/submissionProcessing.json")
submissionPartlyFailedJson <- paste0(testResources, "/submissionPartlyFailed.json")

mockSubmitJson <- function() {
  submitJson <- list()
  submitJson$results <- jsonlite::fromJSON(submissionJson, simplifyVector = FALSE)
  return(submitJson)
}

mockErrorSubmitJson <- function() {
  submitJson <- list()
  submitJson$results <- jsonlite::fromJSON(submissionErrorJson, simplifyVector = FALSE)
  return(submitJson)
}

mockProcessingSubmitJson <- function() {
  submitJson <- list()
  submitJson$results <- jsonlite::fromJSON(submissionProcessingJson, simplifyVector = FALSE)
  return(submitJson)
}

test_that("If no metadata file, does not submit", {
  submitJson <- submitCurrent(noMetadata)
  expect_equal(submitJson$error, "Could not read json")
})

test_that("SubmitCurrent works when no errors", {
  path <- "path"
  stub(submitCurrent, "tmcrstudioaddin::getCredentials", list(token = "abc"))
  stub(submitCurrent, "upload_current_exercise", list(error = NULL))
  stub(submitCurrent, "getExerciseFromServer", mockSubmitJson())
  submitJson <- submitCurrent(path)
  expect_equal(submitJson$results$status, "ok")
  testCases <- submitJson$results$test_cases
  expect_equal(length(testCases), 2)
})

test_that("Exercise data is gotten correctly", {
  response <- "url"
  token <- "abc"
  mock <- mock(mockProcessingSubmitJson(), mockSubmitJson())
  stub(getExerciseFromServer, "get_json_from_submission_url", mock)
  submitJson <- getExerciseFromServer(response, token, 0.1)
  expect_equal(submitJson$results$status, "ok")
  expect_equal(submitJson$error, NULL)
})

test_that("Errors from the server are reported correctly when collecting exercise data", {
  response <- "url"
  token <- "abc"
  mock <- mock(mockProcessingSubmitJson(), mockErrorSubmitJson())
  stub(getExerciseFromServer, "get_json_from_submission_url", mock)
  submitJson <- getExerciseFromServer(response, token, 0.1)
  expect_equal(submitJson$results$status, "error")
  expect_equal(submitJson$error, "Internal Server Error")
})

test_that("Json is processed correctly", {
  json <- jsonlite::fromJSON(submissionPartlyFailedJson, simplifyVector = FALSE)
  submitRes <- processSubmissionJson(json)
  expect_equal(length(submitRes$tests), 2)
  expect_equal(submitRes$tests[[1]]$name, "hello-world test_hello")
  expect_equal(submitRes$tests[[1]]$status, "pass")
  expect_equal(submitRes$tests[[2]]$name, "hello-world is_hello")
  expect_equal(submitRes$tests[[2]]$status, "fail")
  expect_equal(submitRes$exercise_name, "Week0-0_0_helloworld")
  expect_equal(submitRes$all_tests_passed, FALSE)
})

test_that("Dialog message outputs error correctly", {
  submitResults <- list()
  submitResults$data$points <- list("r1", "r2")
  submitResults$data$exercise_name <- "project1"
  submitResults$error <- "<Some url error>"
  message <- getDialogMessage(submitResults)
  expect_equal(message$text, paste0("<p>","<Some url error>"))
})

test_that("Dialog message outputs all tests passed correctly", {
  submitResults <- list()
  submitResults$data$points <- list("r1", "r2")
  submitResults$data$all_tests_passed <- TRUE
  submitResults$data$exercise_name <- "project1"
  message <- getDialogMessage(submitResults)
  expect_equal(message$text, paste0("All tests passed on the server.<p><b>Points permanently awarded: ",
                                    "r1, r2</b><p>View model solution"))
})

test_that("Dialog message outputs some tests passed correctly", {
  submitResults <- list()
  submitResults$data$points <- list("r1", "r2")
  submitResults$data$all_tests_passed <- FALSE
  submitResults$data$exercise_name <- "project1"
  message <- getDialogMessage(submitResults)
  expect_equal(message$text, paste0("Exercise project1 failed partially.<p><b>Points permanently awarded: ",
                                    "r1, r2</b><p>Some tests failed on the server.<p>Press OK to see failing tests"))
})
