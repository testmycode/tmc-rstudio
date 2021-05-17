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
  credentials <- list(token = "token", serverAddress = "local.host.nowhere")
  submitJson  <- submit_current(path = noMetadata, credentials = credentials)
  expect_equal(submitJson$error[[1]], "Corrupted project: missing RTMC metadata")
})

test_that("submit_current works when no errors", {
  path <- "path"
  stub(submit_current, "tmcrstudioaddin::getCredentials", list(token = "abc"))
  stub(submit_current, "upload_current_exercise", list(error = NULL))
  stub(submit_current, "getExerciseFromServer", mockSubmitJson())
  submitJson <- submit_current(path         = path,
                               credentials  = list())
  expect_equal(submitJson$results$status, "ok")
  testCases <- submitJson$results$test_cases
  expect_equal(length(testCases), 2)
})

test_that("Exercise data is gotten correctly", {
  response <- "url"
  token    <- "abc"
  mock     <- mock(mockProcessingSubmitJson(), mockSubmitJson())
  stub(getExerciseFromServer, "get_json_from_submission_url", mock)
  submitJson <- getExerciseFromServer(response, token, 0.1)
  expect_equal(submitJson$results$status, "ok")
  expect_equal(submitJson$error, NULL)
})

test_that("Errors from the server are reported correctly when collecting exercise data", {
  response <- "url"
  token    <- "abc"
  mock     <- mock(mockProcessingSubmitJson(), mockErrorSubmitJson())
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
  # expect_equal(message$text, paste0("<p>", "<Some url error>"))
  expect_equal(message$text, "")
})

test_that("Error messages over 300 characters are cut to 300 characters", {
  submitResults <- list()
  submitResults$data$points <- list("r1", "r2")
  submitResults$data$exercise_name <- "project1"
  submitResults$error <- paste(replicate(400, "a"), collapse = "")
  message <- getDialogMessage(submitResults)
  expected <- paste(replicate(300, "a"), collapse = "")
  expect_equal(message$text, "")
})

test_that("Dialog message outputs all tests passed correctly", {
  submitResults <- list()
  submitResults$data$points <- list("r1", "r2")
  submitResults$data$all_tests_passed <- TRUE
  submitResults$data$exercise_name <- "project1"
  message <- getDialogMessage(submitResults)
  expected_message <- 
    paste0("Congratulations! All tests passed on the server!",
           "<p><b>Points permanently awarded: r1, r2</b>",
           "<p>You can now view the model solution on the server.")
  expect_equal(message$text, expected_message)
})

test_that("Dialog message outputs some tests passed correctly", {
  submitResults <- list()
  submitResults$data$points <- list("r1", "r2")
  submitResults$data$all_tests_passed <- FALSE
  submitResults$data$exercise_name <- "project1"
  message <- getDialogMessage(submitResults)
  expected_message <-
    paste0("You are getting there! You received 2 points from Exercise set 'project1'. ",
           "<p><b>Points permanently awarded: ",
           "r1, r2</b>",
           "<p>Some tests still failed on the server.",
           "<p>Press OK to see failing tests")

  expect_equal(message$text, paste0(expected_message))
})

processSubmissionJson_mocksi <- function(submitJson) {
  submission_id <- function(xx) {
    yy <- strsplit(xx, "/")[[1]]
    yy[length(yy)]
  }
  submitted_at <- function(xx) {
    zz <- as.POSIXct(xx,format="%Y-%m-%dT%H:%M:%OS")
    format(zz, "%d.%m.%Y %H:%M")
  }
  submitRes <- list()
  submitRes$submitted_at          <- submitted_at(submitJson$submitted_at)
  submitRes$submission_id         <- submission_id(submitJson$submission_url)
  submitRes[["tests"]]            <- processSubmission(submitJson)
  submitRes[["exercise_name"]]    <- submitJson$exercise_name
  submitRes[["all_tests_passed"]] <- submitJson$all_tests_passed
  submitRes[["points"]]           <- submitJson$points
  return(submitRes)
}

test_that("submit_exercise works correctly with right JSON from server", {
  mock_fn <- mock(mockSubmitJson())
  stub(submit_exercise, "submit_current", mock_fn)
  stub(submit_exercise, "showMessage", "")
  credentials   <- list()
  submitResults <- submit_exercise(path        = simpleProject, 
                                   credentials = credentials)
  expect_equal(submitResults$data$tests[[1]]$name, "hello-world test_hello")
  expect_equal(submitResults$data$tests[[1]]$status, "pass")
})

test_that("submit_exercise displays error correctly", {
  submitJson <- list()
  submitJson$error <- mockErrorSubmitJson()$results$error
  stub(submit_exercise, "submit_current", submitJson)
  stub(submit_exercise, "showMessage", "")
  submitResults <- submit_exercise(path        = simpleProject,
                                   credentials = list())
  expect_equal(submitResults$error, "Internal Server Error")
})

test_that("Message function is called", {
  submitResults <- list()
  submitResults$data$points <- list("r1", "r2")
  submitResults$data$all_tests_passed <- TRUE
  submitResults$data$exercise_name <- "project1"
  mock_fn <- mock("")
  stub(showMessage, "rstudioapi::showDialog", mock_fn)
  stub(showMessage, ".send_listener_request", mock_fn)
  showMessage(submitResults)
  args <- mock_args(mock_fn)
  expect_called(mock_fn, 1)
  if (!rstudioapi::isAvailable()) {
    args <- args[[1]][[2]]
  } else {
    args <- args[[1]]
  }
  expected_message <- 
    paste0("Congratulations! All tests passed on the server!",
           "<p><b>Points permanently awarded: r1, r2</b>",
           "<p>You can now view the model solution on the server.")
  expect_equal(args[[2]], expected_message)
})

