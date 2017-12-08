submitExercise <- function() {
  output <- list()
  output <- submitCurrent()
  submitRes <- NULL
  if(!is.null(output)) {
    submitRes <- processSubmissionJson(output)
  }
  showMessage(submitRes)
  return(submitRes)
}

submitCurrent <- function() {
  credentials <- tmcrstudioaddin::getCredentials()
  token <- credentials$token
  response <- upload_current_exercise(token, project_path = selectedExercisePath)
  if(!is.null(response)) {
    output <- getExerciseFromServer(response, token)
    return(output)
  } else {
    return(NULL)
  }
}

getExerciseFromServer <- function(response, token) {
  output <- get_json_from_submission_url(response, token)
  if (!is.null(output)) {
    while (output$status == "processing") {
      incProgress(1/3)
      Sys.sleep(10)
      output <- get_json_from_submission_url(response, token)
    }
    if (output$status == "error") {
      print(output$error)
      output <- NULL
    }
  }
  return(output)
}

processSubmissionJson <- function(output) {
  submitRes <- list()
  submitRes[["tests"]] <- processSubmission(output)
  submitRes[["exercise_name"]] <- output$exercise_name
  submitRes[["all_tests_passed"]] <- output$all_tests_passed
  submitRes[["points"]] <- output$points
  return(submitRes)
}

processSubmission <- function(output) {
  tests <- list()
  for (test_case in output$test_cases) {
    result <- list()
    result[["name"]] <- test_case$name
    result[["status"]] <- .getStatusFromBoolean(test_case$successful)
    result[["message"]] <- test_case$message
    tests[[length(tests) + 1]]  <- result
  }
  return(tests)
}

.getStatusFromBoolean <- function(bol) {
  status <- "fail"
  if (bol) {
    status <- "pass"
  }
  return(status)
}

showMessage <- function(submitResults) {
  message <- getDialogMessage(submitResults)
  rstudioapi::showDialog(title = message[["title"]],
                         message = message[["text"]],
                         url = "")
}

getDialogMessage <- function(submitResults) {
  message <- list()
  message[["title"]] <- "Results"
  if (is.null(submitResults)) {
    message[["title"]] <- "Error"
    message[["text"]] <- "Could not submit exercise."
  } else if (submitResults$all_tests_passed) {
    message[["text"]] <- paste0("All tests passed on the server.<p><b>Points permanently awarded: ",
                                submitResults$points, "</b><p>View model solution")
  } else {
    message[["text"]] <- paste0("Exercise ", submitResults$exercise_name,
                                " failed partially.<p><b>Points permanently awarded: ", submitResults$points,
                                "</b><p>Some tests failed on the server.<p>Press OK to see failing tests")
  }
  return(message)
}
