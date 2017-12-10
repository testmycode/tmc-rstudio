submitExercise <- function(path) {
  submitJson <- list()
  submitJson <- submitCurrent(path)
  submitRes <- list()
  if(is.null(submitJson$error)) {
    submitRes$data <- processSubmissionJson(submitJson$results)
  } else {
    submitRes$error <- submitJson$error
  }
  showMessage(submitRes)
  return(submitRes)
}

submitCurrent <- function(path) {
  submitJson <- list()
  credentials <- tmcrstudioaddin::getCredentials()
  token <- credentials$token
  response <- upload_current_exercise(token, project_path = path)
  if(is.null(response$error)) {
    submitJson <- getExerciseFromServer(response$data, token, 10)
  } else {
    submitJson$error <- response$error
  }
  return(submitJson)
}

getExerciseFromServer <- function(response, token, sleepTime) {
  submitJson <- get_json_from_submission_url(response, token)
  if (is.null(submitJson$error)) {
    while (submitJson$results$status == "processing") {
      if (!is.null(shiny::getDefaultReactiveDomain())) {
        shiny::incProgress(1/3)
      }
      Sys.sleep(sleepTime)
      submitJson <- get_json_from_submission_url(response, token)
    }
    if (submitJson$results$status == "error") {
      submitJson$error <- submitJson$results$error
    }
  }
  return(submitJson)
}

processSubmissionJson <- function(submitJson) {
  submitRes <- list()
  submitRes[["tests"]] <- processSubmission(submitJson)
  submitRes[["exercise_name"]] <- submitJson$exercise_name
  submitRes[["all_tests_passed"]] <- submitJson$all_tests_passed
  submitRes[["points"]] <- submitJson$points
  return(submitRes)
}

processSubmission <- function(submitJson) {
  tests <- list()
  for (testCase in submitJson$test_cases) {
    result <- list()
    result[["name"]] <- testCase$name
    result[["status"]] <- getStatusFromBoolean(testCase$successful)
    result[["message"]] <- testCase$message
    tests[[length(tests) + 1]]  <- result
  }
  return(tests)
}

getStatusFromBoolean <- function(bol) {
  status <- "fail"
  if (bol) {
    status <- "pass"
  }
  return(status)
}

showMessage <- function(submitResults) {
  message <- getDialogMessage(submitResults)
  rstudioapi::showDialog(title = message$title,
                         message = message$text,
                         url = "")
}

getDialogMessage <- function(submitResults) {
  message <- list()
  message$title <- "Results"
  if (!is.null(submitResults$error)) {
    message$title <- "Error"
    errormsg <- submitResults$error
    if (nchar(errormsg) > 300) {
      errormsg <- substr(errormsg, 1, 300)
    }
    message$text <- paste0("<p>", errormsg)
  } else if (submitResults$data$all_tests_passed) {
    points <- paste(submitResults$data$points, collapse = ", ")
    message$text <- paste0("All tests passed on the server.<p><b>Points permanently awarded: ",
                                points, "</b><p>View model solution")
  } else {
    points <- paste(submitResults$data$points, collapse = ", ")
    message$text <- paste0("Exercise ", submitResults$data$exercise_name,
                                " failed partially.<p><b>Points permanently awarded: ", points,
                                "</b><p>Some tests failed on the server.<p>Press OK to see failing tests")
  }
  return(message)
}
