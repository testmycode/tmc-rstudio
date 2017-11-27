submitCurrent <- function() {
  credentials <- tmcrstudioaddin::getCredentials()
  token <- credentials$token
  url <- httr::content(upload_current_exercise(token))
  output <- httr::content(get_submission_json(token, url$submission_url))
  while (output$status == "processing") {
    incProgress(1/3)
    Sys.sleep(10)
    output <- get_submission_json(token, url$submission_url)
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
  rstudioapi::showDialog(title = "Results",
                         message = message,
                         url = "")
}
