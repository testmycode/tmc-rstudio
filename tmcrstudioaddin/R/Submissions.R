#' @title Submit current exercise and process the response
#'
#' @description Submit the currently open exercise to the TMC server
#' and process the JSON received from the response.
#'
#' @usage submitExercise()
#'
#' @details Submits the currently open exercise to the TMC server,
#' queries the server until it has finished processing the submission,
#' reads the data from the JSON received in the HTTP response and
#' shows a message popup showing if all of the tests passed or not.
#'
#' @return List of data read from the submission result JSON. List keys:
#' \code{tests}, \code{exercise_name}, \code{all_tests_passed}, \code{points}.
#' \code{NULL} if submitting the exercise to the server failed
#'
#' @seealso \code{\link{submitCurrent}}, \code{\link{processSubmissionJson}},
#' \code{\link{showMessage}}

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

#' @title Submit the currently open exercise to the TMC server
#'
#' @description Submit the currently open exercise to the TMC server and return the
#' Submission result JSON.
#'
#' @usage submitCurrent()
#'
#' @details Reads the OAuth2 token and TMC server address from
#' \code{.crendentials.rds} and uploads the currently open exercise to
#' the TMC server. If the upload was successful, starts querying the TMC server for the
#' submission result JSON until the server has finished processing the tests.
#'
#' @return Submission result JSON if processing the tests in the TMC server was
#' successful. \code{NULL} if processing the tests ended in error.
#'
#' @seealso \code{\link{getCredentials}}, \code{\link{upload_current_exercise}},
#' \code{\link{getExercisePath}}, \code{\link{getExerciseFromServer}}
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

#' @title Get the exercise submission results from the TMC server
#'
#' @description Get the exercise submission results from the TMC server
#'
#' @usage getExerciseFromServer(response, token)
#'
#' @param response HTTP response to the exercise submission upload.
#' @param token OAuth2 token associated with the current login session.
#'
#' @details Queries the server with HTTP GET requests until the server
#' has finished processing the exercise submission.
#'
#' @return Submission result JSON if processing the tests in the TMC server was
#' successful. \code{NULL} if processing the tests ended in error.
#'
#' @seealso \code{\link{get_json_from_submission_url}},
#' \code{\link[shiny]{withProgress}}
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

#' @title Read data from the submission result JSON
#'
#' @description Read data from the submission result JSON.
#'
#' @usage processSubmissionJson(output)
#'
#' @param output HTTP response containg the submission result JSON.
#'
#' @details Reads the test results, exercise name, boolean depending on if all
#' tests passed or not and the received points form the submission result JSON.
#'
#' @return List of data read from the submission result JSON. List keys:
#' \code{tests}, \code{exercise_name}, \code{all_tests_passed}, \code{points}
#'
#' @seealso \code{\link{processSubmission}}
processSubmissionJson <- function(output) {
  submitRes <- list()
  submitRes[["tests"]] <- processSubmission(output)
  submitRes[["exercise_name"]] <- output$exercise_name
  submitRes[["all_tests_passed"]] <- output$all_tests_passed
  submitRes[["points"]] <- output$points
  return(submitRes)
}

#' @title Read test result data from the submission result JSON
#'
#' @description Read test result data from the submission result JSON.
#'
#' @usage processSubmission(output)
#'
#' @param output HTTP response containing the submission result JSON.
#'
#' @details Creates a list of test results received from the submission result JSON.
#'
#' @return List of test results received from the submission result JSON.
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

#' @title Show submission results in a pop-up dialog box
#'
#' @description Show submission reuslts in a pop-up dialog box.
#'
#' @usage showMessage(submitResults)
#'
#' @param submitResults List of data read from the submission result JSON.
#'
#' @seealso \code{\link{getDialogMessage}}, \code{\link[rstudioapi]{showDialog}}
showMessage <- function(submitResults) {
  message <- getDialogMessage(submitResults)
  rstudioapi::showDialog(title = message[["title"]],
                         message = message[["text"]],
                         url = "")
}

#' @title Get message to display in submission result pop-up dialog.
#'
#' @description Creates a message to be shown on the submit result pop-up dialog from
#' the submission results.
#'
#' @usage getDialogMessage(submitResults)
#'
#' @param submitResults List of data read from the submission result JSON.
#'
#' @return Message showing if submitting the exercise failed, some tests failed or all
#' tests passed.
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
