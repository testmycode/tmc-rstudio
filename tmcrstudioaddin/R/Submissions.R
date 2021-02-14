#' @title Submit current exercise and process the response
#'
#' @description Submit the currently chosen exercise to the TMC server
#' and process the \code{JSON} received from the response.
#'
#' @usage submit_exercise(path, credentials)
#'
#' @param path Path to the currently chosen directory.
#' @param credentials List of user credentials.
#'
#' @details Submits the currently open exercise to the TMC server,
#' queries the server until it has finished processing the submission,
#' reads the data from the \code{JSON} received in the \code{HTTP}
#' response and shows a message popup showing if all of the tests passed
#' or not.
#'
#' @return List of data read from the submission result \code{JSON}.
#' List keys: \code{tests}, \code{exercise_name},
#' \code{all_tests_passed}, \code{points}, \code{error}.  \code{error}
#' is not \code{NULL}if submitting the exercise to the server failed
#'
#' @seealso \code{\link{submit_current}},
#' \code{\link{processSubmissionJson}}, \code{\link{showMessage}}

submit_exercise <- function(path, credentials) {
  # print("submit_exercise()")
  submitJson <- list()
  submitJson <- submit_current(path, credentials)
  submitRes <- list()
  if (is.null(submitJson$error)) {
    # print(str(submitJson))
    submitRes$data <- processSubmissionJson(submitJson$results)
  } else {
    if (is.character(submitJson$error)) {
      submitRes$data <- processSubmissionJson(submitJson$results)
    }
    submitRes$error <- submitJson$error
  }
  showMessage(submitRes)
  return(submitRes)
}

#' @title Submit the currently chosen exercise to the TMC server
#'
#' @description Submit the currently chosen exercise to the TMC server
#' and return the submission result \code{JSON}.
#'
#' @usage submit_current(path, credentials)
#'
#' @param path Path to the currently chosen directory.
#' @param credentials List of user credentials.
#'
#' @details Reads the \code{OAuth2} token and TMC server address from
#' \code{crendentials} and uploads the currently open exercise to
#' the TMC server. If the upload was successful, starts querying the TMC
#' server for the submission result \code{JSON} until the server has
#' finished processing the tests.
#'
#' @return Submission result with non \code{NULL} \code{results} if
#' processing the tests in the TMC server was successful. List keys:
#' \code{results}, \code{error}. Error is not \code{NULL} if processing
#' the tests ended in error.
#'
#' @seealso \code{\link{getCredentials}},
#' \code{\link{upload_current_exercise}},
#' \code{\link{getExerciseFromServer}}
submit_current <- function(path, credentials) {
  submitJson <- list()
  .dprint("submit_current()")
  token <- credentials$token
  response <- upload_current_exercise(credentials, project_path = path)
  if (is.null(response$error)) {
    .ddprint("upload_response..")
    .ddprint(str(response))
    submitJson <- getExerciseFromServer(response$data, token, 10)
  } else {
    .dprint("HERE NOW. NO CONNECTION TO SERVER")
    cat("No access to server.\n")
    submitJson$error <- response$error
    submitJson$error$server_access <- FALSE
  }
  return(submitJson)
}

#' @title Get the exercise submission results from the TMC server
#'
#' @description Get the exercise submission results from the TMC server
#'
#' @usage getExerciseFromServer(response, token, sleepTime)
#'
#' @param response \code{HTTP} response to the exercise submission upload.
#' @param token \code{OAuth2} token associated with the current login session.
#' @param sleepTime The time to sleep between queries to the tmc-server.
#'
#' @details Queries the server with \code{HTTP-GET} requests until the server
#' has finished processing the exercise submission.
#'
#' @return Submission result with non \code{NULL} \code{results} if
#' processing the tests in the TMC server was successful. List keys:
#' \code{results}, \code{error}. Error is not \code{NULL} if processing
#' the tests ended in error.
#'
#' @seealso \code{\link{get_json_from_submission_url}},
#' \code{\link[shiny]{withProgress}}
getExerciseFromServer <- function(response, token, sleepTime) {
  if (!is.null(shiny::getDefaultReactiveDomain())) {
    cat("Submission succesfully sent to server for processing...\n")
    shiny::setProgress(message = "Submission sent to server",
                       value = 1/2)
  }
  .dprint("HERE NOW. THE SUBMISSION IS ON THE SERVER")
  submitJson <- get_json_from_submission_url(response, token)
  if (is.null(submitJson$error)) {
    timeout_happened <- FALSE
    first_time       <- TRUE
    while (submitJson$results$status == "processing" |
           (submitJson$results$status == "timeout" & !timeout_happened)) {
      if (submitJson$results$status == "timeout") {
        cat("Connection problems...\nWaiting for 10 seconds and trying again...\n")
        shiny::setProgress(message = "Lost connection. Waiting 10 secs")
        timeout_happened <- TRUE
      } else if (timeout_happened) {
        cat("Reconnected to server.\nServer is still processing submission.\n")
        shiny::setProgress(message = "Reconnected, waiting for results")
        timeout_happened <- FALSE
      } else if (!first_time) {
        cat("Server is still processing...\n")
        shiny::setProgress(message = "Still waiting for results")
      }
      first_time <- FALSE
      for (i in seq_len(sleepTime)) {
        if (!is.null(shiny::getDefaultReactiveDomain())) {
          shiny::incProgress(1 / 120)
        }
        if ((i == round(sleepTime / 2)) &
            !timeout_happened &
            !is.null(shiny::getDefaultReactiveDomain())) {
          cat("Waiting for results from the server...\n")
          shiny::setProgress(message = "Waiting for server results")
        }
        Sys.sleep(1)
      }
      submitJson <- get_json_from_submission_url(response, token)
    }
    if (!is.null(shiny::getDefaultReactiveDomain())) {
      cat("Server results from the submission are ready.\n")
      shiny::setProgress(message = "Results from submission ready",
                         value = 1)
    }
    if (submitJson$results$status == "error") {
      submitJson$error <- submitJson$results$error
      if (!is.character(submitJson$error))
        submitJson$error$server_access <- TRUE
      .dprint("HERE WE GOT AN ERROR")
    }
  }
  return(submitJson)
}

#' @title Read data from the submission result JSON
#'
#' @description Read data from the submission result \code{JSON}.
#'
#' @usage processSubmissionJson(submitJson)
#'
#' @param submitJson \code{HTTP} response containg the submission result
#' \code{JSON}.
#'
#' @details Reads the test results, exercise name, boolean depending on
#' if all tests passed or not and the received points form the
#' submission result \code{JSON}.
#'
#' @return List of data read from the submission result \code{JSON}.
#' List keys: \code{tests}, \code{exercise_name},
#' \code{all_tests_passed}, \code{points}
#'
#' @seealso \code{\link{processSubmission}}
processSubmissionJson <- function(submitJson) {
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

#' @title Read test result data from the submission result JSON
#'
#' @description Read test result data from the submission result \code{JSON}.
#'
#' @usage processSubmission(submitJson)
#'
#' @param submitJson HTTP response containing the submission result \code{JSON}.
#'
#' @details Creates a list of test results received from the submission
#' result \code{JSON}.
#'
#' @return List of test results received from the submission result \code{JSON}.
processSubmission <- function(submitJson) {
  getStatusFromBoolean <- function(bol) {
    status <- "fail"
    if (bol) {
      status <- "pass"
    }
    return(status)
  }
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


#' @title Show submission results in a pop-up dialog box
#'
#' @description Show submission reuslts in a pop-up dialog box.
#'
#' @usage showMessage(submitResults)
#'
#' @param submitResults List of data read from the submission result
#' \code{JSON}.
#'
#' @seealso \code{\link{getDialogMessage}},
#' \code{\link[rstudioapi]{showDialog}}
showMessage <- function(submitResults) {
  message <- getDialogMessage(submitResults)
  if (message$show) {
    if (!rstudioapi::isAvailable()) {
      cat("Showing message not yet implemented\n")
      cat("Title:\n")
      cat(message$title, "\n")
      cat("Message:\n")
      cat(message$text, "\n")
    } else {
      rstudioapi::showDialog(title   = message$title,
                             message = message$text,
                             url     = "")
    }
  }
}

.print_compilation_error <- function(pre_error) {
  if (grepl("^Compilation", pre_error)) {
    pre_lines     <- strsplit(pre_error, split = "\n")[[1]]
    error_msg_vec <- pre_lines[-1]
    error_msg_vec[1] <- sub("compiler_output: ", "", error_msg_vec[1])
    error_msg     <- paste(error_msg_vec, collapse = "\n")
    error_msg
  } else {
    paste(pre_error, "\n", sep = "")
  }
}

#' @title Get message to display in submission result pop-up dialog.
#'
#' @description Creates a message to be shown on the submit result
#' pop-up dialog from the submission results.
#'
#' @usage getDialogMessage(submitResults)
#'
#' @param submitResults List of data read from the submission result
#' \code{JSON}.
#'
#' @return Message showing if submitting the exercise failed, some tests
#' failed or all tests passed.


getDialogMessage <- function(submitResults) {
  message <- list()
  .ddprint("NOW parsing submit Results")
  .ddprint(str(submitResults))
  message$title <- "Results"
  if (!is.null(submitResults$error)) {
    message$show <- FALSE
    message$text <- ""
    return(message)

    # move the texts below to proper place
    message$title <- "Error"
    .dprint("getDialogMessageError")
    pre_error <-
      if (is.character(submitResults$error)) {
        console_error <- .print_compilation_error(submitResults$error)
        cat("Server couldn't run tests with your code,",
            "since your code produced the following error:", sep = "\n")
        cat(console_error, "\n")
	if (console_error == "unable to start data viewer") {
	  next_line <- paste("Server does not have View(...) functionality, so please",
			     "comment out or remove all the View(...) commands.",
			     sep = " ")
	} else if (grepl("invalid multibyte character", console_error)) {
	  next_line <- paste("You might have used",
			     "nordic letters in text with encoding that is not UTF-8.",
			     "Try using UTF-8 encoding or use only ASCII characters.",
			     sep = " ")
	} else {
	  next_line <- paste("You can find the error message on the console.",
			     "This should help you identifying and locating the error.",
			     sep = " ")
	}
	server_error <- paste("Server could not run tests.",
			      next_line,
## 			      "#gsub("\n", "<p>", console_error),
			      sep = "<p>")
## 	print(gsub("\n", "<br>", console_error))
        .ddprint(server_error)
	submitResults$error <- server_error
        message$title <- "Submission succeeded with code problem"
        submitResults$error
      } else {
        message$title <- "Submission failed: error"
        cat("Submission failed with error: ")
        cat(submitResults$error$message)
        cat("\n")
        #print(str(submitResults$error))
        #print(submitResults$error)
        submitResults$error$message
      }
    messages_tmp <-
      matrix(byrow = TRUE, ncol = 3,
	     c(c("Unauthorized (HTTP 401).",
		 paste("Your submission was refused by server (HTTP 401).",
		       "This most likely means that the submission deadline",
		       "has closed."),
		 paste("Your submission was refused by server (HTTP 401).",
		       "This most likely means that the submission deadline",
		       "has closed.")),
	       c("Forbidden (HTTP 403).",
		 paste("Your submission failed as forbidden request (HTTP 403).",
		       "<p>The most common cause of this are firewalls, VPN's,",
		       "antivirus programs that block the connection as well as",
		       "stale credentials. It can also happen if the server is",
		       "down. <p> Try logging out and back in from addin in a",
		       "different network and check if tmc.mooc.fi is working.",
		       "<p> If the problem persists, please contact the course",
		       "instructors."),
		 paste("Your submission failed as forbidden request (HTTP 403).",
		       "<p>The most common cause of this are firewalls, VPN's,",
		       "antivirus programs that block the connection as well as",
		       "stale credentials. It can also happen if the server is",
		       "down. <p> Try logging out and back in from addin in a",
		       "different network and check if tmc.mooc.fi is working.",
		       "<p> If the problem persists, please contact the course",
		       "instructors.")),
	       c("file.exists(path) is not TRUE",
		 paste("Submission uploading failed with 'file.exists(path)",
		       "is not TRUE'. <p> The reason for this is most likely",
		       "with your installation of Rtools. Please take a look at",
		       "Rtools installationmanual. <p> If you are unable to fix this",
		       "contact the course instructors in this case."),
		 paste("Submission uploading failed with 'file.exists(path)",
		       "is not TRUE'.  <p> This is most likely an issue with",
		       "file permissions. <p> Please contact the course instructors",
		       "in this case.")),
	       c("Bad Gateway (HTTP 502).",
		 paste("Your submission failed with 'Bad Gateway (HTTP 502)'.",
		       "You can try restarting RStudio and RTMC and then resubmitting.<p>",
		       "This can also mean that server is is temporarily not accepting",
		       "requests. You should try resubmitting again later, but if you",
		       "are in a hurry, contact the course teacher"),
		 paste("Your submission failed with 'Bad Gateway (HTTP 502)'.",
		       "You can try restarting RStudio and RTMC and then resubmitting.<p>",
		       "This can also mean that server is is temporarily not accepting",
		       "requests. You should try resubmitting again later, but if you",
		       "are in a hurry, contact the course teacher")),
               c("LibreSSL SSL_read: SSL_ERROR_SYSCALL, errno 60",
		 paste("Your submission failed with 'LibreSSL ... errno 60'",
		       "This usually means that your connection failed just before",
                       "submission. You should try resubmitting again for more informative",
                       "error message."),
		 paste("Your submission failed with 'LibreSSL ... errno 60'",
		       "This usually means that your connection failed just before",
                       "submission. You should try resubmitting again for more informative",
                       "error message.")),
	       c("Could not resolve host: tmc.mooc.fi",
		 paste("Host tmc.mooc.fi could not be reached. Do you have a working",
		       "network connection? If you do, then tmc.mooc.fi might be",
		       "currently unreachable. You should try resubmitting again later,",
		       "but if you are in a hurry, contact the course teacher"),
		 paste("Host tmc.mooc.fi could not be reached. Do you have a working",
		       "network connection? If you do, then tmc.mooc.fi might be",
		       "currently unreachable. You should try resubmitting again later,",
		       "but if you are in a hurry, contact the course teacher")),
               c(pre_error,
		 paste(pre_error, "<p>Please contact the course instructors in this case."),
		 paste(pre_error, "<p>Please contact the course instructors in this case."))))
    errormsgs <- list(keys      = messages_tmp[ , 1],
		      msgs_win  = messages_tmp[ , 2],
		      msgs_unix = messages_tmp[ , 3])
    if (!is.null(.Platform$OS.type) && .Platform$OS.type == "windows") {
      errormsg <- errormsgs$msgs_win[errormsgs$keys == pre_error][1]
    } else {
      errormsg <- errormsgs$msgs_unix[errormsgs$keys == pre_error][1]
    }
    if (is.null(.Platform$OS.type)) {
      print("This is mysterious machine")
    }
    if (is.character(submitResults$error)) {
      errormsg <- paste0(pre_error, "<p> There is some issue with your \
code. Please try to fix your code or ask help from course instructors.")
    }
    if (nchar(errormsg) > 300) {
      errormsg <- substr(errormsg, 1, 300)
    }
    message$text <- paste0("<p>", errormsg)
  } else if (submitResults$data$all_tests_passed) {
    message$show <- TRUE
    points <- paste(submitResults$data$points,
                    collapse = ", ")
    message$text <-
      paste0("Congratulations! All tests passed on the server!",
             "<p><b>Points permanently awarded: ",
             points,
             "</b><p>You can now view the model solution on the server.")
  } else {
    message$show <- TRUE
    message_data <-
      list("Just keep trying! ",
           "That's a fine start! ",
           "You are getting there! ",
           "Nice! ",
           "Very nice! ",
           "Almost there! ",
           "Almost there! ",
           "Almost there! ")
    points <- paste(submitResults$data$points, collapse = ", ")
    num_of_points <- length(submitResults$data$points)
    message_1 <- message_data[[num_of_points + 1]]
    message_2 <-
      if (num_of_points == 1) {
        " point "
      } else {
        " points "
      }
    message_3 <- paste0("You received ", num_of_points, message_2, "from ")
    .ddprint(num_of_points)
    .ddprint(message_1)
    .ddprint(message_2)
    message$text <-
      paste0(message_1,
             message_3,
             "Exercise set '",
             submitResults$data$exercise_name,
             "'. <p>",
             "<b>Points permanently awarded: ", points, "</b>",
             "<p>Some tests still failed on the server.",
             "<p>Press OK to see failing tests")
  }
  return(message)
}
