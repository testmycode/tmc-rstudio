.submitTabUI <- function(id, label = "Submit tab") {
  #init selected exercise (this is now done in TMC_plugin.R
  .dprint(".submitTabUI launched")
  ns <- shiny::NS(id)
  inputIDs    <- c("selectExercise",
                   "refreshExercises",
                   "openFiles",
                   "saveFiles",
                   "source",
                   "runTests",
                   "submit",
                   "showAllResults",
                   "toggleEcho")
  ns_inputIDs <- sapply(inputIDs, ns)
  top_row     <- fluidRow(column(width = 6,
                                 class = "col-xs-6",
                                 selectInput(inputId   = ns("selectExercise"),
                                             label     = "Exercise:",
                                             choices   = c(),
                                             selected  = exercisePathFromWd())),
                          column(width = 6,
                                 class = "col-xs-6",
                                 actionButton(inputId  = ns("refreshExercises"),
                                              label    = "Refresh exercises",
                                              style    = "margin-top:25px;")))
  second_row  <- fluidRow(column(width = 12,
                                 class = "col-xs-12",
                                 actionButton(inputId  = ns("openFiles"),
                                              label    = "Open files",
                                              style    = "margin-top:5px;"),
                                 actionButton(inputId  = ns("saveFiles"),
                                              label    = "Save modifications",
                                              style    = "margin-top:5px;")))
  bottom_row  <- fluidRow(column(width = 12,
                                 style = "margin-top:5px;",
                                 actionButton(inputId  = ns("source"),
                                              label    = "Source"),
                                 actionButton(inputId  = ns("runTests"),
                                              label    = "Run tests"),
                                 actionButton(inputId  = ns("submit"),
                                              label    = "Submit to server")))
  bottom2_row  <- fluidRow(column(width = 6,
				  class = "col-xs-6",
				  checkboxInput(inputId = ns("showAllResults"),
						label   = "Show all results",
                                                value   = TRUE)),
			   column(width = 6,
				  class = "col-xs-6",
				  checkboxInput(inputId = ns("toggleEcho"),
						label   = "Echo source",
						value   = TRUE)))
  fluid_page  <- fluidPage(style = "padding:0px;margin:0px;",
                           top_row,
                           second_row,
                           bottom_row,
                           bottom2_row,
                           column(12, uiOutput(outputId = ns("testResultsDisplay"))))
  tab_panel   <- miniTabPanel(title = "Test & Submit",
                              icon  = icon("check"),
                              miniContentPanel(fluid_page))
  list(ns_inputIDs    = ns_inputIDs,
       mini_tab_panel = tab_panel)
}

.submitTab <- function(input, output, session, globalReactiveValues) {
  .dprint(".submitTab launched")
#
# submitTab reactives
#
  grv      <- globalReactiveValues
  reactive <- reactiveValues(submitResults = NULL,
                             testResults   = NULL,
                             runResults    = NULL,
                             runStatus     = NULL,
                             test_names    = NULL,
                             error_state   = FALSE,
                             showAll       = TRUE,
                             sourcing      = FALSE,
                             sourceEcho    = TRUE)
#
# normal functions
#
  enable_tab_UI <- function() {
    .dprint("Enabling new way")
    .ddprint("Ready to do this SubmitTab")
    # Ok. This is just an ad hoc way to do it and is caused by mixing
    # responsibilities. Actually we should just enable and disable ALL the
    # buttons.
    shinyjs::delay(ms = 10,
                   expr = {
                     .ddprint("Launching new way...")
                     not_logged_in      <- is.null(grv$credentials$token)
                     course_tab_UI_list <- grv$UI_elements$course_tab
                     tmcrstudioaddin::enable_UI_elements(grv$UI_elements, grv$UI_state)
                   })
  }
  disable_tab_UI <- function() {
    .ddprint("Disabling new way SubmitTab")
    tmcrstudioaddin::disable_UI_elements(grv$UI_elements)
    globalReactiveValues$UI_disabled <- TRUE
  }

  silent_run_tests2 <- function() {
    guard_test_run <- function() {
      tryCatch({
        .ddprint("Run when tests are launched.")
        test_results <- tmcRtestrunner::run_tests(project_path = grv$selectedExercisePath,
                                                  print        = FALSE,
                                                  addin_data   = list(only_test_names = TRUE))
        shiny::setProgress(value = 1)
        return(test_results)
      }, error = function(e) {
        rstudioapi::showDialog("Cannot run tests",
                               "tmcRtestrunner errored while running tests")
        return(list(run_results = list(),
                    run_status = "run_failed"))
      })
    }
    test_globals_boolean <- c("points", "points_for_all_tests") %in% ls(.GlobalEnv)
    .ddprint(test_globals_boolean)
    test_globals_missing <-
      c("points", "points_for_all_tests")[!test_globals_boolean]
    test_globals_names <-
      c("points", "points_for_all_tests")[test_globals_boolean]
    test_globals_store <- mget(c("points", "points_for_all_tests")[test_globals_boolean],
                               envir = .GlobalEnv)
    .ddprint(test_globals_missing)
    .ddprint(test_globals_names)
    .ddprint(test_globals_store)
    cat("Getting local tests... ")
    run_results <- withProgress(message = "Getting tests",
                                value   = 1/3,
                                { guard_test_run() })
    rm(list = test_globals_missing, envir = .GlobalEnv)
    for (name in test_globals_names) {
      assign(name, value = test_globals_store[[name]], envir = .GlobalEnv)
    }
    cat("done\n")
    run_results
  }

  silent_run_tests <- function() {
    guard_test_run <- function() {
      tryCatch({
        .ddprint("Run when tests are launched.")
        test_results <- tmcRtestrunner::run_tests(project_path = grv$selectedExercisePath,
                                                  print        = TRUE,
                                                  addin_data   = list(only_test_names = FALSE))
        shiny::setProgress(value = 1)
        return(test_results)
      }, error = function(e) {
        rstudioapi::showDialog("Cannot run tests",
                               "tmcRtestrunner errored while running tests")
        return(list(run_results = list(),
                    run_status = "run_failed"))
      })
    }
    test_globals_boolean <- c("points", "points_for_all_tests") %in% ls(.GlobalEnv)
    .ddprint(test_globals_boolean)
    test_globals_missing <-
      c("points", "points_for_all_tests")[!test_globals_boolean]
    test_globals_names <-
      c("points", "points_for_all_tests")[test_globals_boolean]
    test_globals_store <- mget(c("points", "points_for_all_tests")[test_globals_boolean],
                               envir = .GlobalEnv)
    .ddprint(test_globals_missing)
    .ddprint(test_globals_names)
    .ddprint(test_globals_store)
    cat("Running local tests...\n")
    run_results <- withProgress(message = "Running tests",
                                value   = 1/3,
                                { guard_test_run() })
    rm(list = test_globals_missing, envir = .GlobalEnv)
    for (name in test_globals_names) {
      assign(name, value = test_globals_store[[name]], envir = .GlobalEnv)
    }
    run_results
  }
  group_exercises <- function(exercise_paths) {
    .ddprint("group_exercises")
    .ddprint("exercise_paths")
    .ddprint(exercise_paths)
    .ddprint("strsplit")
    .ddprint(strsplit(names(exercise_paths), ":"))
    course_names <- sapply(strsplit(names(exercise_paths), ":"),
                           function (x) {
                             if (!length(x)) return("Unnamed")
                             x[[1]]
                           })
    .ddprint("course_names")
    .ddprint(course_names)
    exercise_names <- sapply(strsplit(names(exercise_paths), ":"),
                             function (x) {
                               if (!length(x)) return("Unnamed")
                               x[[2]]
                             })
    .ddprint("exercise_names")
    .ddprint(exercise_names)
    names(exercise_paths) <- exercise_names
    unique_course_names <-  sort(unique(course_names))
    .ddprint("unique_course_names")
    .ddprint(unique_course_names)
    grouped_exercise_paths <- lapply(unique_course_names,
                                     function(course_name) {
                                       exercise_paths[course_names == course_name]
                                     })
    course_titles <-
      sub("hy-tiltu-ja-r-i-", "Tilastotiede ja R tutuksi I, ",
          sub("hy-tiltu-ja-r-ii-", "Tilastotiede ja R tutuksi II, ",
              sub("kevat-", "Spring 20",
                  sub("kesa-", "Summer 20",
                      sub("syksy-", "Autumn 20",
                          sub("hy-hy-", "hy-",
                              unique_course_names))))))
    names(grouped_exercise_paths) <- course_titles
    .ddprint(grouped_exercise_paths)
    grouped_exercise_paths
  }

 do_the_computation <- function(xx) {
   .dprint("do_the_computation launching ...")
   test_names_local  <- c(sapply(X = xx, function(x) x$name), "")
   # print(test_names_local)
   .dprint("do_the_computation done")
   test_names_local
 }
#
# observer functions
#

  ST_observer1 <- function() {
    .dprint("ST_observer1 launching...")
    # This function is run when the Run tests -button is pressed
    disable_tab_UI()
    .dprint("runTestrunner()")
    .ddprint("Run when tests are launched.")
##     if (globalReactiveValues$selectedExercisePath == "") {
##       rstudioapi::showDialog("Cannot run tests",
##                              "You have not selected the exercises. Please
##                              choose the exercises you wish to test first.")
##       run_results <- list(run_results = list(), run_status = "run_failed")
##     } else {
    run_results <- silent_run_tests()
##     }
    reactive$runResults    <- run_results
    reactive$testResults   <- run_results$test_results
    reactive$test_names    <- do_the_computation(run_results$test_results)
    reactive$runStatus     <- run_results$run_status
    reactive$submitResults <- NULL
    reactive$sourcing      <- FALSE
    reactive$error_state   <- FALSE
    enable_tab_UI()
    # check https://docs.rstudio.com/ide/server-pro/latest/rstudio-ide-commands.html
    rstudioapi::executeCommand("refreshEnvironment")
  }

  .help_text_for_serious_error <- function(message) {
    pre_error <- message
    messages_tmp <-
      c(c("Unauthorized (HTTP 401).",
          paste("Your submission was refused by server (HTTP 401).",
                "This most likely means that the submission deadline",
                "has closed.</p><p>Please check that you have chosen the",
                "right exercise set."),
          paste("Your submission was refused by server (HTTP 401).",
                "This most likely means that the submission deadline",
                "has closed.</p><p>Please check that you have chosen the",
                "right exercise set.")),
        c("Forbidden (HTTP 403).",
          paste("Your submission failed as forbidden request (HTTP 403).",
                "</p><p>This happens after submission deadline has closed.",
                "Please check that you have chosen the",
                "right exercise set.",
		"</p>Other common cause of this are firewalls, VPN's,",
                "antivirus programs that block the connection as well as",
                "stale credentials. It can also happen if the server is",
                "down. </p><p> Try logging out and back in from addin in a",
                "different network and check if tmc.mooc.fi is working.",
                "</p><p> If the problem persists, please contact the course",
                "instructors."),
          paste("Your submission failed as forbidden request (HTTP 403).",
                "</p><p>This happens after submission deadline has closed.",
                "Please check that you have chosen the",
                "right exercise set.",
		"</p>Other common cause of this are firewalls, VPN's,",
                "antivirus programs that block the connection as well as",
                "stale credentials. It can also happen if the server is",
                "down. </p><p> Try logging out and back in from addin in a",
                "different network and check if tmc.mooc.fi is working.",
                "</p><p> If the problem persists, please contact the course",
                "instructors.")),
        c("file.exists(path) is not TRUE",
          paste("Submission uploading failed with 'file.exists(path)",
                "is not TRUE'. </p><p> The reason for this is most likely",
                "with your installation of Rtools. Please take a look at",
                "Rtools installationmanual. </p><p> If you are unable to fix this",
                "contact the course instructors in this case."),
          paste("Submission uploading failed with 'file.exists(path)",
                "is not TRUE'.  </p><p> This is most likely an issue with",
                "file permissions. </p><p> Please contact the course instructors",
                "in this case.")),
        c("Bad Gateway (HTTP 502).",
          paste("Your submission failed with 'Bad Gateway (HTTP 502)'.",
                "You can try restarting RStudio and RTMC and then resubmitting.",
                "</p><p>This can also mean that server is is temporarily not accepting",
                "requests. You should try resubmitting again later, but if you",
                "are in a hurry, contact the course teacher"),
          paste("Your submission failed with 'Bad Gateway (HTTP 502)'.",
                "You can try restarting RStudio and RTMC and then resubmitting.",
                "</p><p>This can also mean that server is is temporarily not accepting",
                "requests. You should try resubmitting again later, but if you",
                "are in a hurry, contact the course teacher")),
        c("LibreSSL SSL_read: SSL_ERROR_SYSCALL, errno 60",
          paste("Your submission failed with 'LibreSSL ... errno 60'",
                "This usually means that your connection failed just before",
                "the submission. You should try resubmitting immediately",
                "again for more informative",
                "error message."),
          paste("Your submission failed with 'LibreSSL ... errno 60'",
                "This usually means that your connection failed just before",
                "the submission. You should try resubmitting immediately",
                "again for more informative",
                "error message.")),
        c("Couldn't connect to server",
          paste("The server could not be reached. This almost surely means",
                "that your network connection is not working currently.",
                "Please check that first.",
                "</p><p>If the network connection is working, then tmc.mooc.fi might be",
                "currently unreachable. You should try resubmitting again later.",
                "If the server is down, please contact the course teacher, if the",
                "submission deadline is close"),
          paste("The server could not be reached. This almost surely means",
                "that your network connection is not working currently.",
                "Please check that first.",
                "</p><p>If the network connection is working, then tmc.mooc.fi might be",
                "currently unreachable. You should try resubmitting again later.",
                "If the server is down, please contact the course teacher, if the",
                "submission deadline is close")),
        c("Could not resolve host: tmc.mooc.fi",
          paste("Host tmc.mooc.fi could not be reached. Do you have a working",
                "network connection? Please check that first.",
                "</p><p>If the network connection is working, then tmc.mooc.fi might be",
                "currently unreachable. You should try resubmitting again later.",
                "</p><p>If the server is down, please contact the course teacher, if the",
                "submission deadline is close"),
          paste("Host tmc.mooc.fi could not be reached. Do you have a working",
                "network connection? Please check that first.",
                "</p><p>If the network connection is working, then tmc.mooc.fi might be",
                "currently unreachable. You should try resubmitting again later.",
                "</p><p>If the server is down, please contact the course teacher, if the",
                "submission deadline is close")),
        c(pre_error,
          paste(pre_error, "</p><p>Please contact the course instructors in this case."),
          paste(pre_error, "</p><p>Please contact the course instructors in this case.")))
    messages_tmp <- matrix(byrow = TRUE, ncol = 3, messages_tmp)
    errormsgs <- list(keys      = messages_tmp[ , 1],
		      msgs_win  = messages_tmp[ , 2],
		      msgs_unix = messages_tmp[ , 3])
    if (!is.null(.Platform$OS.type) && .Platform$OS.type == "windows") {
      errormsg <- errormsgs$msgs_win[errormsgs$keys == pre_error][1]
    } else {
      errormsg <- errormsgs$msgs_unix[errormsgs$keys == pre_error][1]
    }
  }

  .help_text_for_error <- function(message) {
    console_error <- message
    if (console_error == "unable to start data viewer") {
      next_line <- paste("Server does not have View(...) functionality, so please",
                         "comment out or remove all the View(...) commands.",
                         sep = " ")
    } else if (grepl("invalid multibyte character", console_error)) {
      next_line <- paste("You might have used",
                         "nordic letters in text with encoding that is not UTF-8.",
                         "Try using UTF-8 encoding or use only ASCII characters.",
                         sep = " ")
    } else if (grepl("did you use an exit\\(\\) command?", console_error)) {
      next_line <- paste("The execution of your tests took more time",
                         "than the server allows the tests to run, which is roughly",
                         "one minute. Try to locate the part of your code that",
                         "takes long time to run and try to make it more performant",
                         sep = " ")
    } else {
      next_line <- paste("You can find the error message on the console and on addin.",
                         "This should help you identifying and locating the error.",
                         sep = " ")
    }
    next_line
  }

  ST_observer2 <- function() {
    .dprint("ST_observer2 launching...")
    parse_single_test <- function(test_result) {
      .dprint("Parsing...")
      tmp <- test_result$message
      test_result$message <- gsub("\\\\\"",
                                  "\"",
                                  gsub("\\\\n",
                                       "\n",
                                       substr(tmp, 2, nchar(tmp) - 1)))
      .dprint("After...")
      .dprint(str(test_result))
      test_result
    }
   do_the_computation2 <- function(xx) {
    .dprint("do_the_computation2 launching ...")
    test_names_all  <- c(sapply(X = xx, function(x) x$name), "")
    # print(test_names_all)
    .dprint("do_the_computation2 done")
    test_names_all
   }

    parse_test_strings <- function(test_results) {
      lapply(test_results, parse_single_test)
    }

    translation_df <-
      as.data.frame(
        stringsAsFactors = FALSE,
        matrix(byrow = TRUE, ncol = 2,
               c("1 is",         "1 is solved correctly",
                 "1a and",       "1a and 1b are solved correctly",
                 "1a is",        "1a is solved correctly",
                 "1a, 1b",       "1a, 1b and 1c are solved correctly",
                 "1b and",       "1b and 1c are solved correctly",
                 "1b is",        "1b is solved correctly",
                 "1c and",       "1c and 1d are solved correctly",
                 "1c is",        "1c is solved correctly",
                 "1d and",       "1d and 1e are solved correctly",
                 "1d is",        "1d is solved correctly",
                 "2 is",         "2 is solved correctly",
                 "2a and",       "2a and 2b are solved correctly",
                 "2a is",        "2a is solved correctly",
                 "2a, 2b",       "2a, 2b and 2c are solved correctly",
                 "2b and",       "2b and 2c are solved correctly",
                 "2b is",        "2b is solved correctly",
                 "2c and",       "2c and 2d are solved correctly",
                 "2c is",        "2c is solved correctly",
                 "2c, 2d",       "2c, 2d and 2e are solved correctly",
                 "2c, and",      "2c, and 2d are solved correctly",
                 "2d and",       "2d and 2e are solved correctly",
                 "2d is",        "2d is solved correctly",
                 "3 is",         "3 is solved correctly",
                 "3a and",       "3a and 3b are solved correctly",
                 "3a is",        "3a is solved correctly",
                 "3b and",       "3b and 3c are solved correctly",
                 "3b is",        "3b is solved correctly",
                 "3c and",       "3c and 3d are solved correctly",
                 "3c is",        "3c is solved correctly",
                 "4a and",       "4a and 4b are solved correctly",
                 "4a is",        "4a is solved correctly",
                 "4b is",        "4b is solved correctly",
                 "4c is",        "4c is solved correctly",
                 "exercise 3b",  "exercise 3b is solved correctly",
                 "exercise 3c",  "exercise 3c is solved correctly",
                 "exercises 1a", "exercises 1a and 1b are solved correctly",
                 "exercises 1c", "exercises 1c and 1d are solved correctly",
                 "exercises 2a", "exercises 2a and 2b are solved correctly",
                 "exercises 2c", "exercises 2c and 2d are solved correctly",
                 "exercises 3a", "exercises 3a and 3b is solved correctly")))
    names(translation_df) <- c("key", "translation")

    disable_tab_UI()
    submitRes <- NULL
    .dprint("submitExercise()")
    .ddprint("Run when tests are submitted.")
    cat("Creating submission package...\n")
    withProgress(message = "Creating submission package",
                 value = 0, {
                   submitRes <- tmcrstudioaddin::submit_exercise(grv$selectedExercisePath,
                                                                 grv$credentials)
                 })
    # print(str(submitRes))
    if (is.null(submitRes$error)) {
      if (!is.null(reactive$test_names)) {
        .dprint("Adding names")
        test_names_local  <- reactive$test_names
        .dprint(test_names_local)
        test_names_server <- sapply(X = submitRes$data$tests, function(x) x$name)
        ind1 <- 1
        for (ind2 in seq_along(test_names_server)) {
          lname <- test_names_local[ind1]
          sname <- test_names_server[ind2]
          .ddprint(ind1)
          .ddprint(ind2)
          .ddprint(lname)
          .ddprint(sname)
          .ddprint(substr(lname, 1, nchar(sname)) == sname)
          if (!is.na(lname) && substr(lname, 1, nchar(sname)) == sname) {
            ind1 <- ind1 + 1
            test_names_server[ind2] <- lname
          } else {
            temp_bool <- translation_df$key == sname
            if (sum(temp_bool)) {
              .ddprint(which(temp_bool))
              .ddprint(translation_df$translation[which(temp_bool)])
              test_names_server[ind2] <- translation_df$translation[which(temp_bool)]
            }
            # pppp
          }
        }
        .ddprint(test_names_local)
        .ddprint(test_names_server)
        resolved_tests <-
          mapply(function(x, name) {
                   x$name <- name
                   x
                 },
                 submitRes$data$tests,
                 test_names_server,
                 SIMPLIFY = FALSE)
        .ddprint(str(resolved_tests))
        .ddprint(str(translation_df))
        submitRes$data$tests <- resolved_tests
      }
      reactive$submitResults <- submitRes$data
      reactive$testResults   <- parse_test_strings(submitRes$data$tests)
      reactive$test_names    <- do_the_computation2(submitRes$data$tests)
      reactive$runStatus     <- "success"
      reactive$sourcing      <- FALSE
      reactive$error_state   <- FALSE
    } else {
      .dprint("NOW AN ERROR OCCURED")
      # print(str(submitRes))
      reactive$error_state <- TRUE
      if (is.character(submitRes$error)) {
##         print("This came from the server.")
##         print(str(submitRes))
        backtrace_message   <- .print_compilation_error(submitRes$error)
        help_text           <- .help_text_for_error(backtrace_message)
        reactive$runResults <- list(run_status     = "server_failed",
                                    backtrace      = list(paste("Error in source(...) on server :",
                                                                backtrace_message)),
                                    submission_id  = submitRes$data$submitted_at,
                                    submitted_at   = submitRes$data$submitted_at,
                                    help_text      = list(help_text),
                                    test_results   = list())
      } else {
        .dprint("We did not get to the server at all")
        if (submitRes$error$server_access) {
          run_state_message <- "submission_failed_partially"
          disclaimer        <- "Submission state on the server uncertain :"
        } else {
          run_state_message <- "submission_failed"
          disclaimer <- "Submission did not reach the server :"
        }
        help_text           <- .help_text_for_serious_error(submitRes$error$message)
        reactive$runResults <- list(run_status    = run_state_message,
                                    backtrace     = list(paste(disclaimer,
                                                               submitRes$error$message)),
                                    submission_id = NULL,
                                    help_text     = list(help_text),
                                    test_results  = list())
      }

      reactive$submitResults <- list(call = "server", message = submitRes$error)
      # reactive$test_names    <- reactive$test_names # this stays as is
      reactive$testResults   <- list() # this prevents the crash, but needs to be fixed
#      reactive$runStatus <- "success"
      reactive$sourcing      <- FALSE
    }
    enable_tab_UI()
  }
  ST_observer3 <- function() {
    .dprint("ST_observer3 launching...")
    reactive$showAll <- input$showAllResults
  }
  ST_observer4 <- function() {
    .dprint("ST_observer4 launching...")
    reactive$sourceEcho <- input$toggleEcho
  }
  ST_observer5 <- function() {
    .dprint("ST_observer5 launching...")
    # reactive$test_names      <- NULL # this would be optimal place for quick computation
    # testing just running the tests...
    .dprint("Running the tests for names...")
    grv$selectedExercisePath <- input$selectExercise
    # print(str(grv$selectedExercisePath))
    run_results <- silent_run_tests2()
    reactive$test_names    <- do_the_computation(run_results$test_results)
    # reactive$test_names      <- list() # now we always do the conditional computation
    .dprint("Running the tests for names done")
    .dprint("ST_observer5 done")
  }
  ST_observer6 <- function() {
    .dprint("ST_observer6 launching...")
    disable_tab_UI()

    .dprint("sourceExercise()")
    .ddprint("Launched when Source is clicked")
##     if (globalReactiveValues$selectedExercisePath == "") {
##       rstudioapi::showDialog("Cannot source exercises",
##                              "You have not selected the exercises. Please
##                              choose the exercises you wish to source first.")
##     } else 
##     {
    tryCatch({
      sourceExercise(grv$selectedExercisePath, reactive$sourceEcho)
      reactive$sourcing    <- TRUE
      reactive$error_state <- FALSE
    }, error = function(e) {
      cat("Error in ")
      cat(deparse(e$call))
      cat(" : ")
      cat(e$message)
      cat("\n")
      reactive$sourcing      <- TRUE
      reactive$submitResults <- list(call = deparse(e$call), message = e$message)
      reactive$runResults    <- list(run_status     = "local_sourcing_failed",
                                     backtrace      = list(paste("Error in",
                                                                 deparse(e$call),
                                                                 ":",
                                                                 e$message)),
                                     submission_id  = NULL,
                                     test_results   = list())
      reactive$testResults   <- list() # this prevents the crash, but needs to be fixed
      # reactive$test_names  <- reactive$test_names # this stays as is
      reactive$error_state   <- TRUE
    })
##     }
    enable_tab_UI()
    rstudioapi::executeCommand("refreshEnvironment")
  }
  ST_observer7 <- function() {
    .dprint("ST_observer7 launching...")
    globalReactiveValues$downloadedExercises <- tmcrstudioaddin::downloadedExercisesPaths()
  }
  ST_observer8 <- function() {
    .dprint("ST_observer8 launching...")
    disable_tab_UI()

    .ddprint("Launched when clicking open files")
    for (file in list.files(full.names = TRUE,
                            path = file.path(grv$selectedExercisePath, "R"),
                            pattern = "[.]R$")) {
      .ddprint(file)
      rstudioapi::navigateToFile(file)
    }
##     }
    enable_tab_UI()
  }
  ST_observer9 <- function() {
    .dprint("ST_observer9 launching...")
    disable_tab_UI()
    rstudioapi::documentSaveAll()
    enable_tab_UI()
  }
  update_exercises <- function() {
    .dprint("ST_observer10 (update_exercises) launching...")
    .dprint("Only launched when updates via observeEvent")
    grouped_downloaded_exercises <- group_exercises(globalReactiveValues$downloadedExercises)
    updateSelectInput(session  = session,
                      inputId  = "selectExercise",
                      label    = "Exercise:",
                      choices  = grouped_downloaded_exercises,
                      selected = grv$selectedExercisePath)
  }
   ST_observer11 <- function() {
     .dprint("ST_observer11 launched...")
     not_selected  <- input$selectExercise == ""
     grv$UI_state["not_selected"] <- not_selected
     tmcrstudioaddin::enable_UI_elements(grv$UI_elements, grv$UI_state)
     .dprint("ST_observer11 done")
   }
#
# observer initializers
#

  .dprint("ST_observer1...")
  runTestrunner <- observeEvent(input$runTests, ST_observer1())
  .dprint("..initialised")

  .dprint("ST_observer2...")
  submitExercise2 <- observeEvent(input$submit, ST_observer2())
  .dprint("..initialised")

  .dprint("ST_observer3...")
  showResults <- observeEvent(input$showAllResults, ST_observer3())
  .dprint("..initialised")

  .dprint("ST_observer4 ...")
  sourceEcho <- observeEvent(input$toggleEcho, ST_observer4())
  .dprint("..initialised")

  .dprint("ST_observer5 ...")
  selectedExercises <- observeEvent(input$selectExercise, ST_observer5(), ignoreInit = TRUE)
  .dprint("..initialised")

  .dprint("ST_observer6 ...")
  sourceExercise <- observeEvent(input$source, ST_observer6())
  .dprint("..initialised")

  .dprint("ST_observer7 ...")
  observeEvent(input$refreshExercises, ST_observer7())
  .dprint("..initialised")

  .dprint("ST_observer8 ...")
  observeEvent(input$openFiles, ST_observer8())
  .dprint("..initialised")

  .dprint("ST_observer9 ...")
  observeEvent(input$saveFiles, ST_observer9())
  .dprint("..initialised")

  .dprint("ST_observer10 (update_exercises) ...")
  observeEvent(globalReactiveValues$downloadedExercises, update_exercises())
  .dprint("..initialised")

  .dprint("ST_observer11 ...")
  observeEvent(input$selectExercise, { ST_observer11() })
  .dprint("..initialised")

#
# rendering
#
  # Renders a list showing the test results
  output$testResultsDisplay <- renderUI({
    .dprint("WHEN THIS IS LAUNCHED")
    if (is.null(reactive$testResults) & !reactive$sourcing) {
      .dprint("AND THIS?")
      return()
    }

    if (reactive$sourcing & !reactive$error_state) {
      html <- tags$p("Sourced exercise to console.")
    } else if (reactive$sourcing) {
      runResults  <- reactive$runResults
      submission_info  <- list(submission_id = FALSE, submitted_at  = NULL)
      html <- createRunSourcingFailHtml(runResults, grv$selectedExercisePath, submission_info)
    } else if (reactive$error_state) {
      runResults  <- reactive$runResults
      submission_info  <- list(submission_id = runResults$run_status == "server_failed",
                               submitted_at  = reactive$runResults$submitted_at)
      html <- createRunSourcingFailHtml(runResults, grv$selectedExercisePath, submission_info)
    } else {
      testResults    <- reactive$testResults
      runResults     <- reactive$runResults
      showAll        <- reactive$showAll
      if (is.null(reactive$submitResults)) {
        submission_info  <- list(submission_id = TRUE,
                                 submitted_at  = NULL)
      } else {
        submission_info  <- list(submission_id = TRUE,
                                 submitted_at  = reactive$submitResults$submitted_at)
      }
      if (reactive$runStatus == "success") {
        html <- createTestResultsHtml(testResults, showAll, submission_info$submitted_at)
      } else {
        html <- createRunSourcingFailHtml(runResults, grv$selectedExercisePath, submission_info)
      }
    }
    shiny::tagList(html)
  })
}
