.submitTabUI <- function(id, label = "Submit tab") {
  #init selected exercise (this is now done in TMC_plugin.R
  .dprint("submitTabUI launched")
  ns <- shiny::NS(id)
  .ddprint(ns("selectExercise"))
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
                                              label    = "Submit to server"),
                                 checkboxInput(inputId = ns("showAllResults"),
                                               label   = "Show all results",
                                               value   = TRUE),
                                 checkboxInput(inputId = ns("toggleEcho"),
                                               label   = "Echo source",
                                               value   = TRUE)))
  fluid_page  <- fluidPage(style = "padding:0px;margin:0px;",
                           top_row,
                           second_row,
                           bottom_row,
                           column(12, uiOutput(outputId = ns("testResultsDisplay"))))
  tab_panel   <- miniTabPanel(title = "Test & Submit",
                              icon  = icon("check"),
                              miniContentPanel(fluid_page))
  list(ns_inputIDs    = ns_inputIDs,
       mini_tab_panel = tab_panel)
}

.submitTab <- function(input, output, session, globalReactiveValues) {
  grv <- globalReactiveValues
  enable_submit__and_course_tab <- function() {
    .ddprint("Enabling new way")
    tmcrstudioaddin::enable_UI_elements(grv$UI_elements)
    .ddprint("Ready to do this SubmitTab")
    # Ok. This is just an ad hoc way to do it and is caused by mixing
    # responsibilities. Actually we should just enable and disable ALL the
    # buttons.
    shinyjs::delay(ms = 100,
                   expr = {
                     .ddprint("Launching new way...")
                     globalReactiveValues$UI_disabled <- FALSE
                   })
  }
  disable_submit__and_course_tab <- function() {
    .ddprint("Disabling new way SubmitTab")
    tmcrstudioaddin::disable_UI_elements(grv$UI_elements)
    globalReactiveValues$UI_disabled <- TRUE
  }

  reactive <- reactiveValues(submitResults = NULL,
                             testResults = NULL,
                             runStatus = NULL,
                             showAll = TRUE,
                             sourcing = FALSE,
                             sourceEcho = TRUE)
  silent_run_tests <- function() {
    guard_test_run <- function() {
      tryCatch({
        .ddprint("Run when tests are launched.")
        return(tmcRtestrunner::run_tests(project_path = globalReactiveValues$selectedExercisePath,
                                         print = TRUE))
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
    run_results <- withProgress(message = "Running tests",
                                value   = 1,
                                guard_test_run())
    rm(list = test_globals_missing, envir = .GlobalEnv)
    for (name in test_globals_names) {
      assign(name, value = test_globals_store[[name]], envir = .GlobalEnv)
    }
    run_results
  }

  # This function is run when the Run tests -button is pressed
  runTestrunner <- observeEvent(input$runTests, {
    if (grv$UI_disabled) {
      .ddprint("Disabled... ")
      return()
    }

    disable_submit__and_course_tab()
    .dprint("runTestrunner()")
    .ddprint("Run when tests are launched.")
    if (globalReactiveValues$selectedExercisePath == "") {
      rstudioapi::showDialog("Cannot run tests",
                             "You have not selected the exercises. Please
                             choose the exercises you wish to test first.")
      run_results <- list(run_results = list(), run_status = "run_failed")
    } else {
      run_results <- silent_run_tests()
    }
    reactive$runResults <- run_results
    reactive$testResults <- run_results$test_results
    reactive$runStatus <- run_results$run_status
    reactive$submitResults <- NULL
    reactive$sourcing <- FALSE
    enable_submit__and_course_tab()
    # check https://docs.rstudio.com/ide/server-pro/latest/rstudio-ide-commands.html
    rstudioapi::executeCommand("refreshEnvironment")
  })

  submitExercise <- observeEvent(input$submit, { 
    if (grv$UI_disabled) {
      .ddprint("Disabled... ")
      return()
    }
    if (is.null(globalReactiveValues$credentials$token)) {
      disable_submit__and_course_tab()
      rstudioapi::showDialog("Not logged in",
                             "Please log in to submit your solutions to server.",
                             "")
      enable_submit__and_course_tab()
      return()
    }

    tranlation_df <- 
      as.data.frame(
        stringsAsFactors = FALSE,
        matrix(byrow = TRUE, ncol = 2,
               c("1 is",         "1 is solved correctly",
                 "1a and",       "1a and 1b are solved correctly",
                 "1a is",        "1a is solved correctly",
                 "1a, 1b",       "1a, 1b and 1c are solved correctly",
                 "1b and",       "1b and 1c are solved correctly",
                 "1b is",        "1b is solved correctly",
                 "1c and",       "1c and 1d is solved correctly",
                 "1c is",        "1c is solved correctly",
                 "1d and",       "1d and 1e are solved correctly",
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
                 "3a is",        "3a is solved correctly",
                 "3b and",       "3b and 3c are solved correctly",
                 "3b is",        "3b is solved correctly",
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
    names(tranlation_df) <- c("key", "translation")

    disable_submit__and_course_tab()
    submitRes <- NULL
    .dprint("submitExercise()")
    if (globalReactiveValues$selectedExercisePath == "") {
      rstudioapi::showDialog("Cannot submit solutions to server",
                             "You have not selected the exercises. Please
                             choose the assignments you wish to submit first.")
      submitRes <- list(run_results = list(), run_status = "run_failed")
    } else {
      .ddprint("Run when tests are submitted.")
      withProgress(message = "Submitting exercise",
                   value = 0, {
                     submitRes <- submitExercise(globalReactiveValues$selectedExercisePath)
                   })
    }
    if (is.null(submitRes$error)) {
      if (!is.null(reactive$testResults)) {
        test_names_local  <- c(sapply(X = reactive$testResults, function(x) x$name),
                               "")
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
          if (substr(lname, 1, nchar(sname)) == sname) {
            ind1 <- ind1 + 1
            test_names_server[ind2] <- lname
          } else {
            temp_bool <- tranlation_df$key == sname
            if (sum(temp_bool)) {
              .ddprint(which(temp_bool))
              .ddprint(tranlation_df$translation[which(temp_bool)])
              test_names_server[ind2] <- tranlation_df$translation[which(temp_bool)]
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
        .ddprint(str(tranlation_df))
        submitRes$data$tests <- resolved_tests
      }
      reactive$submitResults <- submitRes$data
      reactive$testResults <- submitRes$data$tests
      reactive$runStatus <- "success"
      reactive$sourcing <- FALSE
    }
    enable_submit__and_course_tab()
  })

  showResults <- observeEvent(input$showAllResults, {
    if (grv$UI_disabled) {
      .ddprint("Disabled... ")
      return()
    }

    reactive$showAll <- input$showAllResults
  })

  sourceEcho <- observeEvent(input$toggleEcho, {
    if (grv$UI_disabled) {
      .ddprint("Disabled... ")
      return()
    }

    reactive$sourceEcho <- input$toggleEcho
  })

  selectedExercises <- observeEvent(input$selectExercise, {
    if (grv$UI_disabled) {
      .ddprint("Disabled... ")
      return()
    }

    .ddprint("This is always lauched when a new exercise is selected.")
    .ddprint(str(input$selectExercise))
    globalReactiveValues$selectedExercisePath <- input$selectExercise
  })

  sourceExercise <- observeEvent(input$source, {
    if (grv$UI_disabled) {
      .ddprint("Disabled... ")
      return()
    }

    disable_submit__and_course_tab()

    .dprint("sourceExercise()")
    .ddprint("Launched when Source is clicked")
    if (globalReactiveValues$selectedExercisePath == "") {
      rstudioapi::showDialog("Cannot source exercises",
                             "You have not selected the exercises. Please
                             choose the exercises you wish to source first.")
    } else {
      tryCatch({
        sourceExercise(globalReactiveValues$selectedExercisePath, reactive$sourceEcho)
        reactive$sourcing <- TRUE},
        error = function(e) {
          cat("Error in ")
          cat(deparse(e$call))
          cat(" : ")
          cat(e$message)
          cat("\n")
          rstudioapi::showDialog("Sourcing failed",
                                 "Error while sourcing exercise.")
        })
    }
    enable_submit__and_course_tab()
  })

  # Refresh exercises
  observeEvent(input$refreshExercises, {
    if (grv$UI_disabled) {
      .ddprint("Disabled... ")
      return()
    }

    globalReactiveValues$downloadedExercises <- downloadedExercisesPaths()
  })

  observeEvent(input$openFiles, {
    if (grv$UI_disabled) {
      .ddprint("Disabled... ")
      return()
    }

    disable_submit__and_course_tab()

    .ddprint("Launched when clicking open files")
    if (globalReactiveValues$selectedExercisePath == "") {
      rstudioapi::showDialog("Cannot open files",
                             "You have not selected the exercises. Please
                             choose the exercises you wish to open first.")
    } else {
      for (file in list.files(full.names = TRUE,
                              path = file.path(globalReactiveValues$selectedExercisePath, "R"),
                              pattern = "[.]R$")) {
	.ddprint(file)
        rstudioapi::navigateToFile(file)
      }
    }
    enable_submit__and_course_tab()
  })

  observeEvent(input$saveFiles, {
    if (grv$UI_disabled) {
      .ddprint("Disabled... ")
      return()
    }

    disable_submit__and_course_tab()
    .ddprint("Save modifications")
    rstudioapi::documentSaveAll()
    enable_submit__and_course_tab()
  })

  # Renders a list showing the test results
  output$testResultsDisplay <- renderUI({
    if (is.null(reactive$testResults) & !reactive$sourcing) {
      return()
    }

    if (reactive$sourcing) {
      html <- tags$p("Sourced exercise to console.")
    } else {
      testResults <- reactive$testResults
      runResults <- reactive$runResults
      showAll <- reactive$showAll
      html <- ""
      if (reactive$runStatus == "success") {
        html <- createTestResultsHtml(testResults, showAll)
      } else {
        html <- createRunSourcingFailHtml(runResults)
      }
    }
    shiny::tagList(html)
  })

  # Exercises are updated everytime this module is called
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
                      sub("syksy-", "Autumn 20", unique_course_names)))))
    names(grouped_exercise_paths) <- course_titles
    .ddprint(grouped_exercise_paths)
    grouped_exercise_paths
  }

  update_exercises <-function() {
    .dprint("UPDATE EXERCISES LAUNCHED!")
    .dprint("Only launched when updates via observeEvent")
    grouped_downloaded_exercises <- group_exercises(globalReactiveValues$downloadedExercises)
    updateSelectInput(session  = session,
                      inputId  = "selectExercise",
                      label    = "Exercise:",
                      choices  = grouped_downloaded_exercises,
                      selected = globalReactiveValues$selectedExercisePath)
  }
  observeEvent(globalReactiveValues$downloadedExercises, update_exercises())
}
