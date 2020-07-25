.submitTabUI <- function(id, label = "Submit tab") {
  #init selected exercise:
  .ddprint(".submitTabUI when is this run??")
  assign("selectedExercisePath", exercisePathFromWd(), envir = .GlobalEnv)

  ns <- shiny::NS(id)
  miniTabPanel(
    title = "Test & Submit",
    icon = icon("check"),

    miniContentPanel(
      fluidPage(style = "padding:0px;margin:0px;",
        fluidRow(
          column(6, class = "col-xs-6",
                 selectInput(inputId = ns("selectExercise"),
                             "Exercise:",
                             c(),
                             selected = selectedExercisePath)),
          column(6, class = "col-xs-6",
                 actionButton(inputId = ns("refreshExercises"),
                              label = "Refresh exercises",
                              style = "margin-top:25px;"))
        ),
        fluidRow(
          column(12, class = "col-xs-12",
                 actionButton(inputId = ns("openFiles"),
                              label = "Open files",
                              style = "margin-top:5px;"),
                 actionButton(inputId = ns("saveFiles"),
                              label = "Save modifications",
                              style = "margin-top:5px;"))
        ),
        fluidRow(
          column(12, style = "margin-top:5px;",
                 actionButton(inputId = ns("source"),
                              label = "Source"),
                 actionButton(inputId = ns("runTests"),
                              label = "Run tests"),
                 actionButton(inputId = ns("submit"),
                              label = "Submit to server"),
                 checkboxInput(inputId = ns("showAllResults"),
                               label = "Show all results",
                               value = TRUE),
                 checkboxInput(inputId = ns("toggleEcho"),
                               label = "Echo source",
                               value = TRUE))
        ),
        column(12,
          uiOutput(outputId = ns("testResultsDisplay"))))
    )
  )
}

.submitTab <- function(input, output, session, globalReactiveValues) {
  reactive <- reactiveValues(submitResults = NULL,
                             testResults = NULL,
                             runStatus = NULL,
                             showAll = TRUE,
                             sourcing = FALSE,
                             sourceEcho = TRUE)

  # This function is run when the Run tests -button is pressed
  runTestrunner <- observeEvent(input$runTests, {
    if (.UI_disabled) return()

    tmcrstudioaddin::disable_submit_tab()
    .dprint("runTestrunner()")
    if (selectedExercisePath == "") {
      rstudioapi::showDialog("Cannot run tests",
                             "You have not selected the exercises. Please
                             choose the exercises you wish to test first.")
      runResults <- list(run_results = list(), run_status = "run_failed")
    } else {
      runResults <- withProgress(message = "Running tests",
                                 value = 1, {
        tryCatch({
          return(tmcRtestrunner::run_tests(project_path = selectedExercisePath,
                                           print = TRUE))
        }, error = function(e) {
          rstudioapi::showDialog("Cannot run tests",
                                 "tmcRtestrunner errored while running tests")
          return(list(run_results = list(),
                      run_status = "run_failed"))
        })

      })
    }
    reactive$runResults <- runResults
    reactive$testResults <- runResults$test_results
    reactive$runStatus <- runResults$run_status
    reactive$submitResults <- NULL
    reactive$sourcing <- FALSE
    tmcrstudioaddin::enable_submit_tab()
  })

  submitExercise <- observeEvent(input$submit, { 
    if (.UI_disabled) return()

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

    tmcrstudioaddin::disable_submit_tab()
    submitRes <- NULL
    .dprint("submitExercise()")
    if (selectedExercisePath == "") {
      rstudioapi::showDialog("Cannot submit solutions to server",
                             "You have not selected the exercises. Please
                             choose the assignments you wish to submit first.")
      submitRes <- list(run_results = list(), run_status = "run_failed")
    } else {
    withProgress(message = "Submitting exercise",
                 value = 0, {
      submitRes <- submitExercise(selectedExercisePath) })
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
    tmcrstudioaddin::enable_submit_tab()
  })

  showResults <- observeEvent(input$showAllResults, {
    if (.UI_disabled) return()

    reactive$showAll <- input$showAllResults
  })

  sourceEcho <- observeEvent(input$toggleEcho, {
    if (.UI_disabled) return()

    reactive$sourceEcho <- input$toggleEcho
  })

  selectedExercises <- observeEvent(input$selectExercise, {
    if (.UI_disabled) return()
    assign("selectedExercisePath", input$selectExercise, envir = .GlobalEnv)
  })

  sourceExercise <- observeEvent(input$source, {
    if (.UI_disabled) return()

    tmcrstudioaddin::disable_submit_tab()

    .dprint("sourceExercise()")
    if (selectedExercisePath == "") {
      rstudioapi::showDialog("Cannot source exercises",
                             "You have not selected the exercises. Please
                             choose the exercises you wish to source first.")
    } else {
      tryCatch({
        sourceExercise(selectedExercisePath, reactive$sourceEcho)
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
    tmcrstudioaddin::enable_submit_tab()
  })

  # Refresh exercises
  observeEvent(input$refreshExercises, {
    if (.UI_disabled) return()
    globalReactiveValues$downloadedExercises <- downloadedExercisesPaths()
  })

  observeEvent(input$openFiles, {
    if (.UI_disabled) return()
    tmcrstudioaddin::disable_submit_tab()

    if (selectedExercisePath == "") {
      rstudioapi::showDialog("Cannot open files",
                             "You have not selected the exercises. Please
                             choose the exercises you wish to open first.")
    } else {
      for (file in list.files(full.names = TRUE,
                              path = file.path(selectedExercisePath, "R"),
                              pattern = "[.]R$")) {
	.ddprint(file)
        rstudioapi::navigateToFile(file)
      }
    }
    tmcrstudioaddin::enable_submit_tab()
  })

  observeEvent(input$saveFiles, {
    if (.UI_disabled) return()
    tmcrstudioaddin::disable_submit_tab()
    .ddprint("Save modifications")
    rstudioapi::documentSaveAll()
    tmcrstudioaddin::enable_submit_tab()
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

  #Exercises are updated everytime this module is called
  updateExercises <-
    observeEvent(globalReactiveValues$downloadedExercises, {
                   downloadedExercises <-
                     globalReactiveValues$downloadedExercises
                   updateSelectInput(session = session,
                                     inputId = "selectExercise",
                                     label = "Exercise:",
                                     choices = downloadedExercises,
                                     selected = selectedExercisePath) })
}
