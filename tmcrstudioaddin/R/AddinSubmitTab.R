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
    if (UI_disabled) return()

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
    if (UI_disabled) return()

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
      reactive$submitResults <- submitRes$data
      reactive$testResults <- submitRes$data$tests
      reactive$runStatus <- "success"
      reactive$sourcing <- FALSE
    }
    tmcrstudioaddin::enable_submit_tab()
  })

  showResults <- observeEvent(input$showAllResults, {
    if (UI_disabled) return()

    reactive$showAll <- input$showAllResults
  })

  sourceEcho <- observeEvent(input$toggleEcho, {
    if (UI_disabled) return()

    reactive$sourceEcho <- input$toggleEcho
  })

  selectedExercises <- observeEvent(input$selectExercise, {
    if (UI_disabled) return()
    assign("selectedExercisePath", input$selectExercise, envir = .GlobalEnv)
  })

  sourceExercise <- observeEvent(input$source, {
    if (UI_disabled) return()

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
    if (UI_disabled) return()
    globalReactiveValues$downloadedExercises <- downloadedExercisesPaths()
  })

  observeEvent(input$openFiles, {
    if (UI_disabled) return()
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
    if (UI_disabled) return()
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
