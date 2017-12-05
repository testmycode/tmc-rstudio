
.submitTabUI <- function(id, label = "Submit tab") {
  #init selected exercise:
  selectedExercise <<- exerciseFromWd()

  ns <- shiny::NS(id)
  miniTabPanel(
    title = "Test & Submit",
    icon = icon("check"),

    miniContentPanel(
      selectInput(inputId = ns("selectExercise"), "Exercise:", downloadedExercises(),
                  selected = selectedExercise),
      actionButton(inputId = ns("source"), label = "Source"),
      actionButton(inputId = ns("runTests"), label = "Run tests"),
      actionButton(inputId = ns("submit"), label = "Submit to server"),
      checkboxInput(inputId = ns("showAllResults"), label = "Show all results", value = TRUE),
      uiOutput(outputId = ns("testResultsDisplay"))
    )
  )
}

.submitTab <- function(input, output, session) {
  reactive <- reactiveValues(submitResults = NULL, testResults = NULL, runStatus = NULL, showAll = TRUE,
                             sourcing = FALSE)

  # This function is run when the Run tests -button is pressed
  runTestrunner <- observeEvent(input$runTests, {
    runResults <- withProgress(message= 'Running tests', value = 1, {
      tryCatch({
        #error("lel")
        return(tmcRtestrunner::run_tests(project_path = getExercisePath(selectedExercise),
                                                print = TRUE))
      }, error = function(e) {
        rstudioapi::showDialog("Cannot run tests", "tmcRtestrunner errored while running tests")
        return(list(run_results = list(), run_status = "run_failed"))
      })

    })
    reactive$runResults <- runResults
    reactive$testResults <- runResults$test_results
    reactive$runStatus <- runResults$run_status
    reactive$submitResults <- NULL
    reactive$sourcing <- FALSE
  })

  submitExercise <- observeEvent(input$submit, {
    submitRes <- NULL
    withProgress(message = 'Submitting exercise', value = 0, {
      submitRes <- submitExercise()
    })
    if(!is.null(submitRes)) {
      reactive$submitResults <- submitRes
      reactive$testResults <- submitRes$tests
      reactive$runStatus <- "success"
      reactive$sourcing <- FALSE
    }
  })

  showResults <- observeEvent(input$showAllResults, {
    reactive$showAll = input$showAllResults
  })

  selectedExercises <- observeEvent(input$selectExercise, {
    selectedExercise <<- input$selectExercise
  })

  sourceExercise <- observeEvent(input$source, {
    tryCatch({
      sourceExercise(selectedExercise)
      reactive$sourcing <- TRUE
    }, error = function(e) {
      rstudioapi::showDialog("Sourcing failed", "Error while sourcing exercise.")
    })
  })

  # Renders a list showing the test results
  output$testResultsDisplay <- renderUI({
    if (is.null(reactive$testResults) & !reactive$sourcing) {
      return()
    }

    if (reactive$sourcing) {
      html <- tags$p("Sourced exercise to console.")
    } else {
      testResults = reactive$testResults
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
}
