
.submitTabUI <- function(id, label = "Submit tab") {
  #init selected exercise:
  selectedExercisePath <<- exercisePathFromWd()

  ns <- shiny::NS(id)
  miniTabPanel(
    title = "Test & Submit",
    icon = icon("check"),

    miniContentPanel(
      fluidPage(style="padding:0px;margin:0px;",
        fluidRow(
          column(6, class="col-xs-6", selectInput(inputId = ns("selectExercise"), "Exercise:", c(), selected = selectedExercisePath)),
          column(6, class="col-xs-6", actionButton(inputId = ns("refreshExercises"), label = "Refresh exercises", style = "margin-top:25px;"))
        ),
        fluidRow(
          column(12, class="col-xs-12",actionButton(inputId = ns("openFiles"), label = "Open files", style = "margin-top:5px;"))
        ),
        fluidRow(
          column(12,style ="margin-top:5px;",
                 actionButton(inputId = ns("source"), label = "Source"),
                 actionButton(inputId = ns("runTests"), label = "Run tests"),
                 actionButton(inputId = ns("submit"), label = "Submit to server"),
                 checkboxInput(inputId = ns("showAllResults"), label = "Show all results", value = TRUE))
        ),
        column(12,
          uiOutput(outputId = ns("testResultsDisplay"))))
    )
  )
}

.submitTab <- function(input, output, session, globalReactiveValues) {
  reactive <- reactiveValues(submitResults = NULL, testResults = NULL, runStatus = NULL, showAll = TRUE,
                             sourcing = FALSE)

  # This function is run when the Run tests -button is pressed
  runTestrunner <- observeEvent(input$runTests, {
    if(UI_disabled) return()

    tmcrstudioaddin::disable_submit_tab()
    runResults <- withProgress(message= 'Running tests', value = 1, {
      tryCatch({
        #error("lel")
        return(tmcRtestrunner::run_tests(project_path = selectedExercisePath,
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
    tmcrstudioaddin::enable_submit_tab()
  })

  submitExercise <- observeEvent(input$submit, {
    if(UI_disabled) return()

    tmcrstudioaddin::disable_submit_tab()
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
    tmcrstudioaddin::enable_submit_tab()
  })

  showResults <- observeEvent(input$showAllResults, {
    if(UI_disabled) return()

    reactive$showAll = input$showAllResults
  })

  selectedExercises <- observeEvent(input$selectExercise, {
    if(UI_disabled) return()
    selectedExercisePath <<- input$selectExercise
  })

  sourceExercise <- observeEvent(input$source, {
    if(UI_disabled) return()

    tmcrstudioaddin::disable_submit_tab()

    tryCatch({
      sourceExercise(selectedExercisePath)
      reactive$sourcing <- TRUE
    }, error = function(e) {
      rstudioapi::showDialog("Sourcing failed", "Error while sourcing exercise.")
    })

    tmcrstudioaddin::enable_submit_tab()
  })

  # Refresh exercises
  observeEvent(input$refreshExercises, {
    globalReactiveValues$downloadedExercises <- downloadedExercisesPaths()
  })

  observeEvent(input$openFiles, {
    for (file in list.files(full.names = TRUE, path = file.path(selectedExercisePath, "R"), pattern = "[.]R$")) {
      rstudioapi::navigateToFile(file)
    }
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

  #Exercises are updated everytime this module is called
  updateExercises <- observeEvent(globalReactiveValues$downloadedExercises, {
    downloadedExercises <- globalReactiveValues$downloadedExercises
    updateSelectInput(session = session, inputId = "selectExercise", label = "Exercise:",
                    choices = downloadedExercises, selected = selectedExercisePath)
  })
}
