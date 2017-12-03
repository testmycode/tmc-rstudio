
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
    if(UI_disabled) return()

    tmcrstudioaddin::disable_submit_tab()
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
    tmcrstudioaddin::enable_submit_tab()
  })

  submitExercise <- observeEvent(input$submit, {
    if(UI_disabled) return()

    tmcrstudioaddin::disable_submit_tab()
    output <- list()
    withProgress(message= 'Submitting exercise', value = 0, {
      output <- submitCurrent()
    })
    submitRes <- processSubmissionJson(output)
    reactive$submitResults <- submitRes
    reactive$testResults <- submitRes$tests
    reactive$runStatus <- "success"
    showMessage(submitRes)
    reactive$sourcing <- FALSE
    tmcrstudioaddin::enable_submit_tab()
  })

  showResults <- observeEvent(input$showAllResults, {
    if(UI_disabled) return()

    reactive$showAll = input$showAllResults
  })

  selectedExercises <- observeEvent(input$selectExercise, {
    if(UI_disabled) return()

    selectedExercise <<- input$selectExercise
  })

  sourceExercise <- observeEvent(input$source, {
    if(UI_disabled) return()

    tmcrstudioaddin::disable_submit_tab()

    tryCatch({
      sourceExercise(selectedExercise)
      reactive$sourcing <- TRUE
    }, error = function(e) {
      rstudioapi::showDialog("Sourcing failed", "Error while sourcing exercise.")
    })

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
      testResults = reactive$testResults
      runResults <- reactive$runResults
      showAll <- reactive$showAll
      html <- ""
      if (reactive$runStatus == "success") {
        html <- formatTestResults(testResults, showAll)
      } else {
        html <- .createRunSourcingFailHtml(runResults)
      }
    }
    shiny::tagList(html)
  })
}

formatTestResults <- function(testResults, showAll) {
  if (length(testResults) == 0) {
    return (tags$p("No tests for exercise.",
            style = "color: red;font-weight:bold"))
  }
  testResultOutput <- getTestOutput(testResults, showAll)
  html <- formatResultsWithBar(testResultOutput, .testsPassedPercentage(testResults))
  return(html)
}

submitCurrent <- function() {
  credentials <- tmcrstudioaddin::getCredentials()
  token <- credentials$token
  url <- httr::content(upload_current_exercise(token, getExercisePath(selectedExercise)))
  output <- httr::content(get_submission_json(token, url$submission_url))
  while (output$status == "processing") {
    incProgress(1/3)
    Sys.sleep(10)
    output <- get_submission_json(token, url$submission_url)
  }
  return(output)
}

# Reactively displays results depending on whether the
# show all results -checkbox is checked or not
getTestOutput <- function(testResults, showAll) {
  if (showAll) {
    testResultOutput <- lapply(1:length(testResults), function(i) {
      testResult <- testResults[[i]]
      .createTestResultElement(name = testResult$name, status = testResult$status,
                               index = i, message = testResult$message)
    })
  } else {
    testResultOutput <- createSingleResultDisplay(testResults = testResults)
  }
}

formatResultsWithBar <- function(testResultOutput, testsPassedPercentage) {
  html <- tags$html(tags$head(
    tags$style(HTML(paste(sep = "",
                          ".progressBar { position: relative; width: 100%; background-color: red; border-radius: 0px; }
                            .progress { width:", testsPassedPercentage, "; height: 30px; background-color: green; border-radius: 0px; }
                            .progressText { position: absolute; text-align: center; width: 100%; top: 6px;}")))),
    tags$body(
      tags$div(class = "progressBar",
               tags$div(class = "progressText", testsPassedPercentage),
               tags$div(class = "progress")),
      testResultOutput))
  return(html)
}

processSubmissionJson <- function(output) {
  submitRes <- list()
  submitRes[["tests"]] <- processSubmission(output)
  submitRes[["exercise_name"]] <- output$exercise_name
  submitRes[["all_tests_passed"]] <- output$all_tests_passed
  submitRes[["points"]] <- output$points
  return(submitRes)
}

showMessage <- function(submitResults) {
  message <- getDialogMessage(submitResults)
  rstudioapi::showDialog(title = "Results",
                         message = message,
                         url = "")
}

getDialogMessage <- function(submitResults) {
  message <- ""
  if (submitResults$all_tests_passed) {
    message <- paste0("<p>All tests passed on the server.<p><b>Points permanently awarded: ",
                      submitResults$points, "</b><p>View model solution")
  } else {
    message <- paste0("<p>Exercise ", submitResults$exercise_name,
                      " failed partially.<p><b>Points permanently awarded: ", submitResults$points,
                      "</b><p>Some tests failed on the server.<p>Press OK to see failing tests")
  }
  return(message)
}

# Creates an individual HTML paragraph element for the list displaying test results
.createTestResultElement <- function(name, status, index = NULL, message = NULL) {
  # Assign a color depending on test status
  color <- ifelse(test = grepl(x = status, pattern = "pass"), yes = "green", no = "red")
  elements <- tags$p(paste(name, ":", status),
                    style = paste("color:", color, ";font-weight:bold"))

  #if status is not pass add details
  if (status != "pass"){
    elements <- list(elements, .createDetailedMessageWithButton(index, message))
  }

  return(elements)
}

.createDetailedMessageWithButton <- function(index = NULL, message = NULL){
  btn <- tags$button(id = paste("button_", index, sep = ""), "Toggle details")
  message <- tags$p(style = "display:none", paste("message:", message),
                   id = paste("message_", index, sep = ""))
  script <- tags$script(paste("$(\"#button_", index,
                             "\").click(function(){$(\"#message_",
                             index, "\").toggle()});", sep = ""))
  return(list(message, btn, script))
}

# Creates an HTML paragraph element for either the first failing test or a separate message
# if all tests passed
createSingleResultDisplay <- function(testResults) {
  for (i in 1:length(testResults)) {
    result <- testResults[[i]]

    if (identical(x = result$status, y = "fail")) {
      return(.createTestResultElement(name = result$name, status = result$status,
                                         index = i, message = result$message))
    }
  }

  return(.createTestResultElement(name = "All tests", status = "pass"))
}

# Creates html for runResult with run or sourcing fail
.createRunSourcingFailHtml <- function(runResults) {
  if (runResults$run_status == "sourcing_failed") {
    fail_name = "Sourcing fail"
  } else {
    fail_name = "Run fail"
  }
  html <- tags$html(tags$p(fail_name,
                           style = "color: red;font-weight:bold"),
                    tags$p("TODO: traceback (runner doesnt return traceback yet)"))
  return(html)
}
