.submitTabUI <- function(id, label = "Submit tab") {
  ns <- shiny::NS(id)

  miniTabPanel(
    title = "Test & Submit",
    icon = icon("check"),

    miniContentPanel(
      actionButton(inputId = ns("runTests"), label = "Run tests"),
      actionButton(inputId = ns("submit"), label = "Submit to server"),
      checkboxInput(inputId = ns("showAllResults"), label = "Show all results", value = TRUE),
      uiOutput(outputId = ns("testResultsDisplay"))
    )
  )
}

.submitTab <- function(input, output, session) {
  reactive <- reactiveValues(submitResults = NULL, testResults = NULL, runStatus = NULL, showAll = TRUE)

  # This function is run when the Run tests -button is pressed
  runTestrunner <- observeEvent(input$runTests, {
    withProgress(message= 'Running tests', value = 1, {
      runResults <- tmcRtestrunner::run_tests(print = TRUE)
    })
    reactive$testResults <- runResults$test_results
    reactive$runStatus <- runResults$run_status
    reactive$submitResults <- NULL
  })

  submitExercise <- observeEvent(input$submit, {
    submitRes <- submitExercise()
    if(!is.null(submitRes)) {
      reactive$submitResults <- submitRes
      reactive$testResults <- submitRes$tests
      reactive$runStatus <- "success"
    }
  })

  showResults <- observeEvent(input$showAllResults, {
    reactive$showAll = input$showAllResults
  })

  # Renders a list showing the test results
  output$testResultsDisplay <- renderUI({
    if (is.null(reactive$testResults)) return()
    testResults = reactive$testResults
    showAll <- reactive$showAll
    html <- ""
    if (reactive$runStatus == "success") {
      html <- formatTestResults(testResults, showAll)
    } else {
      html <- .createRunSourcingFailHtml(runResults)
    }
    shiny::tagList(html)
  })
}

formatTestResults <- function(testResults, showAll) {
  testResultOutput <- getTestOutput(testResults, showAll)
  html <- formatResultsWithBar(testResultOutput, .testsPassedPercentage(testResults))
  return(html)
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

getDialogMessage <- function(submitResults) {
  message <- list()
  message[["title"]] <- "Results"
  if (is.null(submitResults)) {
    message[["title"]] <- "Error"
    message[["text"]] <- "Could not submit exercise.<p>Current project is not a tmc project."
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
  if (runResults$run_status == "sourcing_fail") {
    fail_name = "Sourcing fail"
  } else {
    fail_name = "Run fail"
  }
  html <- tags$html(tags$p(fail_name,
                           style = "color: red;font-weight:bold"),
                    tags$p("TODO: traceback (runner doesnt return traceback yet)"))
  return(html)
}
