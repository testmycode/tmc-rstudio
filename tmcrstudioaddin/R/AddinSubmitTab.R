.submitTabUI <- function(id, label = "Submit tab") {
  ns <- shiny::NS(id)

  miniTabPanel(
    title = "Test & Submit",
    icon = icon("check"),

    miniContentPanel(
      actionButton(inputId = ns("runTests"), label = "Run tests"),
      actionButton(inputId = ns("submit"), label = "Submit to server"),
      checkboxInput(inputId = ns("showAllResults"), label = "Show all results", value = FALSE),
      uiOutput(outputId = ns("testResultsDisplay"))
    )
  )
}

.submitTab <- function(input, output, session) {
  # This function is run when the Run tests -button is pressed
  runTestrunner <- eventReactive(input$runTests, {
    tmcRtestrunner::run_tests(print = TRUE)
  })

  # Renders a list showing the test results
  output$testResultsDisplay <- renderUI({
    # Tests are ran only when the run tests -button is pressed
    runResults <- runTestrunner()
    testResults <- runResults$test_results
    testsPassedPercentage <- .testsPassedPercentage(testResults)

    # Reactively displays results depending on whether the
    # show all results -checkbox is checked or not
    if (runResults$run_status == "success") {
      if (input$showAllResults) {
        testResultOutput <- lapply(1:length(testResults), function(i) {
          testResult <- testResults[[i]]
          .createTestResultElement(
            name = testResult$name,
            status = testResult$status,
            index = i,
            message = testResult$message
          )
        })
      } else {
        testResultOutput <-
          createSingleResultDisplay(testResults = testResults)
      }

      html <- tags$html(tags$head(tags$style(HTML(
        paste(
          sep = "",
          ".progressBar { position: relative; width: 100%; background-color: red; border-radius: 0px; }
          .progress { width:",
          testsPassedPercentage,
          "; height: 30px; background-color: green; border-radius: 0px; }
          .progressText { position: absolute; text-align: center; width: 100%; top: 6px;}"
        )
        ))),
        tags$body(
          tags$div(
            class = "progressBar",
            tags$div(class = "progressText", testsPassedPercentage),
            tags$div(class = "progress")
          ),
          testResultOutput
        ))


      shiny::tagList(html)
    } else {
      shiny::tagList(.createRunSourcingFailHtml(runResults))
    }
  })
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
