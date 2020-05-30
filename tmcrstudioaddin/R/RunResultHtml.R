#' @title Create HTML for displaying test results
#'
#' @description Create \code{HTML} for displaying test results.
#'
#' @usage createTestResultsHtml(testResults, showAll)
#'
#' @param testResults List of test results.
#' @param showAll Boolean for showing all test results.
#'
#' @details The created \code{HTML} includes a percentage bar for
#' showing how many of the tests passed, and a list of paragraph
#' elements for each of the test results. If the user has ticked off the
#' \code{Show all results} checkbox, then instead of a list only a
#' single paragraph element is shown. The single parahraph is either of
#' the first failing test, or a message showing "All tests passed" if
#' none of the tests failed. If the current exercise doesn't have any
#' tests associated with it, then only a single paragraph reading "No
#' tests for exercise" is returned.
#'
#' @return A single \code{Shiny} \code{HTML} tag object containing all
#' of the elements in the created \code{HTML} test result output.
#'
#' @seealso \code{\link[shiny]{tags}}

# Creates html for testResults
createTestResultsHtml <- function(testResults, showAll) {
  if (length(testResults) == 0) {
    return(tags$html(tags$p("No tests for exercise.",
                            style = "color: red;font-weight:bold")))
  }

  # Contains html elements for test results
  testResultHtmlElements <- .createTestResultElements(testResults, showAll)

  testsPassedPercentage <- .testsPassedPercentage(testResults)

  # Html page with progress bar. testResultsHtmlElements and
  # testPassedProcentage gets inserted to the html.
  html_str <-
    paste(sep = "",
                ".progressBar { position: relative; width: 100%; ",
                "background-color: red; border-radius: 0px; }\n",
                ".progress { width:",
                testsPassedPercentage,
                "; height: 30px; background-color: green; ",
                "border-radius: 0px; }\n",
                ".progressText { position: absolute; text-align: ",
                "center; width: 100%; top: 6px;}")
  .ddprint("HTML STRING")
  .ddprint(html_str)
  html <- tags$html(tags$head(tags$style(HTML(html_str))),
                    tags$body(
      tags$div(class = "progressBar",
               tags$div(class = "progressText", testsPassedPercentage),
               tags$div(class = "progress")),
      testResultHtmlElements))
  return(html)
}


#' @title Create HTML for displaying a run or sourcing fail
#'
#' @description Create \code{HTML} for displaying a run or sourcing fail.
#'
#' @usage createRunSourcingFailHtml(runResults)
#'
#' @param runResults Results from the failed attempt of running tests.
#'
#' @details Creates an \code{HTML} view for displaying information related to a run- or
#' sourcing fail
#'
#' @return A single \code{Shiny} \code{HTML} tag object containing all of the elements in
#' the created \code{HTML} run result output.
#'
#' @seealso \code{\link[shiny]{tags}}
#'
# Creates html for runResult with run or sourcing fail
createRunSourcingFailHtml <- function(runResults) {
  if (runResults$run_status == "sourcing_failed") {
    fail_name = "Sourcing fail"
  } else {
    fail_name = "Run fail"
  }
  html <- tags$html(tags$p(fail_name,
                           style = "color: red;font-weight:bold"),
                    .backtraceHtmlTags(runResults$backtrace))
  return(html)
}

# Reactively displays results depending on whether the
# show all results -checkbox is checked or not
.createTestResultElements <- function(testResults, showAll) {
  if (showAll) {
    return(lapply(1:length(testResults), function(i) {
      testResult <- testResults[[i]]
      .createTestResultElement(name = testResult$name, status = testResult$status,
                               index = i, message = testResult$message,
                               backtrace = testResult$backtrace)
    }))
  } else {
    return(.createSingleResultElement(testResults = testResults))
  }
}

# Creates an individual HTML paragraph element for the list displaying test results
.createTestResultElement <- function(name, status, index = NULL, message = NULL, backtrace) {
  # Assign a color depending on test status
  color <- ifelse(test = grepl(x = status, pattern = "pass"), yes = "green", no = "red")
  elements <- tags$p(paste(name, ":", status),
                     style = paste("color:", color, ";font-weight:bold"))

  #If status != pass, add de
  if (status != "pass"){
    elements <- list(elements, .createDetailedTestResultElement(index, message, backtrace))
  }

  return(elements)
}

.createDetailedTestResultElement <- function(index = NULL, message = NULL, backtrace) {
  btn <- tags$button(id = paste("button_", index, sep = ""), "Toggle details")
  id <- paste0("message_", index)
  style <- "display:none"
  message <- tags$p(paste("message:", message), style = "font-weight: bold;")
  backtraceTags <- .backtraceHtmlTags(backtrace)
  script <- tags$script(paste0("$(\"#button_", index,
                              "\").click(function(){$(\"#message_",
                              index, "\").toggle()});"))
  return(list(tags$div(message, backtraceTags, style = style, id = id), btn, script))
}



# Creates an HTML paragraph element for either the first failing test or a separate message
# if all tests passed
.createSingleResultElement <- function(testResults) {
  for (i in 1:length(testResults)) {
    result <- testResults[[i]]

    if (identical(x = result$status, y = "fail")) {
      return(.createTestResultElement(name = result$name, status = result$status,
                                      index = i, message = result$message, backtrace = result$backtrace))
    }
  }

  return(.createTestResultElement(name = "All tests", status = "pass"))
}

# Returns backtrace as html p tags.
.backtraceHtmlTags <- function(backtrace) {
  return(lapply(backtrace, tags$p))
}
