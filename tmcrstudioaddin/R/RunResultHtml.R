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
                            style = "color: Crimson;font-weight:bold")))
  }

  # Contains html elements for test results
  testResultHtmlElements <- .createTestResultElements(testResults, showAll)

  testsPassedPercentage <- .testsPassedPercentage(testResults)

  # Html page with progress bar. testResultsHtmlElements and
  # testPassedProcentage gets inserted to the html.
  html_str <-
    paste(sep = "",
                ".progressBar { ",
                "position: relative; ",
                "width: 100%; ",
                "opacity: 0.95; ",
                "box-shadow: 0 2px 5px rgba(0, 0, 0, 0.35) inset;",
                "background-color: Crimson; ",
                "border-radius: 2px; }\n",
##                 ".progress { width:",
##                 testsPassedPercentage,
##                 "; height: 30px; background-color: green; ",
##                 "border-radius: 5px; }\n",
                ".testBar { ",
                "width: ", testsPassedPercentage, "; ",
                "height: 30px; ",
##                "background-color: ForestGreen; ",
                "background-color: DarkGreen; ",
                "opacity: 1; ",
                "box-shadow: 0 2px 5px rgba(0, 0, 0, 0.25) inset;",
                "border-right: 1px solid rgba(0, 0, 0, 0.5); ",
                "border-top-left-radius: 2px; ",
                "border-bottom-left-radius: 2px; ",
                "border-top-right-radius: 0px; ",
                "border-bottom-right-radius: 0px; ",
#                "border-radius: 2px; ",
                "}\n",
                ".progressText { ",
                "position: absolute; ",
                "opacity: 1; ",
                "color: white; ",
                "text-shadow: 1px 1px 1px rgba(0, 0, 0, 0.5); ",
                "text-align: center; ",
                "width: 100%; ",
                "top: 6px;}")
  .ddprint("HTML STRING")
  .ddprint(html_str)
  html <- tags$html(tags$head(tags$style(HTML(html_str))),
                    tags$body(
      tags$div(class = "progressBar",
               tags$div(class = "progressText", testsPassedPercentage),
               tags$div(class = "progress testBar")),
      testResultHtmlElements))
  return(html)
}


#' @title Create HTML for displaying a run or sourcing fail
#'
#' @description Create \code{HTML} for displaying a run or sourcing fail.
#'
#' @usage createRunSourcingFailHtml(runResults, exercise_path)
#'
#' @param runResults Results from the failed attempt of running tests.
#' @param exercise_path Current path to the RTMC files.
#'
#' @details Creates an \code{HTML} view for displaying information
#' related to a run- or sourcing fail
#'
#' @return A single \code{Shiny} \code{HTML} tag object containing all
#' of the elements in the created \code{HTML} run result output.
#'
#' @seealso \code{\link[shiny]{tags}}
#'
# Creates html for runResult with run or sourcing fail
createRunSourcingFailHtml <- function(runResults, exercise_path) {
  if (runResults$run_status == "sourcing_failed") {
    fail_name <- "Sourcing failed during testing. The tester could not run tests."
  } else if (runResults$run_status == "local_sourcing_failed") {
    fail_name <- "Sourcing of exercises failed. There is an error in you code."
  } else if (runResults$run_status == "server_failed") {
    fail_name <- "Sourcing failed at the server. The server could not run tests."
  } else if (runResults$run_status == "submission_failed") {
    fail_name <- "Submission did not reach the server due to a submission error."
  } else {
    fail_name <- "Run fail"
  }
  .dprint("the backtrace")
  # print(str(runResults))
  backtrace <- .backtraceHtmlTags2(runResults$backtrace, exercise_path)
  help_text <- if (!is.null(runResults$help_text)) {
    HTML(paste("<p>", runResults$help_text), "</p>")
  } else {
    ""
  }
  html <- tags$html(tags$p(fail_name,
                           style = "color: Crimson;font-weight:bold"),
                    backtrace,
                    help_text,
                    tags$br())
  # print(str(html))
  # print(html)
  return(html)
}

# Reactively displays results depending on whether the
# show all results -checkbox is checked or not
.createTestResultElements <- function(testResults, showAll) {
  if (showAll) {
    return(lapply(seq_len(length(testResults)), function(i) {
      testResult <- testResults[[i]]
      .createTestResultElement(name = testResult$name,
                               status = testResult$status,
                               index = i,
                               message = testResult$message,
                               backtrace = testResult$backtrace)
    }))
  } else {
    return(.createSingleResultElement(testResults = testResults))
  }
}

# Creates an individual HTML paragraph element for the list displaying
# test results
.createTestResultElement <- function(name,
                                     status,
                                     index = NULL,
                                     message = NULL,
                                     backtrace) {
  # Assign a color depending on test status
  color <- ifelse(test = grepl(x = status, pattern = "pass"),
                  yes = "DarkGreen",
                  no = "Crimson")
  elements <- tags$p(paste(name, ":", status),
                     style = paste("color:", color, ";font-weight:bold"))

  #If status != pass, add de
  if (status != "pass") {
    elements <- list(elements,
                     .createDetailedTestResultElement(index,
                                                      message,
                                                      backtrace))
  }

  return(elements)
}

.createDetailedTestResultElement <- function(index = NULL,
                                             message = NULL,
                                             backtrace) {
##   create_message_html <- function() {
##     T this is not needed, the pre tag is better
##     # this creates a list with tags$br() replacing the <eol> characters
##     # this must be tested still on Windows
##     x <- unlist(strsplit(message, "\n"))
##     x <- as.character(x)
##     n <- length(x)
##     if (n <= 1) {
##       return(list(message))
##     }
##     N <- 2 * n - 1
##     y <- as.list(character(N))
##     y[seq(1, N, by = 2)] <- x
##     y[seq(2, N, by = 2)] <- rep(list(tags$br()), n - 1)
##   }
  .dprint(".createDetailedTestResultElement launched...")
  btn <- tags$button(id = paste("button_", index, sep = ""),
                     "Toggle details")
  id <- paste0("message_", index)
  style <- "display:none"
  fn <- tags$p
  fn2 <- tags$pre
  message <- do.call("fn", list("message:",
                                tags$br(),
                                fn2(message),
                                style = "font-weight: bold;"))
  .dprint(message)
  backtraceTags <- .backtraceHtmlTags(backtrace)
  script <- tags$script(paste0("$(\"#button_",
                               index,
                              "\").click(function(){$(\"#message_",
                              index,
                              "\").toggle()});"))
  .dprint(".createDetailedTestResultElement done")
  return(list(tags$div(message,
                       backtraceTags,
                       style = style,
                       id = id),
              btn,
              script))
}



# Creates an HTML paragraph element for either the first failing test or
# a separate message if all tests passed
.createSingleResultElement <- function(testResults) {
  for (i in seq_len(length(testResults))) {
    result <- testResults[[i]]

    if (identical(x = result$status, y = "fail")) {
      return(.createTestResultElement(name = result$name,
                                      status = result$status,
                                      index = i,
                                      message = result$message,
                                      backtrace = result$backtrace))
    }
  }

  return(.createTestResultElement(name = "All tests", status = "pass"))
}

# Returns backtrace as html p tags.
.backtraceHtmlTags2 <- function(backtrace, exercise_path) {
  .dprint("the backtrace handling")
  # print(str(backtrace))
  val2 <- unlist(backtrace)
  val2[1] <- sub("Error in source\\(.*\\) : ", "Error in source(...) :\n", val2[1])
  val2[1] <- sub("Error in eval\\(.*\\) : ",   "Error in eval(...) :\n", val2[1])
  val2[1] <- sub(paste0(exercise_path, "/R"), ".../R", val2[1])
  val2[1] <- sub("/app/R",  ".../R", val2[1])
  val2[1] <- sub(".../R//", ".../", val2[1])
  val2[1] <- sub(".../R/",  ".../", val2[1])
  #val2[1] <- sub(paste0(exercise_path, "/R/"), "...", val2[1])
  val2[1] <- sub(" : ", " :\n", val2[1])
  # val <- lapply(backtrace, tags$pre)
  # print(str(val))
  # print(str(val2))
  val2 <- paste(val2, collapse = "\n")
  # print(str(val2))
  # print(val2)
  # print(val)
  val2 <- tags$pre(val2)
  # print(val2)
  return(val2)
}
# Returns backtrace as html p tags.
.backtraceHtmlTags <- function(backtrace) {
  # print("the backtrace handling")
  # print(backtrace)
  val <- lapply(backtrace, tags$p)
  # print(str(val))
  # print(val)
  return(val)
}
