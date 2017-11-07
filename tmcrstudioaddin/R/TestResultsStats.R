library(jsonlite)

.testsPassedPercentage <- function(testResults) {
  if (length(testResults) == 0) return("100%")
  passed <- 0
  for (testResult in testResults) {
    if (testResult$status == "pass") {
      passed <- passed + 1
    }
  }

  passedPercentage <- round(passed / length(testResults), digit = 2)
  return(paste(sep = "", toString(passedPercentage * 100), "%"))
}

processSubmission <- function(output) {
  tests <- list()
  for (test_case in output$test_cases) {
    result <- list()
    result[["name"]] <- test_case$name
    result[["status"]] <- .getStatusFromBoolean(test_case$successful)
    result[["message"]] <- test_case$message
    tests[[length(tests) + 1]]  <- result
  }
  return(tests)
}

.getStatusFromBoolean <- function(bol) {
  status <- "fail"
  if (bol) {
    status <- "pass"
  }
  return(status)
}
