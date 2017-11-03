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

processSubmissionJson <- function(json) {
  output <- fromJSON(json, simplifyVector = FALSE)
  exercises <- list()
  for (submission in output$submissions) {
    result <- list()
    result[["name"]] <- submission$exercise_name
    result[["status"]] <- .getStatusFromBoolean(submission$all_tests_passed)
    result[["message"]] <- ""
    exercises[[length(exercises) + 1]]  <- result
  }
  return(exercises)
}

.getStatusFromBoolean <- function(bol) {
  status <- "fail"
  if (bol) {
    status <- "pass"
  }
  return(status)
}
