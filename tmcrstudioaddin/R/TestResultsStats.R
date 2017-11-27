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
