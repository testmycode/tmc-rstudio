
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
