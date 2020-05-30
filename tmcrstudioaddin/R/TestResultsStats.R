.testsPassedPercentage <- function(test_results) {
  .ddprint(".testsPassedPercentage")
  if (length(test_results) == 0) return("100%")
  passed <- 0
  for (test_result in test_results) {
    if (test_result$status == "pass") {
      passed <- passed + 1
    }
  }

  passed_percentage <- round(passed / length(test_results), digits = 2)
  return(paste(sep = "", toString(passed_percentage * 100), "%"))
}
