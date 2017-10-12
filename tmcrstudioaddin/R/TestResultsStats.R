tests_passed_procentage <- function(test_results) {
  if (length(test_results) == 0) return("100%")

  passed <- 0
  for (test_result in test_results) {
    if (test_result$status == "pass") {
      passed <- passed + 1
    }
  }

  passed_procentage <- round(passed / length(test_results), digit = 2)
  return(paste(sep = "", toString(passed_procentage * 100), "%"))
}
