library('testthat')

test_that('createTestResultsHtml returns right html for testResults with no results', {
  #No results testResult
  testResults <- list()
  html <- createTestResultsHtml(testResults  = testResults,
                                showAll      = FALSE,
                                submitted_at = NULL)

  #First element in html should contain "No tests for exercise.".
  expect_equal(html$children[[1]]$children[[1]], "No tests for exercise.")
})

test_that('createTestResultsHtml html contains All tests pass element when all tests pass and
          showall is not selected.', {
  passedTestResult <- list("name" = "test_name",
                           "status" = "pass",
                           "message" = "",
                           "backtrace" = list())
  testResults <- list(passedTestResult)

  # html as character vector
  html_str <- format(createTestResultsHtml(testResults  = testResults,
                                           showAll      = FALSE,
                                           submitted_at = NULL))

  # Html should contain "All tests : pass"
  expect_true(grepl("All tests : pass", html_str))
  # Html should not contain "test_name : pass"
  expect_false(grepl("test_name : pass", html_str))
})

test_that('createTestResultsHtml html contains test result elements when all tests pass and
          showall is selected.', {
  passedTestResult <- list("name" = "test_name", "status" = "pass", "message" = "",
                                     "backtrace" = list())
  testResults <- list(passedTestResult)

  # html as character vector
  html_str <- format(createTestResultsHtml(testResults  = testResults,
                                           showAll      = TRUE,
                                           submitted_at = NULL))

  # Html should contain "test_name : pass"
  expect_true(grepl("test_name : pass", html_str))
  # html should not contain "All tets : pass"
  expect_false(grepl("All tests : pass", html_str))
})

test_that('createTestResultsHtml html contains details for failed test.', {
  #No results testResult
  failedTestResult <- list("name" = "test", "status" = "fail", "message" = "failed",
                           "backtrace" = list())
  testResults <- list(failedTestResult)

  #html as character vector
  html_str <- format(createTestResultsHtml(testResults  = testResults,
                                           showAll      = FALSE,
                                           submitted_at = NULL))

  #Html should contain detailed html elements
  expect_true(grepl("<button id=\"button_1\">Toggle details</button>", html_str))
})


test_that('createRunSourcingFailHtml returns right html for sourcing fail', {
  #Sourcing fail runResult
  submission_info = list(submission_id = TRUE,
                         submitted_at  = NULL)
  runResult <- list(run_status      = "local_sourcing_failed",
                    backtrace       = list(),
                    submission_info = submission_info,
                    submitted_at    = NULL,
                    backtrace       = list(),
                    help_text       = list("help"),
                    test_results    = list())
  html <- createRunSourcingFailHtml(runResult = runResult,
                                    exercise_path = ".",
                                    submission_info = submission_info)

  #First element in html should contain "Sourcing fail"
  expect_equal(html$children[[1]]$children[[2]]$children[[1]],
               "Sourcing of exercises failed. There is an error in your code.")
})

test_that('createRunSourcingFailHtml returns right html for run fail', {
  #run fail runResult
  submission_info = list(submission_id = TRUE,
                         submitted_at  = NULL)
  runResult <- list(run_status      = "run_fail",
                    backtrace       = list(),
                    submission_info = submission_info,
                    submitted_at    = NULL,
                    backtrace       = list(),
                    help_text       = list("help"),
                    test_results    = list())
  html <- createRunSourcingFailHtml(runResult = runResult,
                                    exercise_path = ".",
                                    submission_info = submission_info)

  #First element in html should contain "Run fail"
  expect_equal(html$children[[1]]$children[[2]]$children[[1]], "Run fail")
})
