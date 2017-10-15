
test_that("testsPassedPercentage returns 100% on empty results.", {
  testResults <- list()
  expect_equal(testsPassedPercentage(testResults), "100%")
})

test_that("testsPassedPercentage returns 100% for results with all passed.", {
  testResults <- list(list(status = "pass"))
  expect_equal(testsPassedPercentage(testResults), "100%")

  testResults <- list(list(status = "pass"),
                       list(status = "pass"),
                       list(status = "pass"))
  expect_equal(testsPassedPercentage(testResults), "100%")
})

test_that("testsPassedPercentage returns 0% for results with all failed.", {
  testResults <- list(list(status = "fail"))
  expect_equal(testsPassedPercentage(testResults), "0%")

  testResults <- list(list(status = "fail"),
                       list(status = "fail"),
                       list(status = "fail"))
  expect_equal(testsPassedPercentage(testResults), "0%")
})

test_that("testsPassedPercentage returns 50% with results with 1/2 passed.", {
  testResults <- list(list(status = "pass"),
                       list(status = "fail"))
  expect_equal(testsPassedPercentage(testResults), "50%")
})

test_that("testsPassedPercentage returns 33% with results with 1/3 passed.", {
  testResults <- list(list(status = "pass"),
                       list(status = "fail"),
                       list(status = "fail"))
  expect_equal(testsPassedPercentage(testResults), "33%")
})

test_that("testsPassedPercentage returns 67% with results with 2/3 passed.", {
  testResults <- list(list(status = "pass"),
                       list(status = "pass"),
                       list(status = "fail"))
  expect_equal(testsPassedPercentage(testResults), "67%")
})
