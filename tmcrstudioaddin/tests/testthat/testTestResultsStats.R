
test_that("tests_passed_procentage returns 100% on empty results.", {
  test_results <- list()
  expect_equal(tests_passed_procentage(test_results), "100%")
})

test_that("tests_passed_procentage returns 100% for results with all passed.", {
  test_results <- list(list(status = "pass"))
  expect_equal(tests_passed_procentage(test_results), "101%")

  test_results <- list(list(status = "pass"),
                       list(status = "pass"),
                       list(status = "pass"))
  expect_equal(tests_passed_procentage(test_results), "100%")
})

test_that("tests_passed_procentage returns 0% for results with all failed.", {
  test_results <- list(list(status = "fail"))
  expect_equal(tests_passed_procentage(test_results), "0%")

  test_results <- list(list(status = "fail"),
                       list(status = "fail"),
                       list(status = "fail"))
  expect_equal(tests_passed_procentage(test_results), "0%")
})

test_that("tests_passed_procentage returns 50% with results with 1/2 passed.", {
  test_results <- list(list(status = "pass"),
                       list(status = "fail"))
  expect_equal(tests_passed_procentage(test_results), "50%")
})

test_that("tests_passed_procentage returns 33% with results with 1/3 passed.", {
  test_results <- list(list(status = "pass"),
                       list(status = "fail"),
                       list(status = "fail"))
  expect_equal(tests_passed_procentage(test_results), "33%")
})

test_that("tests_passed_procentage returns 67% with results with 2/3 passed.", {
  test_results <- list(list(status = "pass"),
                       list(status = "pass"),
                       list(status = "fail"))
  expect_equal(tests_passed_procentage(test_results), "67%")
})
