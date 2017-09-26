test_that("Authentication gives a token", {
  token <- authenticate("rtest", "asdasdasd")
  expect_is(token, "character")
})

test_that("False authentication fails", {
  expect_is(authenticate("sddsdaddsdsdadasads", "dsdasasdsdsdasdasdasd"), "list")
})
