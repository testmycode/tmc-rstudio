test_that("Authentication gives a token and saves credentials to file", {
  if(file.exists(".credentials")){
    file.remove(".credentials")
  }
  token <- authenticate("rtest", "asdasdasd")
  expect_is(token, "character")
  expect_true(file.exists(".credentials"))
})

test_that("False authentication fails and creates no file", {
  if(file.exists(".credentials")){
    file.remove(".credentials")
  }
  expect_is(authenticate("sddsdaddsdsdadasads", "dsdasasdsdsdasdasdasd"), "list")
  expect_true(!file.exists(".credentials"))
})
