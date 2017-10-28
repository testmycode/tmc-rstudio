test_that("Authentication gives a token and saves credentials to file", {
  if(file.exists(".credentials")){
    file.remove(".credentials")
  }
  token <- authenticate("rtest", "asdasdasd","https://tmc.mooc.fi")
  expect_is(token, "character")
  expect_true(file.exists(".credentials"))
})

test_that("False authentication (wrong username and password) fails and creates no file", {
  if(file.exists(".credentials")){
    file.remove(".credentials")
  }
  expect_is(authenticate("sddsdaddsdsdadasads", "dsdasasdsdsdasdasdasd","https://tmc.mooc.fi"), "list")
  expect_true(!file.exists(".credentials"))
})

test_that("False authentication (wrong server address) fails and creates no file", {
  if(file.exists(".credentials")){
    file.remove(".credentials")
  }
  expect_is(authenticate("rtest", "asdasdasd","https://tmc.mo"), "list")
  expect_true(!file.exists(".credentials"))
})

