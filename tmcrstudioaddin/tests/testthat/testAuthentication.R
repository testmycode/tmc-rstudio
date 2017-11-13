context("Logging in")
httptest::with_mock_API({
  test_that("Fetching client ID and secret works", {
    response <- fetchClientIdAndSecret("tmc.mooc.fi")
    response <- httr::content(response)
    id <- response$application_id
    secret <- response$secret

    expect_is(id, "character")
    expect_is(secret, "character")
  })

  test_that("Login gives a token and saves credentials to file", {
    if (file.exists(".credentials.rds")){
      file.remove(".credentials.rds")
    }

    response <- login("a", "b", "c", "d", "tmc.mooc.fi")

    expect_is(response, "character")
    expect_equal(substr(response, 0, 6), "Bearer")

    expect_true(file.exists(".credentials.rds"))

    file.remove(".credentials.rds")
  })

  test_that("False authentication (wrong username and password) fails and creates no file", {
    if (file.exists(".credentials.rds")){
      file.remove(".credentials.rds")
    }

    expect_is(authenticate("sddsdaddsdsdadasads", "dsdasasdsdsdasdasdasd", "https://tmc.mooc.fi"), "list")
    expect_true(!file.exists(".credentials.rds"))
  })

  test_that("False authentication (wrong server address) fails and creates no file", {
    if (file.exists(".credentials.rds")){
      file.remove(".credentials.rds")
    }

    response <- authenticate("rtest", "asdasdasd", "https://tmc.mo")
    expect_is(response, "list")
    expect_true(!file.exists(".credentials.rds"))
  })

})
