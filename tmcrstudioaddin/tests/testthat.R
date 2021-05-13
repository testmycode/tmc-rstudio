# install.packages("httptest", repos="http://cran.r-project.org")
# install.packages("mockery", repos="http://cran.r-project.org")
library(testthat)
library(mockery)
library(httptest)
detach("package:tmcrstudioaddin", unload = TRUE)
detach("package:tmcRtestrunner", unload = TRUE)
library(tmcRtestrunner)
library(tmcrstudioaddin)

test_check("tmcrstudioaddin")

    response <- login("a", "b", "c", "d", "tmc.mooc.fi")

login <- function(clientID, secret, username, password, serverAddress) {
  body <- list("grant_type" = "password",
               "client_id" = clientID,
               "client_secret" = secret,
               "username" = username,
               "password" = password)
  # Authenticate
  url <- paste(serverAddress, "/oauth/token", sep = "")
  req <- httr::POST(url = url,
                    body = body,
                    config = httr::timeout(30),
                    encode = "form")
  # if http status is ok return token
   .dcat("req", req)
#   print(req)
  if (status_code(req) == 200) {
    token <- paste("Bearer", httr::content(req)$access_token)
    credentials <- list(username      = username,
                        token         = token,
                        serverAddress = serverAddress,
                        organization  = NULL)
    # print("saveCredentials site A")
    tmcrstudioaddin::saveCredentials(credentials)
    response <- list(token             = token,
                     error             = NULL,
                     error_description = NULL)
  } else if (status_code(req) == 400) {
    response <- list(token             = NULL,
                     error             = "Bad request",
                     error_description = "Check your username and/or password")
  } else if (status_code(req) == 401) {
    response <- list(token             = NULL,
                     error             = "Invalid credentials",
                     error_description = "Check your username and/or password")
  } else {
    response <- list(token             = NULL,
                     error             = "Error",
                     error_description = "Something went wrong. Try again later")
  }
  return(response)
}

.dcat <- function(name, x) {
  cat(paste0("<", name, ">\n"))
  print(x)
  cat(paste0("</", name, ">\n"))
}
