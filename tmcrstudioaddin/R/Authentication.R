library(httr)
library(jsonlite)

#' @title Authenticate to a TMC server
#'
#' @description Fetches the client id and secret from the server and
#' logs in to the server.
#'
#' @usage authenticate(username, password, serverAddress)
#'
#' @param username Username of a TMC account.
#' @param password Password matching the inputed username of a TMC account.
#' @param serverAddress Address of the TMC server which the user wants
#' to log in to.
#'
#' @details Uses the \code{OAuth2} protocol for authentication. Fetches
#' the client id and secret from the server and uses them along with the
#' username and password to login to the server.
#'
#' @return a named list of an \code{OAuth2} token if the authentication was succesful,
#' with NULL error message. Otherwise returns an error message with NULL
#' token.
#'
#' @examples authenticate(username = "test", password = "hello123",
#' serverAddress = "https://tmc.mooc.fi")
#'
#' @seealso \code{\link{fetchClientIdAndSecret}},
#' \code{\link[httr]{status_code}}, \code{\link[httr]{content}},
#' \code{\link{login}}

# authentication first fetches clientId and secret after that logs in
# with username and password
authenticate <- function(username, password, serverAddress) {
  .dprint("authenticate()")
  tryCatch({
    response   <- tmcrstudioaddin::fetchClientIdAndSecret(serverAddress)
    status_code <- status_code(response)
    if (status_code == 200) {
      clientID <- httr::content(response)$application_id
      secret   <- httr::content(response)$secret
      response <- tmcrstudioaddin::login(clientID,
                                         secret,
                                         username,
                                         password,
                                         serverAddress)
      return(response)
    } else {
        stop()
    }
  },
  error = function(e) {
    response <- list(token             = NULL,
                     error             = "Bad request",
                     error_description = "Invalid request")
    return(response)
  })
}

#' @title Login to a TMC server
#'
#' @description Logs in to a TMC server using an username and a password
#' along with a clientID and secret used for \code{OAuth2}
#' authentication.
#'
#' @usage login(clientID, secret, username, password, serverAddress)
#'
#' @param clientID Client ID used in \code{OAuth2}.
#' @param secret Secret used in \code{OAuth2}.
#' @param username Username of a TMC account.
#' @param password Password matching the inputed username of a TMC account.
#' @param serverAddress Address of the TMC server which the user wants
#' to log in to.
#'
#' @details If logging in was successful, saves the login credentials
#' (username, access-token and the server address) in the
#' \code{.credentials} file.
#'
#' @return a named list of an \code{OAuth2} token if the authentication was succesful,
#' with NULL error message. Otherwise returns an error message with NULL
#' token.
#'
#' @seealso \code{\link[httr]{POST}}, \code{\link[httr]{status_code}},
#' \code{\link[httr]{content}}, \code{\link{saveCredentials}}

# actual login function
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
  if (status_code(req) == 200) {
    token <- paste("Bearer", httr::content(req)$access_token)
    credentials <- list(username      = username,
                        token         = token,
                        serverAddress = serverAddress,
                        organization  = NULL)
    .dprint("saveCredentials site A")
    tmcrstudioaddin::saveCredentials(credentials)
    response <- list(token             = token,
                     error             = NULL,
                     error_description = NULL)
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


#' @title Fetch client id and secret from the server
#'
#' @description Fetch the client id and secret required in \code{OAuth2}
#' authentication from the server.
#'
#' @usage fetchClientIdAndSecret(serverAddress)
#'
#' @param serverAddress Address of the TMC server which the user wants
#' to log in to.
#'
#' @return Client ID and secret fetched from the server.
#'
#' @seealso \code{\link[httr]{GET}}
fetchClientIdAndSecret <- function(serverAddress) {
  .dprint("fetchClientIdAndSecret()")
  url <- paste(serverAddress,
               "/api/v8/application/rstudio_plugin/credentials.json",
               sep = "")
  req <- httr::GET(url = url, config = httr::timeout(30))
  return(req)
}

#' @title Delete the saved user credentials
#'
#' @description Delete the \code{.credentials} file.
#'
#' @usage deleteCredentials()
#'
#' @return  \code{TRUE} if succeeded in deleting the credentials.
#' \code{FALSE} if the deletion did not succeed or \code{.credentials}
#' were not found.
#'
#' @seealso \code{\link[base]{files}}
deleteCredentials <- function() {
  if (file.exists(".credentials")) {
    file.remove(".credentials")
  }
}

#' @title Get the saved server address
#'
#' @description Get the saved server address from the \code{.server} file.
#'
#' @usage getServerAddress()
#'
#' @return The saved server address from the \code{.server} file.
#' \code{NULL} if the file does not exist or if it is corrupted.
#'
#' @seealso \code{\link[base]{file.exists}}, \code{\link[base]{scan}}
getServerAddress <- function() {
  if (!file.exists(".server")) {
    return(NULL)
  }

  # read credentials from file, catch if file is corrupted
  server <- tryCatch({
    scan(".server", what = character(), quiet = TRUE)},
    error = function(e) NULL)

  return(server)
}

#' @title Save the TMC user crendentials into a \code{rds} file.
#'
#' @description Save the TMC user credentials (username, access token
#' and the server address) into a \code{rds} file.
#'
#' @usage saveCredentials(credentials)
#'
#' @param credentials The user credentials to be saved. Is assumed to be
#' in a list format with the keys \code{username}, \code{token} and
#' \code{serverAddress}.
#'
#' @details The file is named \code{credentials.rds} and is saved on the
#' \code{tmcr} folder.
#'
#' @return Always \code{NULL}.
#'
#' @seealso \code{\link{get_tmcr_directory}}, \code{\link[base]{readRDS}}
saveCredentials <- function(credentials) {
  .dprint("saveCredentials()")
  credentials_path <- paste(get_tmcr_directory(),
                            ".credentials.rds",
                            sep = .Platform$file.sep)
  saveRDS(credentials, credentials_path)
}

#' @title Get the TMC user credentials
#'
#' @description Get the TMC user credentials (username, access token and
#' the server address) from the \code{credentials.rds} file.
#'
#' @usage getCredentials()
#'
#' @return A list with the keys \code{username}, \code{token} and
#' \code{serverAddress} and their corresponding values. If reading the
#' file was unsuccessful then an empty list is returned instead.
#'
#' @seealso \code{\link[base]{readRDS}}
getCredentials <- function() {
  credentials <- list()
  .dprint("getCredentials()")
  tryCatch({
    credentials <- readRDS(paste(get_tmcr_directory(),
                                 ".credentials.rds",
                                 sep = .Platform$file.sep))
  }, warning = function(e) {
    .dprint(e)
  })
  return(credentials)
}
