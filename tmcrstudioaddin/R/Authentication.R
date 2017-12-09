library(httr)
library(jsonlite)

#' @title Authenticate to a TMC server
#' @description Fetches the client id and secret from the server and logs in to the server.
#' @usage Authenticate(username, password, serverAddress)
#' @param username Username of a TMC account.
#' @param password Password matching the inputed username of a TMC account.
#' @param serverAddress Address of the TMC server which the user wants to log in to.
#' @details Uses the OAuth2 protocol for authentication. Calls \code\{\link{fetchClientIdAndSecret}} for
#' fetching the client id and secret from the server and \code{\link{login}} for the actual login process.
#' @return An OAuth2 token if the authentication was succesful, otherwise returns an error message.
#' @examples authenticate(username = "test", password = "hello123", serverAddress = "https://tmc.mooc.fi")
#' @seealso \code\{\link{fetchClientIdAndSecret}}, \code{\link{login}}

#authentication first fetches clientId and secret after that logs in with username and password
authenticate <- function(username, password, serverAddress) {

  tryCatch({
    response <- tmcrstudioaddin::fetchClientIdAndSecret(serverAddress)
    status_code <- status_code(response)
    if (status_code == 200){
      clientID <- httr::content(response)$application_id
      secret <- httr::content(response)$secret
      return(tmcrstudioaddin::login(clientID,
          secret, username, password, serverAddress))
    } else{
        stop()
      }
    },
    error = function(e){
    response <- list(error = "Bad request",
        error_description = "Invalid request")

    return(response)
  })
}

#' @title Login to a TMC server
#' @description Logs in to a TMC server using an username and a password along with a clientID and secret used
#' for OAuth2 authentication.
#' @usage login(clientID, secret, username, password, serverAddress)
#' @param clientID Client ID used in OAuth2.
#' @param secret Secret used in OAuth2.
#' @param username Username of a TMC account.
#' @param password Password matching the inputed username of a TMC account.
#' @param serverAddress Address of the TMC server which the user wants to log in to.
#' @details Called from the \code{\link{authenticate}} function, which provides the client id and secret
#' used in this function's parameters. If logging in was successful, saves the login credentials (username,
#' access-token and the server address) in the credentials file.
#' @return An OAuth2 token if the authentication was succesful, otherwise returns an error message.
#' @seealso \code{\link{authenticate}}
#actual login function
login <- function(clientID, secret, username, password, serverAddress){
  body <- paste(sep = "",
                "grant_type=password&client_id=", clientID,
                "&client_secret=", secret,
                "&username=", username,
                "&password=", password)
  # Authenticate
  url <- paste(serverAddress, "/oauth/token", sep = "")
  req <- httr::POST(url = url, body = body, config = timeout(30))
  # if http status is ok return token
  if (status_code(req) == 200){
    token <- paste("Bearer", httr::content(req)$access_token)
    credentials <- list(username = username,token = token, serverAddress = serverAddress)
    tmcrstudioaddin::saveCredentials(credentials)

    return(token)
  }
  else if(status_code(req) == 401){
    response <- list(error = "Invalid credentials", error_description = "Check your username and/or password")
  }
  else{
    response <- list(error = "Error", error_description = "Something went wrong. Try again later")
    return(response)
  }
}


#' @title Fetch client id and secret from the server
#' @description Fetch the client id and secret required in OAuth2 authentication from the server.
#' @usage fetchClientIdAndSecret(serverAddress)
#' @param serverAddress Address of the TMC server which the user wants to log in to.
#' @details Called from the \code{\link{authenticate}} function which provides the desired TMC server address.
#' @return Client ID and secret fetched from the server.
#' @seealso \code{\link[=func]{authenticate}}
fetchClientIdAndSecret <-function(serverAddress){
  url <- paste(serverAddress, "/api/v8/application/rstudio_plugin/credentials.json", sep = "")
  req <- httr::GET(url = url, config = timeout(30))
  return(req)
}

#' @title Delete the saved credentials
#' @description Delete the saved credentials.
#' @usage deleteCredentials()
#' @return True if succeeded in deleting the credentials. False if the deletion did not succeed or the saved
#' credentials were not found.
deleteCredentials <- function(){
  if (file.exists(".credentials")){
    file.remove(".credentials")
  }
}


#' @title Get the saved server address
#' @description Get the saved server address.
#' @usage getServerAddress()
#' @return The saved server from the .server file. NULL if the file does not exists or if it is corrupted.
getServerAddress <- function(){

  if(!file.exists(".server")){
    return(NULL)
  }
  #read credentials from file, catch if file is corrupted
  server <- tryCatch(scan(".server", what = character(), quiet = TRUE),
    error = function(e) NULL)

  return(server)
}

#' @title Save the TMC user crendentials into a rds file.
#' @description Save the TMC user credentials (username, access token and the server address) into a rds file.
#' @usage saveCredentials(credentials)
#' @param credentials The user credentials to be saved. Is assumed to be in a list format with the keys \code{username},
#' \code{token} and \code{serverAddress}.
#' @details The file is named \code{credentials.rds} and is saved on the \code{tmcr}-folder where the exercises are
#' downloaded into.
#' @return Always NULL.
#' @examples
#' credentials <- list(username = username,token = token, serverAddress = serverAddress)
#' saveCredentials(credentials)
saveCredentials <- function(credentials){
  credentials_path <- paste(get_tmcr_directory(), ".credentials.rds",
    sep = .Platform$file.sep)
  saveRDS(credentials, credentials_path)
}

#' @title Get the TMC user credentials from a file
#' @description Get the TMC user credentials (username, access token and the server address) from the
#' \code{credentials.rds} file.
#' @usage getCredentials()
#' @return A list with the keys \code{username}, \code{token} and \code{serverAddress} and their corresponding values.
#' If reading the file was unsuccessful then an empty list is returned instead.
getCredentials <- function(){
  credentials <- list()
  tryCatch({
    credentials <- readRDS(paste(get_tmcr_directory(), ".credentials.rds",
      sep = .Platform$file.sep))
  }, warning = function(e){})
  return(credentials)
}
