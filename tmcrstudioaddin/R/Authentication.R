library(httr)
library(jsonlite)
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

fetchClientIdAndSecret <-function(serverAddress){
  url <- paste(serverAddress, "/api/v8/application/rstudio_plugin/credentials.json", sep = "")
  req <- httr::GET(url = url, config = timeout(30))
  return(req)
}


deleteCredentials <- function(){
  if (file.exists(".credentials")){
    file.remove(".credentials")
  }
}

getServerAddress <- function(){

  if(!file.exists(".server")){
    return(NULL)
  }
  #read credentials from file, catch if file is corrupted
  server <- tryCatch(scan(".server", what = character(), quiet = TRUE),
    error = function(e) NULL)

  return(server)
}
saveCredentials <- function(credentials){
  credentials_path <- paste(get_tmcr_directory(), ".credentials.rds",
    sep = .Platform$file.sep)
  saveRDS(credentials, credentials_path)
}
getCredentials <- function(){
  credentials <- list()
  tryCatch({
    credentials <- readRDS(paste(get_tmcr_directory(), ".credentials.rds",
      sep = .Platform$file.sep))
  }, warning = function(e){})
  return(credentials)
}
