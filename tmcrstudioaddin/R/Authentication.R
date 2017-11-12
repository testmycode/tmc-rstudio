library(httr)
library(jsonlite)
#authentication first fetches clientId and secret after that logs in with username and password
authenticate <- function(username, password, serverAddress) {

  tryCatch({
    response <- tmcrstudioaddin::fetchClientIdAndSecret(serverAddress)
    status_code <- status_code(response)
    if(status_code == 200){
      clientID <- httr::content(response)$application_id
      secret <- httr::content(response)$secret
      return(tmcrstudioaddin::login(clientID, secret, username, password, serverAddress))
    }
    else{
      stop()
    }
  }, error = function(e){
    response <-list(error_description = "Invalid request", error = "Bad request")
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
  req <- httr::POST(url = url, body = body)
  # if http status is ok return token
  if (status_code(req) == 200){
    # Extract the authentication token
    httr::stop_for_status(x = req, task = "Authenticate with TMC")
    token <- paste("Bearer", httr::content(req)$access_token)
    credentials <- list(username = username,token = token, serverAddress = serverAddress)
    tmcrstudioaddin::saveCredentials(credentials)
    return(token)
  }
  else{
    response <- list(error_description = "Check your username and/or password",error = "Invalid credentials")
    return(response)
  }
}

fetchClientIdAndSecret <-function(serverAddress){
  url <- paste(serverAddress, "/api/v8/application/rstudio_plugin/credentials.json", sep = "")
  req <- httr::GET(url = url)
  return(req)
}
# Temporary testing/example function that fetches the data of a single course from TMC
tempGetCourse <- function(token) {
  url <- "https://tmc.mooc.fi/api/v8/courses/199"
  req <- httr::GET(url = url, config = httr::add_headers(Authorization = token))
  httr::stop_for_status(x = req, task = "Fetching data from the TMC API")
  course <- httr::content(req)
  return(course)
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
  saveRDS(credentials, ".credentials.rds")
}
getCredentials <- function(){
  credentials <- list()
  tryCatch({
    credentials <- readRDS(".credentials.rds")
  }, warning = function(e){})
  return(credentials)
}
