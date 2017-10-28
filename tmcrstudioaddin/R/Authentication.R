library(httr)

authenticate <- function(username, password,serverAddress) {
  response <- fetchClientIdAndSecret(serverAddress)
  if(tryCatch(status_code(response)==200,error = function(e)FALSE)){
    clientID <- httr::content(response)$application_id
    secret <- httr::content(response)$secret
    body <- paste(sep = "",
                "grant_type=password&client_id=", clientID,
                "&client_secret=", secret,
                "&username=", username,
                "&password=", password)
    # Authenticate
    url = paste(serverAddress,"/oauth/token",sep="")
    req <- httr::POST(url = url, body = body)
    # if http status is ok return token
    if (status_code(req) == 200){
      # Extract the authentication token
      httr::stop_for_status(x = req, task = "Authenticate with TMC")
      token <- paste("Bearer", httr::content(req)$access_token)
      credentials <- c(username,token,serverAddress)
      saveCredentials(credentials)
      return(token)
      }
    else{
      response <-c()
      response$error <- "Invalid credentials"
      response$error_description <- "Check your username and/or password"
      return(response)
    }
  }
  else{
    response <-c()
    response$error <- "Invalid server address"
    response$error_description <- "Check your server address"
    return(response)
  }
}
fetchClientIdAndSecret <-function(serverAddress){
  url = paste(serverAddress,"/api/v8/application/rstudio_plugin/credentials.json",sep="")
  req <- tryCatch(httr::GET(url = url),error = function(e) NA)
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

# note: ord_id is a string, not int
tempGetAllCourses <- function(token, orgID) {
  url <- paste("https://tmc.mooc.fi/api/v8/core/org/", orgID, "/courses", sep = "")
  # url <- "https://tmc.mooc.fi/api/v8/core/org/hy/courses"

  req <- httr::GET(url = url, config = httr::add_headers(Authorization = token))
  httr::stop_for_status(x = req, task = "Fetching data from the TMC API")
  courses <- httr::content(req)

  return(courses)
}

deleteCredentials <- function(){
  if(file.exists(".credentials")){
    file.remove(".credentials")
  }
}

saveCredentials <- function(credentials){
  write(credentials,".credentials")
}

getCredentials <- function(){
  if(!file.exists(".credentials")){
    return(NULL)
  }
  #read credentials from file, catch if file is corrupted
  credentials<-tryCatch(scan(".credentials", what = character()), error = function(e) NULL)
  return(credentials)
}
