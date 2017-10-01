library(rjson)
library(httr)

authenticate <- function(username, password) {
  client_id <- "d56662a9898966d8275edb2e609bf1c52919ca7a4e3797bd663e40833522ab3b"
  secret <- "86ef820fe2e191c1dfd61ccd21c60c42ddbc4de9adba0743e07917adeeefb554"
  body <- paste(sep = "",
                "grant_type=password&client_id=", client_id,
                "&client_secret=", secret,
                "&username=", username,
                "&password=", password)
  # Authenticate
  req <- httr::POST(url = "https://tmc.mooc.fi/oauth/token", body = body)

  # if http status is ok return token
  if (status_code(req) == 200){
    # Extract the authentication token
    httr::stop_for_status(x = req, task = "Authenticate with TMC")
    token <- paste("Bearer", httr::content(req)$access_token)
    return(token)
  }
  else{
    return(httr::content(req))
  }
}

# ?POST
# ?stop_for_status
# ?content

# Temporary testing/example function that fetches the data of a single course from TMC
temp_get_course <- function(token) {
  url <- "https://tmc.mooc.fi/api/v8/courses/199"

  req <- httr::GET(url = url, config = httr::add_headers(Authorization = token))
  httr::stop_for_status(x = req, task = "Fetching data from the TMC API")
  course <- httr::content(req)

  return(course)
}

# note: ord_id is a string, not int
temp_get_all_courses <- function(token, org_id) {
  url <- paste("https://tmc.mooc.fi/api/v8/core/org/", org_id, "/courses", sep = "")
  # url <- "https://tmc.mooc.fi/api/v8/core/org/hy/courses"
  print(url)

  req <- httr::GET(url = url, config = httr::add_headers(Authorization = token))
  httr::stop_for_status(x = req, task = "Fetching data from the TMC API")
  courses <- httr::content(req)

  return(courses)
}
