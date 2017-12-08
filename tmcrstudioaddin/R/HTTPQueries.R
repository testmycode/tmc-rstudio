#Exercise_id is the identifier of the exercise. For example, 36463.
#Target is the place where zip-file is stored, if it's not deleted.
download_exercise <- function(exercise_id,
                        zip_target = getwd(),
                        zip_name = "temp.zip",
                        exercise_directory,
                        exercise_name) {
  credentials <- tmcrstudioaddin::getCredentials()
  token <- credentials$token
  serverAddress <- credentials$serverAddress

  zip_path <- paste(sep = "", zip_target, "/", zip_name)

  exercises_url <- paste(sep = "", serverAddress, "/", "api/v8/core/exercises/",
                        exercise_id, "/", "download")



  exercises_response <- httr::GET(exercises_url,
                               httr::add_headers(Authorization = token),
                              config = timeout(30),
                              write_disk(zip_path, overwrite = FALSE))

  .tmc_unzip(zipfile_name = zip_path, target_folder = exercise_directory)

  file.remove(zip_path)

  create_exercise_metadata(exercise_id, exercise_directory, exercise_name)


  return(exercises_response)
}

# Zips and uploads a single exercise, which is located in project_path.
# Returns the response, which contains a field $submission_url containing
# the details of the submission.
upload_exercise <- function(token, exercise_id, project_path,
                             server_address, zip_name = "temp",
                             remove_zip = TRUE) {
  base_url <- server_address
  exercises_url <- paste(sep = "", base_url, "api/v8/core/exercises/",
                         exercise_id, "/", "submissions")
  url_config <- httr::add_headers(Authorization = token)

  .tmc_zip(project_path, zip_name)
  zipped_file <- paste(sep = "", getwd(), "/", zip_name, ".zip")
  submission_file <- httr::upload_file(zipped_file)

  exercises_response <- httr::POST(exercises_url,
                              config = url_config,
                              encode = "multipart",
                              body = list("submission[file]" = submission_file))

  if (remove_zip) {
    file.remove(paste(sep = "", zip_name, ".zip"))
  }

  return(exercises_response)
}

# Returns details of the submission in url
get_submission_json <- function(token, url) {
  url_config <- httr::add_headers(Authorization = token)

  exercises_response <- httr::GET(url, config = url_config)

  return(exercises_response)
}

# Zips the current working directory and uploads it to the server
# Uses the path of the currently active R-project by default
# For testing purposes, you can provide some other file path
upload_current_exercise <- function(token, project_path, zip_name = "temp", remove_zip = TRUE) {
  metadata <- tryCatch({
    json <- base::list.files(path = project_path, pattern = ".metadata.json", all.files = TRUE, full.names = TRUE)
    jsonlite::fromJSON(txt = json, simplifyVector = FALSE)
  }, error = function(e){
    print(e)
    NULL
  })
  if (!is.null(metadata$id[[1]])) {
    id <- metadata$id[[1]]
    credentials <- tmcrstudioaddin::getCredentials()
    address <- paste(sep = "", credentials$serverAddress, "/")

    response <- upload_exercise(token = token, exercise_id = id,
                                project_path = project_path, server_address = address,
                                zip_name = zip_name, remove_zip = remove_zip)
    return(response)
  } else {
    return(NULL)
  }
}

getAllOrganizations <- function(){
  organizations <- tryCatch({
    credentials <- tmcrstudioaddin::getCredentials()
    url <- paste(credentials$serverAddress, '/api/v8/org.json', sep = "")
    token <- credentials$token
    req <-  httr::stop_for_status(httr::GET(url = url,
      httr::add_headers(Authorization = token), config = timeout(30), encode = "json"))
    jsonlite::fromJSON(httr::content(req, "text"))
  }, error = function(e){
    list(name = list(),slug = list())
  })
  return(organizations)
}

getAllCourses <- function(organization) {
  courses <- tryCatch({
    credentials <- tmcrstudioaddin::getCredentials()
    serverAddress <- credentials$serverAddress
    token <- credentials$token
    url <- paste(serverAddress, "/api/v8/core/org/", organization, "/courses", sep = "")
    req <- httr::stop_for_status(httr::GET(url = url, config = httr::add_headers(Authorization = token), encode = "json",timeout(30)))
    jsonlite::fromJSON(httr::content(req, "text"))
  }, error = function(e){
    list(id=list(),name = list())
  })
  return(list(id=courses$id,name=courses$title))
}

getAllExercises <- function(course){
  exercises <- tryCatch({
    credentials <- tmcrstudioaddin::getCredentials()
    serverAddress <- credentials$serverAddress
    token <- credentials$token
    url <- paste(serverAddress, "/api/v8/courses/",course, "/exercises", sep = "")
    req <- httr::stop_for_status(httr::GET(url = url,httr::add_headers(Authorization = token), config = timeout(30), encode = "json"))
    jsonlite::fromJSON(httr::content(req, "text"))

  }, error = function(e){
      list()
  })
}

get_json_from_submission_url <- function(response, token) {
  output <- tryCatch({
    url <- httr::content(response)
    httr::content(get_submission_json(token, url$submission_url))
  }, error = function(e) {
    if(!is.null(url$error)) print(url$error)
    print(e)
    NULL
  })
  return(output)
}
