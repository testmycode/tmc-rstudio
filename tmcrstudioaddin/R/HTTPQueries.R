#' @title Download an exercise from the TMC server
#' @description Download an exercise from the TMC server.
#' @usage download_exercise(exercise_id, zip_target = getwd(), zip_name = "temp.zip",
#'                          exercise_directory, exercise_name)
#' @param exercise_id Id of the exercise.
#' @param zip_target Path to where the temporary zip file is stored. Defaults to the current
#' working directory.
#' @param zip_name Name of the temporary zip file. Default is \code{temp.zip}
#' @param exercise_directory Path to the directory where the exercise directory is unzipped to.
#' @param exercise_name Name of the downloaded exercise.
#' @details Reads the user credentials from the credentials file, downloads an zipped exercise
#' matching the given id from the TMC server, unzips it to the given directory and creates a
#' JSON file containing the exercise name and id on the exercise directory.
#' @return HTTP response to the download attempt.
#' @examples download_exercise(exercise_id = 36463, exercise_directory = getwd(),
#'                             exercise_name = "0_0_helloworld")

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

#' @title Upload an exercise submission to the TMC server
#' @description Upload an exercise submission to the TMC server
#' @usage upload_exercise(token, exercise_id, project_path, server_address,
#'         zip_name = "temp", remove_zip = TRUE)
#' @param token OAuth2 token associated with the current login session.
#' @param exercise_id Id of the submitted exercise.
#' @param project_path Path to the directory of the submitted exercise.
#' @param server_address Address to the TMC server where the exercise is submitted to.
#' @param zip_name Name of the zip file which contains the exercise submission. Default is
#' \code{temp}
#' @param remove_zip \code{TRUE} or \code{FALSE} depending on if you wish to delete the
#' submission zip file after sending it to the server. Defaults to \code{TRUE}.
#' @details Packs the exercise directory into a zip file and sends it to the TMC server.
#' @return HTTP response to the submission attempt.
#' @examples upload_exercise(token = token, exercise_id = 36463, project_path = getwd(),
#'            server_address = "https://tmc.mooc.fi")

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

#' @title Get exercise submission result JSON
#' @description Get the exercise submission JSON file from the TMC server.
#' @usage get_submission_json(token, url)
#' @param token OAuth2 token associated with the the current login session to the TMC server.
#' @param url URL where the exercise submission JSON is fetched from.
#' @details Makes a HTTP-GET request to fetch the exercise submission JSON from the
#' specified URL.
#' @return HTTP-response from the TMC server which contains the submission result JSON if
#' the server has finished processing the exercise submission.

# Returns details of the submission in url
get_submission_json <- function(token, url) {
  url_config <- httr::add_headers(Authorization = token)

  exercises_response <- httr::GET(url, config = url_config)

  return(exercises_response)
}

#' @title Upload the currently open exercise to the TMC server
#' @description Upload the currently open exercise to the TMC server.
#' @usage upload_current_exercise(token, project_path, zip_name = "temp", remove_zip = TRUE)
#' @param token OAuth2 token associated with the current login session.
#' @param project_path Path to the directory of the submitted exercise.
#' @param zip_name Name of the zip file which contains the exercise submission. Default is
#' \code{temp}
#' @param remove_zip \code{TRUE} or \code{FALSE} depending on if you wish to delete the
#' submission zip file after sending it to the server. Defaults to \code{TRUE}.
#' @details Reads the exercise id from \code{.metadata.json} and the server address from
#' \code{.credentials.json}. These along with the function parameters are passed to the actual
#' upload function, which is \code{\link{upload_exercise}}
#' @return HTTP response to the submission attempt. NULL if reading the metadata or credentials file
#' caused an error.
#' @examples upload_current_exercise(token = token, project_path = getwd())
#' @seealso \code{\link{upload_exercise}}

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

#' @title Get all TMC organizations
#' @description Get all TMC organizations.
#' @usage getAllOrganizations()
#' @details Reads the OAuth2 token and server address from \code{.credentials.json} and uses
#' them to make a HTTP-Get request for the list of organizations.
#' @return List of TMC organization names and slugs. If reading \code{.credentials.json}
#'  or sending the HTTP request failed, returns a list with 2 empty sublists called \code{name}
#'  and \code{slug}.
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

#' @title Get all courses offered by a TMC organization
#' @description Get all courses offered by a TMC organization.
#' @usage getAllCourses(organization)
#' @param organization Organization slug (Identifying URL-friendly name).
#' @details Reads the OAuth2 token and server address from \code{.credentials.json} and uses
#' them to make a HTTP-Get request for the list of courses of the organization.
#' @return List of course names and course ids belonging to the given organization.
#' If reading \code{.credentials.json} or sending the HTTP request failed, returns a list with
#' 2 empty sublists called \code{id} and \code{name}.
#' @examples getAllCourses("hy")
getAllCourses <- function(organization) {
  courses <- tryCatch({
    credentials <- tmcrstudioaddin::getCredentials()
    serverAddress <- credentials$serverAddress
    token <- credentials$token
    url <- paste(serverAddress, "/api/v8/core/org/", organization, "/courses", sep = "")
    req <- httr::stop_for_status(httr::GET(url = url, config = httr::add_headers(Authorization = token), encode = "json",timeout(30)))
    jsonlite::fromJSON(httr::content(req, "text"))
  }, error = function(e){
    list(id=list(),name = list(),title = list())
  })
  return(list(id = courses$id, name = courses$name, title = courses$title))
}

#' @title Get all exercises of a TMC course.
#' @description Get all exercises of a TMC course.
#' @usage getAllExercises(course)
#' @param course Id of the course.
#' @details Reads the OAuth2 token and server address from \code{.credentials.json} and uses
#' them to make a HTTP-Get request for the list of exercises in the course.
#' @return List of exercises in the course. If reading \code{.credentials.json} or sending
#' the HTTP request failed, returns an empty list.
#' @examples getAllExercies(242)
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

#' @title Get exercise submission result JSON
#' @description Get exercise submission result JSON from the TMC server.
#' @usage get_json_from_submission_url(response, token)
#' @param response HTTP response to the exercise submission.
#' @param token OAuth2 token associated with the current login session to the TMC server.
#' @details Extracts the exercise submission url from the given response and calls the
#' \code{\link{get_submission_json}} function which does the actual HTTP GET-request.
#' @return HTTP response from the TMC server containing the submission result JSON if the
#' server has finished processing the exercise submission. \code{NULL} if the HTTP
#' GET-reaction failed.
#' @seealso \code{\link{get_submission_json}}
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
