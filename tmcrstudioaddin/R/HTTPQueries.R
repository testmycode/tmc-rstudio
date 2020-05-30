#' @title Download an exercise from the TMC server
#'
#' @description Download an exercise from the TMC server.
#'
#' @usage download_exercise(exercise_id,
#'                          zip_target = getwd(),
#'                          zip_name = "temp.zip",
#'                          exercise_directory,
#'                          exercise_name)
#'
#' @param exercise_id ID of the exercise.
#' @param zip_target Path to where the temporary \code{zip} file is
#' stored. Defaults to the current working directory.
#' @param zip_name Name of the temporary \code{zip} file. Default is
#' \code{temp.zip}.
#' @param exercise_directory Path to the directory where the exercise
#' directory is unzipped to.
#' @param exercise_name Name of the downloaded exercise.
#'
#' @details Reads the user credentials from the credentials file,
#' downloads an zipped exercise matching the given id from the TMC
#' server, unzips it to the given directory and creates a \code{JSON}
#' file containing the exercise name and id on the exercise directory.
#'
#' @return \code{HTTP} response to the download attempt.
#'
#' @seealso \code{\link{getCredentials}}, \code{\link[httr]{GET}},
#' \code{\link[httr]{add_headers}}, \code{\link[httr]{write_disk}},
#' \code{\link[base]{file.remove}}, \code{\link{create_exercise_metadata}}

# Exercise_id is the identifier of the exercise. For example, 36463.
# Target is the place where zip-file is stored, if it's not deleted.
download_exercise <- function(exercise_id,
                        zip_target = getwd(),
                        zip_name = "temp.zip",
                        exercise_directory,
                        exercise_name) {
  credentials <- tmcrstudioaddin::getCredentials()
  token <- credentials$token
  serverAddress <- credentials$serverAddress
  .dprint("download_exercise()")

  zip_path <- paste(sep = "", zip_target, "/", zip_name)

  exercises_url <- paste(sep = "", serverAddress, "/", "api/v8/core/exercises/",
                        exercise_id, "/", "download")

  .ddprint(zip_path)


  exercises_response <- httr::GET(exercises_url,
                               httr::add_headers(Authorization = token),
                              config = timeout(30),
                              write_disk(zip_path, overwrite = FALSE))

  .dprint("exercises_response")

  # move this to better location
  exercise_forbidden_num <- 403
  # exercise_ok_num value would be 200
  # move this to better location
  if (exercises_response$status_code == exercise_forbidden_num) {
    file.remove(zip_path)
    stop(paste("Forbidden (HTTP ", as.character(exercise_forbidden_num),
               ")",
               sep = ""))
  }
  .tmc_unzip(zipfile_name = zip_path, target_folder = exercise_directory)
  file.remove(zip_path)

  create_exercise_metadata(exercise_id, exercise_directory, exercise_name)

  return(exercises_response)
}

#' @title Upload an exercise to the TMC server
#'
#' @description Upload an exercise to the TMC server
#'
#' @usage upload_exercise(token, exercise_id, project_path, server_address,
#'         zip_name = "temp", remove_zip = TRUE)
#'
#' @param token \code{OAuth2} token associated with the current login
#' session.
#' @param exercise_id ID of the submitted exercise.
#' @param project_path File path to the directory of the submitted
#' exercise.
#' @param server_address Address of the TMC server where the exercise is
#' submitted to.
#' @param zip_name Name of the \code{zip} file which contains the
#' exercise submission. Default is \code{temp}
#' @param remove_zip \code{TRUE} or \code{FALSE} depending on if you
#' wish to delete the submission \code{zip} file after sending it to the
#' server. Defaults to \code{TRUE}.
#'
#' @details Packs the exercise directory into a \code{zip} file and
#' sends it to the TMC server as a list.
#'
#' @return \code{HTTP} response to the submission attempt.
#'
#' @seealso \code{\link[httr]{add_headers}},
#' \code{\link[httr]{upload_file}}, \code{\link[httr]{POST}},
#' \code{\link[base]{file.remove}}

# Zips and uploads a single exercise, which is located in project_path.
# Returns the response, which contains a field $submission_url
# containing the details of the submission.
upload_exercise <- function(token, exercise_id, project_path,
                             server_address, zip_name = "temp",
                             remove_zip = TRUE) {
  exercises_response <- list()
  base_url <- server_address
  exercises_url <- paste(sep = "", base_url, "api/v8/core/exercises/",
                         exercise_id, "/", "submissions")
  url_config <- httr::add_headers(Authorization = token)

  .dprint("upload_exercise()")
  zip_path <- paste0(tempfile(), ".zip")
  .dprint(zip_path)
  tryCatch({
  .tmc_zip(project_path, zip_path)
  .dprint(paste("Project path", project_path))
  .dprint(paste0("Sending zip to server ", zip_path))
  .dprint(paste0("file.exists(zip_path) ", file.exists(zip_path)))
  submission_file <- httr::upload_file(zip_path)},
  error = function(e) {
    cat("Uploading failed.\n")
    stop(e)
  })

  .dprint("exercises_response")
  exercises_response <- tryCatch({
    exercises_response$data <-
      httr::stop_for_status(
        httr::POST(exercises_url,
                   config = url_config,
                   encode = "multipart",
                   body = list("submission[file]" = submission_file)))
    .ddprint(str(exercises_response))
    if (!is.null(exercises_response$error)) {
      stop(exercises_response$error)
    }
    exercises_response
  }, error = function(e) {
    .dprint(e)
    exercises_response$error <- e
    exercises_response
  })
  .dprint("exercises_response2")
  if (remove_zip) {
    file.remove(zip_path)
  }

  return(exercises_response)
}

#' @title Get exercise submission result JSON
#'
#' @description Get the exercise submission result \code{JSON} file from
#' the TMC server.
#'
#' @usage get_submission_json(token, url)
#'
#' @param token \code{OAuth2} token associated with the the current
#' login session to the TMC server.
#' @param url URL where the exercise submission \code{JSON} is fetched from.
#'
#' @details Makes a \code{HTTP-GET} request to fetch the exercise
#' submission result \code{JSON} from the specified URL.
#'
#' @return \code{HTTP} response from the TMC server which contains the
#' submission result \code{JSON} if the server has finished processing
#' the exercise submission.
#'
#' @seealso \code{\link[httr]{add_headers}}, \code{\link[httr]{GET}}

# Returns details of the submission in url
get_submission_json <- function(token, url) {
  url_config <- httr::add_headers(Authorization = token)

  exercises_response <- httr::GET(url, config = url_config)

  return(exercises_response)
}

#' @title Upload the currently open exercise to the TMC server
#'
#' @description Upload the currently open exercise to the TMC server.
#'
#' @usage upload_current_exercise(token, project_path, zip_name =
#' "temp", remove_zip = TRUE)
#'
#' @param token \code{OAuth2} token associated with the current login
#' session.
#' @param project_path Path to the directory of the submitted exercise.
#' @param zip_name Name of the \code{zip} file which contains the
#' exercise submission. Default is \code{temp}.
#' @param remove_zip \code{TRUE} or \code{FALSE} depending on if you
#' wish to delete the submission \code{zip} file after sending it to the
#' server. Defaults to \code{TRUE}.
#'
#' @details Reads the exercise id from \code{.metadata.json} and the
#' server address from \code{.credentials.json} which are used to form
#' the correct uploading address.
#'
#' @return \code{HTTP} response as a list to the submission attempt.
#' List containing \code{error} key with an error message if reading the
#' metadata or credentials file caused an error.
#'
#' @seealso \code{\link[base]{list.files}},
#' \code{\link[jsonlite]{fromJSON}}, \code{\link{getCredentials}},
#' \code{\link{upload_exercise}}

# Zips the current working directory and uploads it to the server Uses
# the path of the currently active R-project by default For testing
# purposes, you can provide some other file path
upload_current_exercise <- function(token,
                                    project_path,
                                    zip_name = "temp",
                                    remove_zip = TRUE) {
  response <- list()
  .dprint("upload_current_exercise()")
  metadata <- tryCatch({
    json <- base::list.files(path = project_path,
                             pattern = ".metadata.json",
                             all.files = TRUE,
                             full.names = TRUE)
    jsonlite::fromJSON(txt = json, simplifyVector = FALSE)
  }, error = function(e) {
    NULL
  })
  response <-
    if (!is.null(metadata$id[[1]])) {
      id <- metadata$id[[1]]
      credentials <- tmcrstudioaddin::getCredentials()
      address <- paste(sep = "", credentials$serverAddress, "/")
      tryCatch({
      response <- upload_exercise(token = token, exercise_id = id,
                                  project_path = project_path,
                                  server_address = address,
                                  zip_name = zip_name,
                                  remove_zip = remove_zip)
      response
      }, error = function(e) {
        cat("Uploading exercise failed.\n")
        response$error <- e
        response
      })
    } else {
      response$error <- "Could not read json"
      response
    }
  return(response)
}

#' @title Get all TMC organizations
#'
#' @description Get all TMC organizations.
#'
#' @usage getAllOrganizations()
#'
#' @details Reads the \code{OAuth2} token and server address from \code{.credentials.json} and uses
#' them to make a \code{HTTP-GET} request for the list of organizations.
#'
#' @return List of TMC organization names and slugs. If reading \code{.credentials.json}
#'  or sending the \code{HTTP-GET} request failed, returns a list with 2 empty sublists called \code{name}
#'  and \code{slug}.
#'
#' @seealso \code{\link{getCredentials}}, \code{\link[httr]{stop_for_status}}, \code{\link[httr]{add_headers}},
#' \code{\link[jsonlite]{fromJSON}}
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
#'
#' @description Get all courses offered by a TMC organization.
#'
#' @usage getAllCourses(organization)
#'
#' @param organization Organization slug (Identifying URL-friendly name).
#'
#' @details Reads the \code{OAuth2} token and server address from \code{.credentials.json}
#' and uses them to make a \code{HTTP-GET} request for the list of courses belonging to
#' the organization.
#'
#' @return List of course names and course ids belonging to the given organization.
#' If reading \code{.credentials.json} or sending the \code{HTTP-GET} request failed, returns
#' a list with 2 empty sublists called \code{id} and \code{name}.
#'
#' @seealso \code{\link{getCredentials}}, \code{\link[httr]{stop_for_status}},
#' \code{\link[jsonlite]{fromJSON}}
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

#' @title Get all exercises of a TMC course
#'
#' @description Get all exercises of a TMC course.
#'
#' @usage getAllExercises(course)
#'
#' @param course ID of the course.
#'
#' @details Reads the \code{OAuth2} token and server address from \code{.credentials.json} and uses
#' them to make a \code{HTTP-GET} request for the list of exercises in the course.
#'
#' @return List of exercises in the course. If reading \code{.credentials.json} or sending
#' the \code{HTTP-GET} request failed, returns an empty list.
#'
#' @seealso \code{\link{getCredentials}}, \code{\link[httr]{stop_for_status}},
#' \code{\link[jsonlite]{fromJSON}}
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
#'
#' @description Get exercise submission result \code{JSON} from the TMC server.
#'
#' @usage get_json_from_submission_url(response, token)
#'
#' @param response \code{HTTP} response to the exercise submission.
#' @param token \code{OAuth2} token associated with the current login session to the TMC server.
#'
#' @details Extracts the exercise submission result url from the given response and makes an
#' \code{HTTP-GET} request for the exercise submission result \code{JSON}.
#'
#' @return \code{HTTP} response as a list from the TMC server containing the submission result \code{JSON}
#' if the server has finished processing the exercise submission. List containing \code{error} key
#' with an error message if the \code{HTTP-GET} request failed.
#'
#' @seealso \code{\link[httr]{content}}, \code{\link{get_submission_json}}
get_json_from_submission_url <- function(response, token) {
  submitJson <- list()
  url <- list()
  url$error <- NULL
  submitJson <- tryCatch({
    url <- httr::content(response)
    submitJson$results <- httr::content(get_submission_json(token, url$submission_url))
    submitJson
  }, error = function(e) {
    if(!is.null(url$error)) {
      submitJson$error <- url$error
    }
    submitJson$error <- e
    submitJson
  })
  return(submitJson)
}
