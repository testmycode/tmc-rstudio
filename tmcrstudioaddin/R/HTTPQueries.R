create_exercise_metadata <- function(exercise_id, exercise_directory, exercise_name) {
    dir <- paste0(exercise_directory, "/", gsub("-", "/", exercise_name))
    course_directory_path <- file.path(dir, "metadata.json",
                              fsep = .Platform$file.sep)
    newfile <- file(course_directory_path)

    export_json <- jsonlite::toJSON(list(id = exercise_id,
                    name = basename(exercise_directory)),
                    pretty = TRUE)

    cat(export_json, file = newfile, sep = "\n")
    close(newfile)
}

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
<<<<<<< HEAD
                              httr::add_headers(Authorization = token),
                              config = timeout(30),
                              write_disk(zip_path, overwrite = FALSE))
=======
                              config = url_config,
                              httr::write_disk(zip_path, overwrite = TRUE))
>>>>>>> master

  .tmc_unzip(zipfile_name = zip_path, target_folder = exercise_directory)

  file.remove(zip_path)

  create_exercise_metadata(exercise_id, exercise_directory, exercise_name)


  return(exercises_response)
}

#Example course id: 242.
download_all_exercises <- function(token, course_id) {

    # The base url.  TODO Refactor this so that we can get
    # the server the user has signed on.
    #https://tmc.mooc.fi
    base_url <- getServerAddress()

    # Course url for the course we want to download.
    course_url <- paste(sep = "", base_url, "/", "api/v8/courses",
                        "/", course_id)

    course_exercises_url <- paste(sep = "/", course_url, "exercises")

    # We need the token in the header.
    url_config <- httr::add_headers(Authorization = token)

    course_exercises_response <- httr::GET(course_exercises_url,
                                config = url_config,
                                write_disk("temp.json", overwrite = TRUE))

    course_name_response <- httr::GET(course_url, config = url_config)

    # Name of the course is retrieved from the server.

    course_name <- httr::content(course_name_response)$name
    user_home <- Sys.getenv("HOME")
    r_home <- file.path(user_home, "tmcr-projects")

    # The path where we want to download the exercises.
    course_directory_path <- file.path(r_home, course_name,
                              fsep = .Platform$file.sep)

    dir.create(course_directory_path, showWarnings = FALSE, recursive = TRUE)

    json_exercises <- jsonlite::read_json("temp.json")

    for (i in 1:length(json_exercises)) {
      from_json_to_download(i, json_exercises, token, course_directory_path)
    }

    file.remove("temp.json")

}

# Helper function that takes as arguments the iteration number i that indicates
# the number of the exercise to be downloaded, json_exercises which is the list
# containing all the info of the course, token, and course_directory_path
# (where we want to store the exercises).
from_json_to_download <- function(exercise_iteration,
  json_exercises, token, course_directory_path) {

    exercise_id <- json_exercises[exercise_iteration][[1]]$id
    exercise_name <- json_exercises[exercise_iteration][[1]]$name
    exercise_dir <- paste(sep = "/", course_directory_path, exercise_name)

    download_exercise(exercise_id, zip_target = course_directory_path,
                      exercise_directory = exercise_dir, exercise_name = exercise_name)
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
upload_current_exercise <- function(token, zip_name = "temp", remove_zip = TRUE,
                                    project_path = rstudioapi::getActiveProject()) {
  json <- base::list.files(path = project_path, pattern = "metadata.json", full.names = TRUE)
  metadata <- jsonlite::fromJSON(txt = json, simplifyVector = FALSE)
  id <- metadata$id[[1]]
  credentials <- tmcrstudioaddin::getCredentials()
  address <- paste(sep = "", credentials$serverAddress, "/")

  response <- upload_exercise(token = token, exercise_id = id,
                              project_path = project_path, server_address = address,
                              zip_name = zip_name, remove_zip = remove_zip)
  return(response)
}

getAllOrganizations <- function(){
  organizations <- tryCatch({
    credentials <- tmcrstudioaddin::getCredentials()
    url <- paste(credentials$serverAddress, '/api/v8/org.json', sep = "")
    token <- credentials$token
    req <-  httr::stop_for_status(httr::GET(url = url,httr::add_headers(Authorization = token), config = timeout(30), encode = "json"))
    jsonlite::fromJSON(httr::content(req, "text"))
  }, error = function(e){
    list(slug = list())
  })
  return(organizations$slug)
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
  return(list(id=courses$id,name=courses$name))
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
