create_exercise_metadata <- function(exercise_id, exercise_directory) {
    course_directory_path <- file.path(exercise_directory, "metadata.json",
                              fsep = .Platform$file.sep)
    newfile <- file(course_directory_path)

    export_json <- toJSON(list(id = exercise_id,
                    name = basename(exercise_directory)),
                    pretty = TRUE)

    cat(export_json, file = newfile, sep = "\n")
    close(newfile)
}

#Exercise_id is the identifier of the exercise. For example, 36463.
#Target is the place where zip-file is stored, if it's not deleted.
download_exercise <- function(token, exercise_id,
                        zip_target = getwd(),
                        zip_name = "temp.zip",
                        exercise_directory) {

  base_url <- getServerAddress()

  zip_path <- paste(sep = "", zip_target, "/", zip_name)

  exercises_url <- paste(sep = "", base_url, "/", "api/v8/core/exercises/",
                        exercise_id, "/", "download")

  url_config <- httr::add_headers(Authorization = token)

  exercises_response <- httr::GET(exercises_url,
                              config = url_config,
                              write_disk(zip_path, overwrite = TRUE))

  .tmc_unzip(zipfile_name = zip_path, target_folder = exercise_directory)

  file.remove(zip_path)

  create_exercise_metadata(exercise_id, exercise_directory)


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

    download_exercise(token, exercise_id, zip_target = course_directory_path,
                      exercise_directory = exercise_dir)
  }


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

  return(httr::content(exercises_response))
}

# Zips the current working directory and uploads it to the server
# TODO:
# -Dynamic exercise_id and server_address (currently hardcoded)
upload_current_exercise <- function(token, exercise_id, server_address,
                                     zip_name = "temp", remove_zip = TRUE) {
  upload_exercise(token = token, exercise_id = exercise_id, project_path = getwd(),
                   server_address = server_address, zip_name = zip_name, remove_zip = remove_zip)
}

getAllOrganizations <- function(){
  organizations <- tryCatch({
    credentials <- tmcrstudioaddin::getCredentials()
    url <- paste(credentials$serverAddress, '/api/v8/org.json', sep = "")
    token <- credentials$token
    req <- httr::GET(url = url, config = httr::add_headers(Authorization = token), encode = "json")
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
    req <- httr::stop_for_status(httr::GET(url = url, config = httr::add_headers(Authorization = token), encode = "json"))
    jsonlite::fromJSON(httr::content(req, "text"))
  }, error = function(e){
    list(name = list())
  })
  return(courses$name)
}
