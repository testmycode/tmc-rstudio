#Exercise_id is the identifier of the exercise. For example, 36463.
#Target is the place where zip-file is stored, if it's not deleted.
download_exercises <- function(token, exercise_id,
                        zip_target = getwd(),
                        zip_name = "temp.zip",
                        exercise_directory, remove_zip = TRUE) {
  base_url <- "https://tmc.mooc.fi/"

  zip_path <- paste(sep = "", zip_target, "/", zip_name)

  exercises_url <- paste(sep = "", base_url, "api/v8/core/exercises/",
                        exercise_id, "/", "download")

  url_config <- httr::add_headers(Authorization = token)

  exercises_response <- httr::GET(exercises_url,
                              config = url_config,
                              write_disk(zip_path, overwrite = TRUE))

  unzip(zipfile = zip_path, exdir = exercise_directory)

  if (remove_zip) {
    file.remove(zip_path)
  }

  return(exercises_response)
}

# Doesn't work yet! Also should add zipping.
upload_exercises <- function(token, exercise_id, file_location, server_address) {
  base_url <- server_address
  exercises_url <- paste(sep = "", base_url, "api/v8/core/exercises/",
                         exercise_id, "/", "submissions")

  url_config <- httr::add_headers(Authorization = token)

  submission_file <- httr::upload_file(file_location)

  exercises_response <- httr::POST(exercises_url,
                                  config = url_config,
                                  encode = "multipart",
                                  body = list("submission[file]"=submission_file))

  return(httr::content(exercises_response))
}
