#Exercise_id is the identifier of the exercise. For example, 36463

download_exercises <- function(token, exercise_id, target, exercise_directory) {
  #This can be removed later
  exercise_directory <- "tests/testthat/resources/downloaded_exercises/"

  base_url <- "https://tmc.mooc.fi/"
  exercises_url <- paste(sep = "", base_url, "api/v8/core/exercises/",
                        exercise_id, "/", "download")

  url_config <- httr::add_headers(Authorization = token)

  exercises_response <- httr::GET(exercises_url,
                              config = url_config,
                              write_disk(target, overwrite = TRUE))

  unzip(zipfile = target, exdir = exercise_directory)

  file.remove(target)

  return(exercises_response)
}
