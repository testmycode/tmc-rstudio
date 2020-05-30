#' @title Create a JSON file containing exercise metadata
#'
#' @description Create a \code{JSON} file containing exercise metadata.
#'
#' @usage create_exercise_metadata(exercise_id, exercise_directory,
#' exercise_name)
#'
#' @param exercise_id ID of the exercise.
#' @param exercise_directory Path to the directory where the exercise is
#' downloaded into.
#' @param exercise_name Name of the exercise.
#'
#' @details Creates a \code{JSON} file in the exercise's directory root.
#' The \code{JSON} file contains the exercise's name and id, which is
#' used when the user wants to upload their exercise submission to the
#' TMC server.
#'
#' @return Either \code{NULL} or an integer status.
#'
#' @seealso \code{\link[base]{file.path}},
#' \code{\link[base]{connections}}, \code{\link[jsonlite]{toJSON}},
#' \code{\link[jsonlite]{unbox}}, \code{\link[base]{cat}}
create_exercise_metadata <- function(exercise_id,
                                     exercise_directory,
                                     exercise_name) {
  dir <- paste0(exercise_directory, "/", gsub("-", "/", exercise_name))
  course_directory_path <- file.path(dir,
                                     ".metadata.json",
                                     fsep = .Platform$file.sep)
  newfile <- file(course_directory_path)

  # Note: you can use unbox() so string isn't a list
  export_json <- jsonlite::toJSON(list(id = exercise_id,
                                       exercise_name = unbox(exercise_name),
                                       name = basename(exercise_directory)),
                                  pretty = TRUE)

  cat(export_json, file = newfile, sep = "\n")
  close(newfile)
}

#' @title Read exercise metadata
#'
#' @description Read exercise metadata from the \code{.metadata.json} file.
#'
#' @usage get_exercise_metadata(exercise_path)
#'
#' @param exercise_path File path to the exercise project folder.
#'
#' @return \code{R} object created from \code{.metadata.json}. \code{NULL} if
#' \code{.metadata.json} doesn't exist.
#'
#' @seealso \code{\link[base]{file.path}}, \code{\link[base]{files}},
#' \code{\link[jsonlite]{read_json}}

# Reads and returns exercise metadata, if metadata doesn't exists returns NULL
get_exercise_metadata <- function(exercise_path) {
  metadata_path <- file.path(exercise_path, ".metadata.json")
  if (file.exists(metadata_path)) {
    return(jsonlite::read_json(metadata_path))
  }
  return(NULL)
}
