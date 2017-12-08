create_exercise_metadata <- function(exercise_id,
    exercise_directory, exercise_name) {

    dir <- paste0(exercise_directory, "/", gsub("-", "/", exercise_name))
    course_directory_path <- file.path(dir, ".metadata.json",
                              fsep = .Platform$file.sep)
    newfile <- file(course_directory_path)

    #Note: you can use unbox() so string isn't a list
    export_json <- jsonlite::toJSON(list(id = exercise_id,
                    exercise_name = unbox(exercise_name),
                    name = basename(exercise_directory)),
                    pretty = TRUE)

    cat(export_json, file = newfile, sep = "\n")
    close(newfile)
}

# Reads and returns exercise metadata, if metadata doesn't exists returns NULL
get_exercise_metadata <- function(exercise_path) {
  metadata_path <- file.path(exercise_path, ".metadata.json")
  if (file.exists(metadata_path)) {
    return(jsonlite::read_json(metadata_path))
  }
  return(NULL)
}
