create_exercise_metadata <- function(exercise_id,
    exercise_directory, exercise_name) {

    dir <- paste0(exercise_directory, "/", gsub("-", "/", exercise_name))
    course_directory_path <- file.path(dir, ".metadata.json",
                              fsep = .Platform$file.sep)
    newfile <- file(course_directory_path)

    export_json <- jsonlite::toJSON(list(id = exercise_id,
                    name = basename(exercise_directory)),
                    pretty = TRUE)

    cat(export_json, file = newfile, sep = "\n")
    close(newfile)
}
