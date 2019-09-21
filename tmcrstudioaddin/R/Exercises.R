#' @title Get the currently open exercise
#'
#' @description If the currently open exercise (the exercise which is located at the current
#' working directory) is a valid TMC R exercise project, return it as a \code{c(name = path)}
#' vector.
#'
#' @usage exercisePathFromWd()
#'
#' @details The exercise is a valid TMC R exercise project if it contains the \code{R} and
#' \code{tests/testthat} folders and is located in the TMC R projects directory.
#'
#' @return Exercise as a named vector: \code{c(name = path)}. If the current working directory
#' isn't a valid exercise project folder, returns \code{c(" ", " ")}.
#'
#' @seealso \code{\link[base]{basename}}, \code{\link[base]{getwd}}, \code{\link{get_projects_folder}}
#' \code{\link{pathIsExercise}}, \code{\link{getExerciseName}}, \code{\link[stats]{setNames}}

# Returns the exercise that is selected in wd.
# If wd doesn't contain a valid exercise returns c(" ", " ").
# Returns exercise as a named vector: c(name = path)
exercisePathFromWd <- function() {
  dirname <- dirname(getwd())
  basename <- basename(getwd())

  wdIsInProjectsDir <- grepl(paste0("^", get_projects_folder()), getwd())

  # Current wd is not an exercise (a folder in exercises_path)
  if(!pathIsExercise(getwd()) || !wdIsInProjectsDir) {
    return(c(" " = " "))
  } else {
    exerciseName <- getExerciseName(getwd())
    return(setNames(c(getwd()), c(exerciseName)))
  }
}

#' @title Get a list of downloaded exercises
#'
#' @description Get a list of downloaded exercises as named vector \code{c(name = path)}format.
#'
#' @usage downloadedExercisesPaths()
#'
#' @details Searches for downloaded exercise projects at the TMC R project folder.
#'
#' @return List of downloaded exercises as named vector \code{c(name = path)}format.
#'
#' @seealso \code{\link{findExercisesFromPath}}, \code{\link{get_projects_folder}},
#' \code{\link{getExerciseName}}, \code{\link[stats]{setNames}}

# Returns a list of downloaded exercises as named vector format.
# For example: c(name = path, name1 = path1, name2 = path2)
downloadedExercisesPaths <- function() {
  exercisePaths = findExercisesFromPath(get_projects_folder())
  names <- vector()
  for(exercisePath in exercisePaths) {
    names <- c(names, getExerciseName(exercisePath))
  }
  return(setNames(exercisePaths, names))
}

#' @title Source all exercise R files.
#'
#' @description Source all exercise \code{R} files.
#'
#' @usage sourceExercise(exercisePath, sourceEcho)
#'
#' @param exercisePath File path to the exercise project directory.
#'
#' @param sourceEcho A boolean determining whether to source echo on console.
#'
#' @details The \code{R} files contained in the exercise
#' directory's \code{R} folder are sourced with \code{print.eval} on.
#'
#' @seealso \code{\link[base]{environment}}, \code{\link[base]{list.files}},
#' \code{\link[base]{file.path}}, \code{\link[base]{cat}}, \code{\link[base]{source}}

# Sources all files in exercise with print.eval on.
sourceExercise <- function(exercisePath, sourceEcho) {
  env <- new.env()
  for (file in list.files(pattern = "[.]R$", path = file.path(exercisePath, "R"),
                          full.names = TRUE)) {
    cat("Sourcing file: ", file, "\n\n")
    source(file, env, print.eval = TRUE, echo = sourceEcho)
  }
}

#' @title Get paths to exercises
#'
#' @description Recursively searches for exercise projects in the given file path.
#'
#' @usage findExercisesFromPath(path)
#'
#' @param path Path to the directory where the exercises are searched from.
#'
#' @return A vector of full file paths to the found exercises. Always contains an
#' empty \code{""} path.
#'
#' @seealso \code{\link[base]{list.dirs}}, \code{\link{pathIsExercise}}

# Finds exercises from a path recursively. Returns the full path of found exercises
# as a vector. Contains path empty path "" always.
findExercisesFromPath <- function(path) {
  dirs <- list.dirs(path = path, full.names = TRUE, recursive = TRUE)
  foundExercises <- c("")
  for (dir in dirs) {
    if (pathIsExercise(dir)) {
      foundExercises <- c(foundExercises, dir)
    }
  }
  return(foundExercises)
}

#' @title Determine if the given file path leads to an exercise project directory
#'
#' @description Determine if the given file path leads to an exercise project
#' directory.
#'
#' @usage pathIsExercise(path)
#'
#' @param path File path to be checked.
#'
#' @details Determines if the given file path leads to an exercise project path by
#' checking if the path leads to a directory which contains the \code{R} and
#' \code{tests/testthat} folders.
#'
#' @return \code{TRUE} if the file path leads to an exercise project directory.
#' \code{FALSE otherwise}.
#'
#' @seealso \code{\link[base]{file.path}}, \code{\link[base]{file.info}}

# Determines if a path is an exercise
pathIsExercise <- function(path) {
  R_dir <- file.path(path, "R")
  testthat_dir <- file.path(path, "tests", "testthat")
  return(isTRUE(file.info(R_dir)$isdir) & isTRUE(file.info(testthat_dir)$isdir))
}

#' @title Get the exercise's name
#'
#' @description Get the name of the exercise located at the given file path.
#'
#' @usage getExerciseName(exercisePath)
#'
#' @param exercisePath File path to the exercise project directory.
#'
#' @details Reads the \code{.metadata.json} file for the exercise name.
#'
#' @return Exercise's name read from \code{.metadata.json}. If the file doesn't
#' exist or if the file doesn't have the exercise name, returns the name of the
#' path's basename (the final directory/file in the file path).
#'
#' @seealso \code{\link{get_exercise_metadata}}, \code{\link[base]{basename}}

# Read's exercises name from metadata file. If metadata file doesn't exist
# returns the name of the path's basename.
getExerciseName <- function(exercisePath) {
  metadata <- get_exercise_metadata(exercisePath)

  #No metadata: exercise name is folder name
  if (is.null(metadata) || is.null(metadata$exercise_name)) {
    return(basename(exercisePath))
  } else {
    return(metadata$exercise_name)
  }
}
