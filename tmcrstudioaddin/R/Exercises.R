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

# Sources all files in exercise with print.eval on.
sourceExercise <- function(exercisePath) {
  env <- new.env()
  for (file in list.files(pattern = "[.]R$", path = file.path(exercisePath, "R"),
                          full.names = TRUE)) {
    cat("Sourcing file: ", file, "\n\n")
    source(file, env, print.eval = TRUE)
  }
}

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

# Determines if a path is an exercise
pathIsExercise <- function(path) {
  R_dir <- file.path(path, "R")
  testthat_dir <- file.path(path, "tests", "testthat")
  return(isTRUE(file.info(R_dir)$isdir) & isTRUE(file.info(testthat_dir)$isdir))
}

# Read's exercises name from metadata file. If metadata file doesn't exist
# returns the name of the path's basename.
getExerciseName <- function(exercisePath) {
  metadata <- get_exercise_metadata(exercisePath)

  #No metadata: exercise name is folder name
  if (is.null(metadata)) {
    return(basename(exercisePath))
  } else {
    return(metadata$exercise_name)
  }
}
