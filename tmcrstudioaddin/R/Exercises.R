#' @title Get the currently open exercise
#'
#' @description If the currently open exercise (the exercise which is
#' located at the current working directory) is a valid TMC R exercise
#' project, return it as a \code{c(name = path)} vector.
#'
#' @usage exercisePathFromWd()
#'
#' @details The exercise is a valid TMC R exercise project if it
#' contains the \code{R} and \code{tests/testthat} folders and is
#' located in the TMC R projects directory.
#'
#' @return Exercise as a named vector: \code{c(name = path)}. If the
#' current working directory isn't a valid exercise project folder,
#' returns \code{c(" " = "")}.
#'
#' @seealso \code{\link[base]{basename}}, \code{\link[base]{getwd}},
#' \code{\link{get_projects_folder}} \code{\link{pathIsExercise}},
#' \code{\link{getExerciseName}}, \code{\link[stats]{setNames}}

# Returns the exercise that is selected in wd.
# If wd doesn't contain a valid exercise returns c(" " = "").
# Returns exercise as a named vector: c(name = path)
exercisePathFromWd <- function() {
  dirname <- dirname(getwd())
  basename <- basename(getwd())

  wd_is_in_projects_dir <- grepl(paste0("^", get_projects_folder()),
                                 getwd())

  # Current wd is not an exercise (a folder in exercises_path)
  if (!pathIsExercise(getwd()) || !wd_is_in_projects_dir) {
    .dprint("This is most likely to happen. This is unnecessary.")
    return(c(" " = ""))
  } else {
    exercise_name <- getExerciseName(getwd())
    .ddprint("What is the exercise_name")
    .ddprint(str(exercise_name))
    .ddprint(str(c(exercise_name)))
    .ddprint(getwd())
    return_value <- stats::setNames(c(getwd()), c(exercise_name))
    .ddprint(return_value)
    return(return_value)
  }
}

#' @title Get a list of downloaded exercises
#'
#' @description Get a list of downloaded exercises as named vector
#' \code{c(name = path)}format.
#'
#' @usage downloadedExercisesPaths()
#'
#' @details Searches for downloaded exercise projects at the TMC R
#' project folder.
#'
#' @return List of downloaded exercises as named vector
#' \code{c(name = path)} format.
#'
#' @seealso \code{\link{findExercisesFromPath}},
#' \code{\link{get_projects_folder}}, \code{\link{getExerciseName}},
#' \code{\link[stats]{setNames}}

# Returns a list of downloaded exercises as named vector format.
# For example: c(name = path, name1 = path1, name2 = path2)
downloadedExercisesPaths <- function() {
  .dprint("downloadedExercisesPaths")
  exercisePaths <- findExercisesFromPath(get_projects_folder())
  names <- vector()
  for (exercisePath in exercisePaths) {
    new_names <- getExerciseName(exercisePath)
    .ddprint("new_names")
    names <- c(names, new_names)
  }
  .ddprint("HERE ARE THE NAMES!!!!")
  .ddprint(names)
  .ddprint(exercisePath)
  return_value <- stats::setNames(exercisePaths, names)
  .ddprint(return_value)
  return(return_value)
}

#' @title Source all exercise R files.
#'
#' @description Source all exercise \code{R} files.
#'
#' @usage sourceExercise(exercisePath, sourceEcho)
#'
#' @param exercisePath File path to the exercise project directory.
#'
#' @param sourceEcho A boolean determining whether to source echo on
#' console.
#'
#' @details The \code{R} files contained in the exercise directory's
#' \code{R} folder are sourced with \code{print.eval} on.
#'
#' @seealso \code{\link[base]{environment}},
#' \code{\link[base]{list.files}}, \code{\link[base]{file.path}},
#' \code{\link[base]{cat}}, \code{\link[base]{source}}

# Sources all files in exercise with print.eval on.
sourceExercise <- function(exercisePath, sourceEcho) {
  env <- .GlobalEnv
  for (file in list.files(pattern = "[.]R$",
                          path = file.path(exercisePath, "R"),
                          full.names = TRUE)) {
    cat("Sourcing file: ", file, "\n\n")
    .ddprint(.file_encoding(file))
    if (!is.null(.Platform$OS.type) && .Platform$OS.type == "windows" &&
        .file_encoding(file) == "UTF-8") {
      .ddprint("NOW HERE")
      source(file, env, print.eval = TRUE, echo = sourceEcho,
             encoding = "UTF-8")
    } else {
      source(file, env, print.eval = TRUE, echo = sourceEcho)
    }
  }
  if (rstudioapi::isAvailable()) {
    rstudioapi::executeCommand("refreshEnvironment")
  }
}

#' @title Get paths to exercises
#'
#' @description Recursively searches for exercise projects in the given
#' file path.
#'
#' @usage findExercisesFromPath(path)
#'
#' @param path Path to the directory where the exercises are searched from.
#'
#' @return A vector of full file paths to the found exercises. Always
#' contains an empty \code{""} path.
#'
#' @seealso \code{\link[base]{list.dirs}}, \code{\link{pathIsExercise}}

# Finds exercises from a path recursively. Returns the full path of
# found exercises as a vector. Contains path empty path "" always.
findExercisesFromPath <- function(path) {
  .ddprint("findExercisesFromPath")
  dirs <- list.dirs(path = path, full.names = TRUE, recursive = TRUE)
  foundExercises <- c("")
  for (dir in dirs) {
    if (pathIsExercise(dir)) {
      foundExercises <- c(foundExercises, dir)
    }
  }
  .ddprint(foundExercises)
  return(foundExercises)
}

#' @title Determine if the given file path leads to an exercise project
#' directory
#'
#' @description Determine if the given file path leads to an exercise project
#' directory.
#'
#' @usage pathIsExercise(path)
#'
#' @param path File path to be checked.
#'
#' @details Determines if the given file path leads to an exercise
#' project path by checking if the path leads to a directory which
#' contains the \code{R} and \code{tests/testthat} folders.
#'
#' @return \code{TRUE} if the file path leads to an exercise project
#' directory.  \code{FALSE otherwise}.
#'
#' @seealso \code{\link[base]{file.path}}, \code{\link[base]{file.info}}

# Determines if a path is an exercise
pathIsExercise <- function(path) {
  R_dir <- file.path(path, "R")
  testthat_dir <- file.path(path, "tests", "testthat")
  return(isTRUE(file.info(R_dir)$isdir) &
         isTRUE(file.info(testthat_dir)$isdir))
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
#' @return Exercise's name read from \code{.metadata.json}. If the file
#' doesn't exist or if the file doesn't have the exercise name, returns
#' the name of the path's basename (the final directory/file in the file
#' path).
#'
#' @seealso \code{\link{get_exercise_metadata}}, \code{\link[base]{basename}}

# Read's exercises name from metadata file. If metadata file doesn't
# exist returns the name of the path's basename.
getExerciseName <- function(exercisePath) {
  .ddprint("getExerciseName")
  metadata <- get_exercise_metadata(exercisePath)

  # No metadata: exercise name is folder name
  if (is.null(metadata) || is.null(metadata$exercise_name)) {
    return(basename(exercisePath))
  } else {
    .ddprint(metadata$exercise_name)
    .ddprint(metadata$name[[1]])
    .ddprint(paste(metadata$name[[1]], metadata$exercise_name, sep = ":"))
    return(paste(metadata$name[[1]], metadata$exercise_name, sep = ":"))
    # return(metadata$exercise_name)
  }
}

.clear_global_environment <- function(gl_store_name) {
  env <- .GlobalEnv
  store <- get(gl_store_name, env)
  names <- ls(env, all.names = TRUE)
  names <- names[names != gl_store_name]
  for (var in (ls(env, all.names = TRUE))) {
    .ddprint(var)
    rm(list = var, envir = env)
  }
  return(store)
}

.restore_global_environment <- function(store) {
  env <- .GlobalEnv
  new_copy <- store
  new_copy
  for (var in (ls(new_copy, all.names = TRUE))) {
    .ddprint(var)
    val <- get(var, new_copy)
    .ddprint(str(val))
    assign(x = var, value = val, envir = env)
  }
  NULL
}

.copy_global_environment <- function() {
  copy_environment <- function(env, seen_envs) {
    copy_env <- new.env()
    names <- ls(env, all.names = TRUE)
    for (var in names) {
      val <- get(var, env)
      if (!is.environment(val)) {
        assign(x = var, value = val, envir = copy_env)
      } else {
        seen_before <- any(unlist(lapply(seen_envs,
                                         FUN = function(seen) identical(val, seen))))
        if (seen_before) {
          assign(x = var, value = val, envir = copy_env)
        } else {
          sub_env_and_seens <- copy_environment(val, c(val, seen_envs))
          sub_env   <- sub_env_and_seens[[1]]
          seen_envs <- sub_env_and_seens[[2]]
          assign(x = var, value = sub_env, envir = copy_env)
        }
      }
    }
    list(copy_env, seen_envs)
  }
  copy_environment(.GlobalEnv, list(.GlobalEnv))[[1]]
}

.file_encoding <- function(fname) {
  pre_file_type <- tryCatch(system2("file", fname, stdout = TRUE, stderr = FALSE),
                            error   = function(e) "",
                            warning = function(e) "")
  pre_file_type2 <- strsplit(pre_file_type, split = ":")[[1]]
  if (length(pre_file_type2) == 0) return("")
  recognizers <- c("ISO-8859", "ASCII", "UTF-8")
  matches <- recognizers[sapply(recognizers,
                                function(pattern) {
                                  grepl(pattern, pre_file_type2[2])
                                })]
  .ddprint(pre_file_type)
  .ddprint(pre_file_type2)
  .ddprint("-----")
  .ddprint(ifelse(length(matches), matches, ""))
  .ddprint("-----")
  ifelse(length(matches), matches, "")
}
