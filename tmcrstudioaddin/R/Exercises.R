projectsDir <- paste(sep = "", Sys.getenv("HOME"), "/tmcr-projects")

# Returns the exercise that is selected as wd.
# If wd doenst contain an exercise returns "".
exerciseFromWd <- function() {
  dirname <- dirname(getwd())
  basename <- basename(getwd())

  # Current wd is not an exercise (a folder in exercises_path)
  if(dirname != projectsDir || !(basename %in% downloadedExercises())) {
    return("")
  } else {
    return(basename)
  }
}

#Returns a list of downloaded exercises
downloadedExercises <- function() {
  return(c("", list.dirs(path = projectsDir, full.names = FALSE, recursive = FALSE)))
}

getExercisePath <- function(exercise) {
  return(paste0(projectsDir, "/", exercise))
}

sourceExercise <- function(exercise) {
  env <- new.env()
  for (file in list.files(pattern = "[.]R$", path = paste0(getExercisePath(exercise), "/R"),
                          full.names = TRUE)) {
    cat("Sourcing file: ", file, "\n\n")
    source(file, env, print.eval = TRUE)
  }
}
