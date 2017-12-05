assign(x = "UI_disabled", value = FALSE, envir = .GlobalEnv)

disable_elements <- function(...) {
  elements <- as.list(substitute(list(...)))[-1L]
  lapply(elements, function(i) {shinyjs::disable(i)})
  UI_disabled <<- TRUE
}

enable_elements <- function(...) {
  elements <- as.list(substitute(list(...)))[-1L]
  lapply(elements, function(i) {shinyjs::enable(i)})
  shinyjs::delay(ms = 1000, expr = UI_disabled <<- FALSE)
}

disable_submit_tab <- function() {
  disable_elements("selectExercise", "source", "runTests", "submit", "showAllResults")
}

enable_submit_tab <- function() {
  enable_elements("selectExercise", "source", "runTests", "submit", "showAllResults")
}

disable_course_tab <- function() {
  disable_elements("refreshOrganizations", "organizationSelect", "refreshCourses",
                   "courseSelect", "download", "all_exercises", "exercises")
}

enable_course_tab <- function() {
  enable_elements("refreshOrganizations", "organizationSelect", "refreshCourses",
                  "courseSelect", "download", "all_exercises", "exercises")
}

disable_login_tab <- function() {
  disable_elements("username", "password", "login", "changeServer", "resetServer", "logout")
}

enable_login_tab <- function() {
  enable_elements("username", "password", "login", "changeServer","resetServer",  "logout")
}


