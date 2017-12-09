#' @title Disable shiny input elements
#'
#' @description Disable shiny input elements.
#'
#' @usage disable_elements(...)
#'
#' @param ... IDs of the shiny input elements which are to be disabled.
#'
#' @details Disables the shiny input elements and sets the global boolean \code{UI_disabled}
#' to \code{FALSE}. This is done in order to prevent excessive buffering of button actions if
#' the user starts clicking buttons in other tabs during an operation that is taking a long itme.
#'
#' @examples disable_elements("login", "logout", "username", "password")

disable_elements <- function(...) {
  elements <- as.list(substitute(list(...)))[-1L]
  lapply(elements, function(i) {shinyjs::disable(i)})
  UI_disabled <<- TRUE
}

#' @title Enable shiny input elements
#'
#' @description Enable shiny input elements.
#'
#' @usage enable_elements(...)
#'
#' @param ... IDs of the shiny input elements which are to be enabled.
#'
#' @details Enables the shiny input elements and sets the global boolean \code{UI_disabled} to
#' \code{FALSE} after 1000 milliseconds (=1 second) have passed. This is done in order to
#' prevent excessive buffering of button actions if the user starts clicking buttons in other
#' tabs during an operation that is taking a long itme.
#'
#' @examples enable_elements("login", "logout", "username", "password")
enable_elements <- function(...) {
  elements <- as.list(substitute(list(...)))[-1L]
  lapply(elements, function(i) {shinyjs::enable(i)})
  shinyjs::delay(ms = 1000, expr = UI_disabled <<- FALSE)
}

#' @title Disable shiny input elements in the Test & Submit tab
#'
#' @description Disable shiny input elements in the Test & Submit tab
#'
#' @usage disable_submit_tab()
#'
#' @seealso \code{\link{disable_elements}}
disable_submit_tab <- function() {
  disable_elements("selectExercise", "source", "runTests", "submit", "showAllResults")
}

#' @title Enable shiny input elements in the Test & Submit tab
#'
#' @description Enable shiny input elements in the Test & Submit tab
#'
#' @usage enable_submit_tab()
#'
#' @seealso \code{\link{enable_elements}}
enable_submit_tab <- function() {
  enable_elements("selectExercise", "source", "runTests", "submit", "showAllResults")
}

#' @title Disable shiny input elements in the Course & Exercise tab
#'
#' @description Disable shiny input elements in the Course & Exercise tab.
#'
#' @usage disable_course_tab()
#'
#' @seealso \code{\link{disable_elements}}
disable_course_tab <- function() {
  disable_elements("refreshOrganizations", "organizationSelect", "refreshCourses",
                   "courseSelect", "download", "all_exercises", "exercises")
}

#' @title Enable shiny input elements in the Course & Exercise tab
#'
#' @description Enable shiny input elements in the Course & Exercise tab.
#'
#' @usage enable_course_tab()
#'
#' @seealso \code{\link{enable_elements}}
enable_course_tab <- function() {
  enable_elements("refreshOrganizations", "organizationSelect", "refreshCourses",
                  "courseSelect", "download", "all_exercises", "exercises")
}


#' @title Disable shiny input elements in the Login tab.
#'
#' @description Disable shiny input elements in the Login tab.
#'
#' @usage disable_login_tab()
#'
#' @seealso \code{\link{disable_elements}}
disable_login_tab <- function() {
  disable_elements("username", "password", "login", "changeServer", "resetServer", "logout")
}

#' @title Enable shiny input elements in the Login tab.
#'
#' @description Enable shiny input elements in the Login tab.
#'
#' @usage enable_login_tab()
#'
#' @seealso \code{\link{enable_elements}}
enable_login_tab <- function() {
  enable_elements("username", "password", "login", "changeServer","resetServer",  "logout")
}


