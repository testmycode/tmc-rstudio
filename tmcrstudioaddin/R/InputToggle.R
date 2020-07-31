#' @title Disable Shiny input elements
#'
#' @description Disable \code{Shiny} input elements.
#'
#' @usage disable_elements(...)
#'
#' @param ... IDs of the \code{Shiny} input elements which are to be
#' disabled.
#'
#' @details Disables the \code{Shiny} input elements.
#' This is done in order to
#' prevent excessive buffering of button actions if the user starts
#' clicking buttons in other tabs during an operation that is taking a
#' long itme.
#'
#' @seealso \code{\link[base]{as.list}},\code{\link[base]{substitute}},
#' \code{\link[base]{lapply}}, \code{\link[shinyjs]{disable}}

disable_elements <- function(...) {
  # no access to globalReactiveValues
  # we move assign to all the callers
  elements <- as.list(substitute(list(...)))[-1L]
  .ddprint("disable_elements")
  lapply(elements, shinyjs::disable)
}

#' @title Enable Shiny input elements
#'
#' @description Enable \code{Shiny} input elements.
#'
#' @usage enable_elements(...)
#'
#' @param ... IDs of the \code{Shiny} input elements which are to be
#' enabled.
#'
#' @details Enables the \code{Shiny} input elements.
#'
#' @seealso \code{\link[base]{as.list}},\code{\link[base]{substitute}},
#' \code{\link[base]{lapply}}, \code{\link[shinyjs]{enable}}

enable_elements <- function(...) {
  # no access to globalReactiveValues
  # we move assign to all the callers
  elements <- as.list(substitute(list(...)))[-1L]
  lapply(elements, shinyjs::enable)
}

#' @title Disable Shiny input elements in the Test & Submit tab
#'
#' @description Disable \code{Shiny} input elements in the Test & Submit tab
#'
#' @usage disable_submit_tab()
#'
#' @seealso \code{\link{disable_elements}}
disable_submit_tab <- function() {
  # no access to globalReactiveValues
  disable_elements("selectExercise",
                   "refreshExercises",
                   "openFiles",
                   "saveFiles",
                   "source",
                   "runTests",
                   "submit",
                   "showAllResults",
                   "toggleEcho")
# and sets the global boolean \code{.UI_disabled} to \code{FALSE}. 
  assign(".UI_disabled", TRUE, envir = .GlobalEnv)
}

#' @title Enable Shiny input elements in the Test & Submit tab
#'
#' @description Enable \code{Shiny} input elements in the Test & Submit tab
#'
#' @usage enable_submit_tab()
#'
#' @seealso \code{\link{enable_elements}}
enable_submit_tab <- function() {
  # no access to globalReactiveValues
  enable_elements("selectExercise",
                  "refreshExercises",
                  "openFiles",
                  "saveFiles",
                  "source",
                  "runTests",
                  "submit",
                  "showAllResults",
                  "toggleEcho")
}

#' @title Disable Shiny input elements in the Course & Exercise tab
#'
#' @description Disable \code{Shiny} input elements in the Course &
#' Exercise tab.
#'
#' @usage disable_course_tab()
#'
#' @seealso \code{\link{disable_elements}}
disable_course_tab <- function() {
  # no access to globalReactiveValues
  disable_elements("refreshOrganizations",
                   "organizationSelect",
                   "refreshCourses",
                   "courseSelect",
                   "download",
                   "all_exercises",
                   "unpublished_exercises",
                   "exercises")
  assign(".UI_disabled", TRUE, envir = .GlobalEnv)
}

#' @title Enable Shiny input elements in the Course & Exercise tab
#'
#' @description Enable \code{Shiny} input elements in the Course &
#' Exercise tab.
#'
#' @usage enable_course_tab()
#'
#' @seealso \code{\link{enable_elements}}
enable_course_tab <- function() {
  # no access to globalReactiveValues
  enable_elements("refreshOrganizations",
                  "organizationSelect",
                  "refreshCourses",
                  "courseSelect",
                  "download",
                  "all_exercises",
                  "exercises")
  shinyjs::delay(ms = 1000,
                 expr = assign(".UI_disabled", FALSE, envir = .GlobalEnv))
}


#' @title Disable Shiny input elements in the Login tab.
#'
#' @description Disable \code{Shiny} input elements in the Login tab.
#'
#' @usage disable_login_tab()
#'
#' @seealso \code{\link{disable_elements}}
disable_login_tab <- function() {
  # no access to globalReactiveValues
  disable_elements("username",
                   "password",
                   "login",
                   "changeServer",
                   "resetServer",
                   "logout")
  assign(".UI_disabled", TRUE, envir = .GlobalEnv)
}

#' @title Enable Shiny input elements in the Login tab.
#'
#' @description Enable \code{Shiny} input elements in the Login tab.
#'
#' @usage enable_login_tab()
#'
#' @seealso \code{\link{enable_elements}}
enable_login_tab <- function() {
  # no access to globalReactiveValues
  enable_elements("username",
                  "password",
                  "login",
                  "changeServer",
                  "resetServer",
                  "logout")
  shinyjs::delay(ms = 1000,
                 expr = assign(".UI_disabled", FALSE, envir = .GlobalEnv))
}
