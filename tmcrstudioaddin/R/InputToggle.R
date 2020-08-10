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

#' @title Disable Shiny input elements using UI tab list
#'
#' @description Disable \code{Shiny} input elements given by UI list
#'
#' @param tab A named input element vectors or NULL.
#'
#' @usage disable_tab_UI_list(tab)
#'
#' @details Disables the \code{Shiny} input elements.
#' This is done in order to prevent button actions while an
#' operation that might take a long itme is running.
#'
#' @seealso \code{\link{enable_UI_elements}}

disable_tab_UI_list <- function(tab) {
  disable_single_element <- function(element) {
    shinyjs::disable(element, asis = TRUE)
  }
  lapply(tab, disable_single_element)
}

#' @title Disable Shiny input elements using UI elements
#'
#' @description Disable \code{Shiny} input elements given by UI elements
#'
#' @param UI_list A named list of input element vectors or NULL.
#'
#' @usage disable_UI_elements(UI_list)
#'
#' @details Disables the \code{Shiny} input elements.
#' This is done in order to prevent button actions while an
#' operation that might take a long itme is running.
#'
#' @seealso \code{\link{enable_UI_elements}}

disable_UI_elements <- function(UI_list = NULL) {
  .dprint("Disabling...")
  .ddprint(str(UI_list))

  lapply(UI_list$UI_normal, disable_tab_UI_list)
}

#' @title Enable Shiny input elements using UI list
#'
#' @details Enables the \code{Shiny} input elements given by UI list.
#'
#' @param UI_list A named list of vectors of input element IDs or NULL.
#' @param UI_state A named logical vector showing have we selected exercise or
#' not.
#'
#' @usage enable_UI_elements(UI_list = NULL,
#'                           UI_state = c("not_logged_in" = FALSE,
#'                                        "not_selected"  = FALSE,
#'                                        "not_downloading" = FALSE))
#'
#' @details Enables the \code{Shiny} input elements.
#'
#' @seealso \code{\link{disable_UI_elements}}

enable_UI_elements <- function(UI_list  = NULL,
                               UI_state = c("not_logged_in"   = FALSE,
                                            "not_selected"    = FALSE,
                                            "not_downloading" = FALSE)) {
  enable_single_element <- function(element) {
    shinyjs::enable(element, asis = TRUE)
  }
  enable_tab_UI_list <- function(tab) {
    .ddprint(str(tab))
    lapply(tab, enable_single_element)
  }
  .dprint("Enabling...")
  .ddprint(str(UI_list))
  not_logged_in    <- UI_state["not_logged_in"]
  not_selected     <- UI_state["not_selected"]
  not_downloading  <- UI_state["not_downloading"]

  .dprint(UI_state)
  lapply(UI_list$UI_normal, enable_tab_UI_list)
  if (not_logged_in) lapply(UI_list$UI_limited, disable_tab_UI_list)
  if (not_selected)  {
    lapply(UI_list$UI_no_selected_exercise, disable_tab_UI_list)
  }
  if (not_downloading)  {
    lapply(UI_list$UI_nothing_to_download, disable_tab_UI_list)
  }
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
}
