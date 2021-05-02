# Need RStudio version > 1.1.67 for rstudioapi::showDialog()
# https://www.rstudio.com/products/rstudio/download/preview/ <- working version

#' @importFrom shiny HTML
#' @importFrom shiny actionButton
#' @importFrom shiny checkboxGroupInput
#' @importFrom shiny checkboxInput
#' @importFrom shiny column
#' @importFrom shiny div
#' @importFrom shiny fluidPage
#' @importFrom shiny fluidRow
#' @importFrom shiny h1
#' @importFrom shiny icon
#' @importFrom shiny incProgress
#' @importFrom shiny observe
#' @importFrom shiny observeEvent
#' @importFrom shiny passwordInput
#' @importFrom shiny reactiveValues
#' @importFrom shiny renderUI
#' @importFrom shiny selectInput
#' @importFrom shiny tagList
#' @importFrom shiny tags
#' @importFrom shiny textInput
#' @importFrom shiny uiOutput
#' @importFrom shiny updateCheckboxInput
#' @importFrom shiny updateSelectInput
#' @importFrom shiny updateTextInput
#' @importFrom shiny withProgress

#' @importFrom miniUI miniTabPanel
#' @importFrom miniUI miniContentPanel
#' @importFrom miniUI miniPage
#' @importFrom miniUI gadgetTitleBar
#' @importFrom miniUI miniTitleBarCancelButton
#' @importFrom miniUI miniTabstripPanel

#' @importFrom httr status_code
#' @importFrom httr timeout
#' @importFrom httr write_disk

#' @importFrom jsonlite unbox

#' @importFrom shinyjs disable
#' @importFrom shinyjs enable
#' @importFrom shinyjs hide
#' @importFrom shinyjs hidden
#' @importFrom shinyjs show

#' @importFrom callr r_bg
#' @importFrom later later

#' @importFrom utils str
#' @importFrom utils tar
#' @importFrom utils untar
#' @importFrom utils unzip
#' @importFrom utils zip

#' @exportPattern "^[[:alpha:]]+"

#' @title Run the TMC addin
#'
#' @description Run the TMC addin on the \code{RStudio} viewer pane.
#'
#' @usage tmcGadget()
#'
#' @details The TMC \code{RStudio} addin was made using
#' \code{\link[shiny]{shiny-package}}, which allows making web
#' applications and \code{RStudio} addins using \code{R}.
tmcGadget <- function() {
  .starting_messages()
  .global_env_copy <- if (rstudioapi::isAvailable()) {
    .initialising_global_env()
  } else {
    FALSE
  }
#
  rstudioapi::isAvailable(rstudioapi::executeCommand("refreshEnvironment"))
  tabs_data_list <- list(login_tab_data  =  .loginTabUI(id = "login"),
                         course_tab_data = .courseTabUI(id = "courses"),
                         submit_tab_data = .submitTabUI(id = "testAndSubmit"))
#
#
  ui <- .create_rtmc_ui(tabs_data_list)
  tmc_shiny_server <- .create_rtmc_server(tabs_data_list, .global_env_copy)
#
  shiny::onStop(.ending_rtmc_session(.global_env_copy))
#
  app <- shiny::shinyApp(ui, tmc_shiny_server)
  .run_rtmc_addin(app)
}

.in_bold <- function(string) {
  paste0(paste0('\033', "[1m"),
         string,
         paste0('\033', "[0m"))
}
.in_red <- function(string) {
  paste0(paste0('\033', "[31m"),
         string,
         paste0('\033', "[39m"))
}
.in_br_red <- function(string) {
  paste0(paste0('\033', "[31;1m"),
         string,
         paste0('\033', "[0m"))
}
.in_blue <- function(string) {
  paste0(paste0('\033', "[34m"),
         string,
         paste0('\033', "[39m"))
}
.in_green <- function(string) {
  paste0(paste0('\033', "[32m"),
         string,
         paste0('\033', "[39m"))
}
.starting_messages <- function() {
  is_blocking     <- rstudioapi::isAvailable()
  blocking_string <- if (is_blocking) "original" else "experimental nonblocking"
  cat("Starting", blocking_string, "RTMC session...\n")
  cat(.in_green("NOTE: "))
  if (is_blocking) {
    cat(paste0("The console ", .in_br_red("WILL NOT BE AVAILABLE"),
               " during RTMC session and you"),
        "need to use the addin buttons. The environment will be restored as is",
        "after the session.",
        sep = "\n")
  } else {
    cat(paste0("The console is working while RTMC is running and ",
               .in_green("IS AVAILABLE"),
               " normally"),
        "during RTMC session. For sourcing you should use the normal source, since",
        "the addin source button is obsolete and will be removed shortly.",
        "",
        "In order to end the RTMC session, use the 'Exit' button in addin.",
        sep = "\n")
  }
}

.initialising_global_env <- function() {
  .global_env_copy <- .copy_global_environment()
  # FIX .copy_global_environment
  # FIX do not copy at all for non-blocking
  # these are always going to be empty
  assign(x = ".global_env_copy", value = .global_env_copy,
         envir = .GlobalEnv)
  # clean this ASAP
  if (exists(".__tmc_debug", envir = .GlobalEnv)) {
    .tmc_debug <- get(".__tmc_debug", envir = .GlobalEnv)
  } else {
    .tmc_debug <- NULL
  }
  .global_env_copy <- .clear_global_environment(".global_env_copy")

  # Fix this later
  if (!is.null(.tmc_debug)) {
    assign(x = ".__tmc_debug", value = .tmc_debug, envir = .GlobalEnv)
  }
  assign(x = ".global_env_copy", value = .global_env_copy,
         envir = .GlobalEnv)
}

.choose_used_theme <- function(css_prefix) {
  style_setup <- sub(pattern     = " ",
                     replacement = "",
                     unlist(strsplit(Sys.getenv("TMCR_UNTESTED"),
                                     split = ",")))
  style_set  <- any(style_setup == "dark")
  if (style_set) {
    paste0(css_prefix, "/", "darktheme.css")
  } else {
    NULL
  }
}

.create_rtmc_ui <- function(tabs_data_list) {
  # cat("4\n")
  css_prefix <- "tmcrstudioaddin-0.8.3"
  used_theme <- .choose_used_theme(css_prefix)
  shiny::addResourcePath(css_prefix,
                         system.file("www", package = "tmcrstudioaddin"))
  login_tab_data  <- tabs_data_list[["login_tab_data"]]
  course_tab_data <- tabs_data_list[["course_tab_data"]]
  submit_tab_data <- tabs_data_list[["submit_tab_data"]]
  miniUI::miniPage(shinyjs::useShinyjs(),
                   theme = used_theme,
                   miniUI::gadgetTitleBar(title = "TMC RStudio",
                                          right = NULL, # Might be needed: cancel button
                                          left  = miniUI::miniTitleBarCancelButton(inputId = "exit",
                                                                                   label = "Exit")),
                   miniUI::miniTabstripPanel(login_tab_data[["mini_tab_panel"]],
                                             course_tab_data[["mini_tab_panel"]],
                                             submit_tab_data[["mini_tab_panel"]]))
}

.create_rtmc_server <- function(tabs_data_list, .global_env_copy) {
  login_tab_data  <- tabs_data_list[["login_tab_data"]]
  course_tab_data <- tabs_data_list[["course_tab_data"]]
  submit_tab_data <- tabs_data_list[["submit_tab_data"]]
  function(input, output, session) {
    # cat("3\n")
    login_tab_ui  <-  login_tab_data[["ns_inputIDs"]]
    course_tab_ui <- course_tab_data[["ns_inputIDs"]]
    submit_tab_ui <- submit_tab_data[["ns_inputIDs"]]

    UI_lim_names2 <- c("openFiles", "runTests", "submit")
    if (rstudioapi::isAvailable()) {
      UI_lim_names2 <- c(UI_lim_names2, "saveFiles", "source")
    }
    UI_limited    <- list(login_tab  = login_tab_ui["login"],
                          course_tab = course_tab_ui,
                          submit_tab = submit_tab_ui[c("refreshExercises", "submit")])
    UI_limited2   <- list(submit_tab = submit_tab_ui[UI_lim_names2])
    UI_limited3   <- list(course_tab = course_tab_ui[c("download")])
    UI_normal     <- list(all_tabs   = c("exit"),
                          login_tab  = login_tab_ui,
                          course_tab = course_tab_ui,
                          submit_tab = submit_tab_ui)
    globalReactiveValues <-
      reactiveValues(credentials = tmcrstudioaddin::getCredentials(),
                     downloadedExercises = tmcrstudioaddin::downloadedExercisesPaths(),
                     exerciseMap = list(),
                     selectedExercisePath = tmcrstudioaddin::exercisePathFromWd(),
                     UI_disabled = FALSE,
                     UI_state    = c("not_logged_in"   = FALSE,
                                     "not_selected"    = FALSE,
                                     "not_downloading" = FALSE),
                     UI_elements = list(UI_limited              = UI_limited,
                                        UI_no_selected_exercise = UI_limited2,
                                        UI_nothing_to_download  = UI_limited3,
                                        UI_normal               = UI_normal),
                     unpublishedExercisesMap = list(),
                     downloadedExercisesMap = list(),
                     coursesInfo = list())

    shiny::onStop(function() {
                    if (!rstudioapi::isAvailable()) {
                      cat("RTMC session crashed... Ending RTMC session.\n")
                      shiny::stopApp(stop("RTMC session crashed..."))
                    }
                    .ending_rtmc_session(.global_env_copy)
                  })
    # Function for the exit button
    shiny::observeEvent(input$exit, { shiny::stopApp() })
    # Function for the cancel button (which we don't have)
    # observeEvent(input$cancel, { shiny::stopApp(stop("User cancel", call. = FALSE)) })

    shiny::callModule(.loginTab,
                      "login",
                      globalReactiveValues = globalReactiveValues)
    shiny::callModule(.courseTab,
                      "courses",
                      globalReactiveValues = globalReactiveValues)
    shiny::callModule(.submitTab,
                      "testAndSubmit",
                      globalReactiveValues = globalReactiveValues)
  }
}

.run_rtmc_addin <- function(app) {
  if (!rstudioapi::isAvailable()) {
    # cat("2\n")
    shiny::runApp(app,
                  launch.browser = FALSE, #shiny::paneViewer(),
                  quiet = FALSE)
  } else {
    # cat("2\n")
    shiny::runApp(app,
                  launch.browser = shiny::paneViewer(),
                  quiet = TRUE)
  }
}

.restore_host_global_env <- function(.global_env_copy) {
  cat("Restoring environment...\n")
  assign(x     = ".global_env_copy",
         value = .global_env_copy,
         envir = .GlobalEnv)
  .global_env_copy <- .clear_global_environment(".global_env_copy")
  .restore_global_environment(.global_env_copy)
}

.ending_rtmc_session <- function(.global_env_copy) {
  function() {
    cat("RTMC session ended.\n")
    if (rstudioapi::isAvailable()) {
      .restore_host_global_env(.global_env_copy)
      rstudioapi::isAvailable(rstudioapi::executeCommand("refreshEnvironment"))
    }
  }
}

#' @title Run the nonblocking TMC addin
#'
#' @description Run the nonblocking TMC addin on the \code{RStudio} viewer pane.
#' The console is working normally during the RTMC session.
#'
#' @usage tmcGadget_nonblock()
#'
#' @return An listener environment with access to the R session running the
#' the shiny application and to the listener status
#'
#' @details The TMC \code{RStudio} addin was made using
#' \code{\link[shiny]{shiny-package}}, which allows making web
#' applications and \code{RStudio} addins using \code{R}.
#' The nonblocking version uses \code{\link[callr]{callr-package}}
#' to run the shiny server in another R session. The listener
#' is implemented using \code{later}.

tmcGadget_nonblock <- function() {
#  value_name <- NULL
  rx <- callr::r_bg(function() { tmcrstudioaddin::tmcGadget() },
                    stdout = "|", stderr = "2>&1", poll_connection = TRUE)
  listener_env <- .listener(rx, res_name = NULL, env = NULL)
  cat("Waiting for RTMC to start.\n")
  invisible(listener_env)
}

