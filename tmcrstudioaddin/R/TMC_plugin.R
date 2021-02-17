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
  .global_env_copy <- .initialising_global_env()

  rstudioapi::isAvailable(rstudioapi::executeCommand("refreshEnvironment"))
  login_tab_data  <- .loginTabUI(id = "login")
  course_tab_data <- .courseTabUI(id = "courses")
  submit_tab_data <- .submitTabUI(id = "testAndSubmit")
#
  style_setup <- sub(pattern     = " ",
                     replacement = "",
                     unlist(strsplit(Sys.getenv("TMCR_UNTESTED"),
                                     split = ",")))
  style_set  <- any(style_setup == "dark")
  css_prefix <- "tmcrstudioaddin-0.6.3"
  used_theme <- if (style_set) {
    paste0(css_prefix, "/", "darktheme.css")
  } else {
    NULL
  }

  shiny::addResourcePath(css_prefix, system.file("www",
                                                 package = "tmcrstudioaddin"))
#

  ui <- miniUI::miniPage(shinyjs::useShinyjs(), theme = used_theme,
                         miniUI::gadgetTitleBar(title = "TMC RStudio",
                                                # right = miniUI::miniTitleBarCancelButton(inputId = "cancel",
                                                #                                  label = "Cancel"),
                                                right = NULL,
                                                left  = miniUI::miniTitleBarCancelButton(inputId = "exit",
                                                                                         label = "Exit")),
                         miniUI::miniTabstripPanel(login_tab_data[["mini_tab_panel"]],
                                                   course_tab_data[["mini_tab_panel"]],
                                                   submit_tab_data[["mini_tab_panel"]]))
  # print("After...")
  tmc_shiny_server <- function(input, output, session) {
    # print("After...")
    # print("Later...")
    login_tab_ui  <- login_tab_data[["ns_inputIDs"]]
    course_tab_ui <- course_tab_data[["ns_inputIDs"]]
    submit_tab_ui <- submit_tab_data[["ns_inputIDs"]]
    UI_limited    <- list(login_tab  = login_tab_ui["login"],
                          course_tab = course_tab_ui,
                          submit_tab = submit_tab_ui[c("refreshExercises", "submit")])
    UI_limited2   <- list(submit_tab = submit_tab_ui[c("openFiles",
                                                       "saveFiles",
                                                       "source",
                                                       "runTests",
                                                       "submit")])
    UI_limited3   <- list(course_tab = course_tab_ui[c("download")])
    UI_normal     <- list(all_tabs   = c("exit"),
                          login_tab  = login_tab_ui,
                          course_tab = course_tab_ui,
                          submit_tab = submit_tab_ui)
    # print(str(course_tab_data[["ns_inputIDs"]]))
    # print(str(submit_tab_ui))
    # print(str(submit_tab_ui[c("refreshExercises", "submit")]))
    # print("Initial launch of observer1 with getCredentials...")
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
                    cat("RTMC session ended.\n")
                    cat("Restoring environment...\n")
                    # fix this later
                    # print(exists(".global_env_copy"))
                    # print(exists(".global_env_copy", envir = .GlobalEnv))
                    assign(x = ".global_env_copy", value = .global_env_copy,
                           envir = .GlobalEnv)
                    # print(exists(".global_env_copy", envir = .GlobalEnv))
                    .global_env_copy <- .clear_global_environment(".global_env_copy")
                    .restore_global_environment(.global_env_copy)
                    rstudioapi::executeCommand("refreshEnvironment")
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
  # print("Before...")
  shiny::onStop(function() {
                  if (!rstudioapi::isAvailable()) {
                      cat("RTMC session ended.\n")
                      cat("Not really restoring environment...\n")
                      assign(x = ".global_env_copy", value = .global_env_copy,
                             envir = .GlobalEnv)
                      # print(exists(".global_env_copy", envir = .GlobalEnv))
                      # print(ls(.global_env_copy, all.names = TRUE))
                      # print(ls(.GlobalEnv, all.names = TRUE))
                      # [1] TRUE
                      # character(0)
                      # [1] ".global_env_copy" ".Random.seed"
                      .global_env_copy <- .clear_global_environment(".global_env_copy")
                      # print(ls(.global_env_copy, all.names = TRUE))
                      # print(ls(.GlobalEnv, all.names = TRUE))
                      # character(0)
                      # character(0)
                      .restore_global_environment(.global_env_copy)
                      # print(ls(.global_env_copy, all.names = TRUE))
                      # print(ls(.GlobalEnv, all.names = TRUE))
                      # character(0)
                      # character(0)
                      # rstudioapi::executeCommand("refreshEnvironment")
                  }
  })
  # print("Before...")
  app <- shiny::shinyApp(ui, tmc_shiny_server)
  if (!rstudioapi::isAvailable()) {
    shiny::runApp(app,
                  launch.browser = shiny::paneViewer())
  } else {
    shiny::runApp(app,
                  launch.browser = shiny::paneViewer(),
                  quiet = TRUE)
  }

}

.starting_messages <- function() {
  is_blocking     <- rstudioapi::isAvailable()
  blocking_string <- if (is_blocking) "original" else "experimental nonblocking"
  cat("Starting", blocking_string, "RTMC session...\n")
  cat(paste0('\033', "[", "3", "2", "m"))
  cat("NOTE: ")
  cat(paste0('\033', "[", "3", "9", "m"))
  if (is_blocking) {
    cat("The console WILL NOT BE available during RTMC session and you",
        "need to use the addin buttons. The environment will be restored as is",
        "after the session.",
        sep = "\n")
  } else {
    cat("The console is relased after the addin has started and IS AVAILABLE normally",
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
  # print(ls(.GlobalEnv, all.names = TRUE))
  # clean this ASAP
  if (exists(".__tmc_debug", envir = .GlobalEnv)) {
    .tmc_debug <- get(".__tmc_debug", envir = .GlobalEnv)
  } else {
    .tmc_debug <- NULL
  }
  .global_env_copy <- .clear_global_environment(".global_env_copy")
  # print(.global_env_copy)
  # print(ls(.global_env_copy, all.names = TRUE))
  # print(ls(.GlobalEnv, all.names = TRUE))
  # character(0)
  # character(0)
  # these are always going to be empty

  # Fix this later
  if (!is.null(.tmc_debug)) {
    assign(x = ".__tmc_debug", value = .tmc_debug, envir = .GlobalEnv)
  }
  assign(x = ".global_env_copy", value = .global_env_copy,
         envir = .GlobalEnv)
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
  value_name <- "inspection"
  env1 <- parent.frame()
  rx <- callr::r_bg(function() { tmcrstudioaddin::tmcGadget() },
                    stdout = "|", stderr = "2>&1", poll_connection = TRUE)
  listener_env <- .listener(rx, res_name = value_name, env = NULL)
  launch <-
    function (port) {
      server_port <- paste0("http://127.0.0.1:", port)
      cat("Server for 'shiny' has started.\n")
      cat("Server port = ", server_port, "\n")
      # cat("count = ", count1, "\n")
      cat("Opening viewer.\n")
      rstudioapi::viewer(server_port)
    }
  # cat(rx$is_alive(), "\n")
  polls <- rx$poll_io(timeout = 10)
  count1 <- 0
  count  <- 0
  server_port <- ""
  cat("Waiting for 'shiny' to start.\n")
  while (server_port == "" & rx$is_alive()) {
    while (polls["output"] != "ready") {
      cat(".")
      # cat(polls["output"])
      Sys.sleep(0.1)
      # cat("Woke up.\n")
      polls <- rx$poll_io(timeout = 10)
      count <- count + 1
    }
    err_text <- rx$read_output()
    if (count > 0) cat("\n")
    start_idx   <- regexpr("http://", err_text)
    if (start_idx >= 0) {
      server_port <-  sub(pattern = "\n", replacement = "",
                          substr(err_text, start = start_idx, stop = nchar(err_text)))
    } else {
      cat(err_text)
    }
    count1 <- count1 + count
    count <- 0
    polls <- rx$poll_io(timeout = 10)
  }
  if (rx$is_alive()) {
    cat("Server for 'shiny' has started.\n")
    cat("Server port = ", server_port, "\n")
    cat("count = ", count1, "\n")
    cat("Opening viewer.\n")
    rstudioapi::viewer(server_port)
#  cat("Waiting 4 seconds to show that before releasing console printing works\n")
#  Sys.sleep(4)
    cat("Releasing console. Have fun!\n")
    listener_env
  } else {
    cat("Server 'shiny' failed to start.\n")
    cat("Just retry, it is normal that it just sometimes fails.\n")
    listener_env
  }



}

