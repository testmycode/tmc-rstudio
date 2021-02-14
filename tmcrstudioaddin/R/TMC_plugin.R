# Need RStudio version > 1.1.67 for rstudioapi::showDialog()
# https://www.rstudio.com/products/rstudio/download/preview/ <- working version

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
#  print(ls(.GlobalEnv, all.names = TRUE))
  .global_env_copy <- .copy_global_environment()
#  print(.global_env_copy)
#  print(ls(.global_env_copy, all.names = TRUE))
  #
  # character(0)
  # character(0)
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
      reactiveValues(credentials = getCredentials(),
                     downloadedExercises = downloadedExercisesPaths(),
                     exerciseMap = list(),
                     selectedExercisePath = exercisePathFromWd(),
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
