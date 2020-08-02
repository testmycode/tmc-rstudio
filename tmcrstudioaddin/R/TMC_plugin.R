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
  cat("Starting RTMC session...\n")
  cat("Note. The console will not be available during RTMC session and you",
      "need to use the addin buttons. The environment will be restored as is",
      "after the session.",
      sep = "\n")

  .global_env_copy  <- .copy_global_environment()
  assign(x = ".global_env_copy", value = .global_env_copy, envir = .GlobalEnv)
  # clean this ASAP
  if (exists(".__tmc_debug", envir = .GlobalEnv)) {
    .tmc_debug <- get(".__tmc_debug", envir = .GlobalEnv)
  } else {
    .tmc_debug <- NULL
  }
  .global_env_copy  <- .clear_global_environment(".global_env_copy")

  # Fix this later
  if (!is.null(.tmc_debug)) {
    assign(x = ".__tmc_debug", value = .tmc_debug, envir = .GlobalEnv)
  }
  assign(x = ".global_env_copy", value = .global_env_copy, envir = .GlobalEnv)
  rstudioapi::executeCommand("refreshEnvironment")

  .ddprint("Before...")

  login_tab_data  <- .loginTabUI(id = "login")
  course_tab_data <- .courseTabUI(id = "courses")
  submit_tab_data <- .submitTabUI(id = "testAndSubmit")
  ui <- miniPage(shinyjs::useShinyjs(),
                 gadgetTitleBar(title = "TMC RStudio",
                                right = NULL,
                                left = miniTitleBarCancelButton(inputId = "exit",
                                                                label = "Exit")),
                 miniTabstripPanel(login_tab_data[["mini_tab_panel"]],
                                   course_tab_data[["mini_tab_panel"]],
                                   submit_tab_data[["mini_tab_panel"]]))
  .ddprint("After...")

  server <- function(input, output, session) {

    .ddprint("Later...")
    .ddprint(str(course_tab_data[["ns_inputIDs"]]))
    .ddprint(str(submit_tab_data[["ns_inputIDs"]]))

    globalReactiveValues <-
      reactiveValues(credentials = tmcrstudioaddin::getCredentials(),
                     downloadedExercises = downloadedExercisesPaths(),
                     exerciseMap = list(),
                     selectedExercisePath = exercisePathFromWd(),
                     UI_disabled = FALSE,
                     UI_elements = list(login_tab  = login_tab_data[["ns_inputIDs"]],
                                        course_tab = course_tab_data[["ns_inputIDs"]],
                                        submit_tab = submit_tab_data[["ns_inputIDs"]]),
                     unpublishedExercisesMap = list(),
                     downloadedExercisesMap = list(),
                     courseInfo = list())
    shiny::onStop(function() {
                    cat("RTMC session ended.\n")
                    cat("Restoring environment...\n")
                    # fix this later
                    .ddprint(exists(".global_env_copy"))
                    .ddprint(exists(".global_env_copy", envir = .GlobalEnv))
                    assign(x = ".global_env_copy", value = .global_env_copy, envir = .GlobalEnv)
                    .ddprint(exists(".global_env_copy", envir = .GlobalEnv))
                    .global_env_copy <- .clear_global_environment(".global_env_copy")
                    .restore_global_environment(.global_env_copy)
                    rstudioapi::executeCommand("refreshEnvironment")
                  })
    # Function for the exit button
    observeEvent(input$exit, {
      if (globalReactiveValues$UI_disabled) {
        .ddprint("Disabled... ")
        return()
      }

      return(shiny::stopApp())
    })

    shiny::callModule(.loginTab, "login",
                      globalReactiveValues = globalReactiveValues)
    shiny::callModule(.courseTab, "courses",
                      globalReactiveValues = globalReactiveValues)
    shiny::callModule(.submitTab, "testAndSubmit",
                      globalReactiveValues = globalReactiveValues)
  }

  shiny::runGadget(app = ui, server = server)
}
