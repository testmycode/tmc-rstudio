# Need RStudio version > 1.1.67 for rstudioapi::showDialog()
# https://www.rstudio.com/products/rstudio/download/preview/ <- working version

#' @title Run the TMC addin
#'
#' @description Run the TMC addin on the \code{RStudio} viewer pane.
#'
#' @usage tmcGadget()
#'
#' @details The TMC \code{RStudio} addin was made using \code{\link[shiny]{shiny-package}}, which
#' allows making web applications and \code{RStudio} addins using \code{R}.
tmcGadget <- function() {
  # Assign the UI_disabled variable as a global variable
  assign(x = "UI_disabled", value = FALSE, envir = .GlobalEnv)

  ui <- miniPage(
    shinyjs::useShinyjs(),

    gadgetTitleBar(title = "TMC RStudio", right = NULL,
                   left = miniTitleBarCancelButton(inputId = "exit", label = "Exit")),

    miniTabstripPanel(
      .loginTabUI(id = "login"),
      .courseTabUI(id = "courses"),
      .submitTabUI(id = "testAndSubmit")
    )
  )

  server <- function(input, output, session) {

    globalReactiveValues <- reactiveValues(downloadedExercises = downloadedExercisesPaths())

    # Function for the exit button
    observeEvent(input$exit, {
      if(UI_disabled) return()

      return(shiny::stopApp())
    })

    shiny::callModule(.loginTab, "login")
    shiny::callModule(.courseTab, "courses", globalReactiveValues = globalReactiveValues)
    shiny::callModule(.submitTab, "testAndSubmit", globalReactiveValues = globalReactiveValues)
  }

  shiny::runGadget(app = ui, server = server)
}
