# Need RStudio version > 1.1.67 for rstudioapi::showDialog()
# https://www.rstudio.com/products/rstudio/download/preview/ <- working version

tmcGadget <- function() {
  ui <- miniPage(
    gadgetTitleBar(title = "TMC RStudio", right = NULL,
                   left = miniTitleBarCancelButton(inputId = "exit", label = "Exit")),

    miniTabstripPanel(
      .loginTabUI(id = "login"),
      .courseTabUI(id = "courses"),
      .submitTabUI(id = "testAndSubmit")
    )
  )

  server <- function(input, output) {
    # Function for the exit button
    observeEvent(input$exit, {
      return(shiny::stopApp())
    })

    shiny::callModule(.loginTab, "login")
    shiny::callModule(.courseTab, "courses")
    shiny::callModule(.submitTab, "testAndSubmit")
  }

  shiny::runGadget(app = ui, server = server)
}
