# Need RStudio version > 1.1.67 for rstudioapi::showDialog()
# https://www.rstudio.com/products/rstudio/download/preview/ <- working version

tmc_gadget <- function() {
  ui <- miniPage(
    gadgetTitleBar(title = "TMC RStudio", right = NULL,
                   left = miniTitleBarCancelButton(inputId = "exit", label = "Exit")),

    miniTabstripPanel(
      .login_tab_ui(id = "login"),
      .course_tab_ui(id = "courses"),
      .submit_tab_ui(id = "test_and_submit")
    )
  )

  server <- function(input, output) {
    # Function for the exit button
    observeEvent(input$exit, {
      return(shiny::stopApp())
    })

    shiny::callModule(.login_tab, "login")
    shiny::callModule(.course_tab, "courses")
    shiny::callModule(.submit_tab, "test_and_submit")
  }

  shiny::runGadget(app = ui, server = server)
}
