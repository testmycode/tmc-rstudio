# Need RStudio version > 1.1.67 for rstudioapi::showDialog()
# https://www.rstudio.com/products/rstudio/download/preview/ <- working version

tmc_gadget <- function() {
  ui <- miniPage(
    gadgetTitleBar(title = "TMC RStudio", right = NULL,
                   left = miniTitleBarCancelButton(inputId = "exit", label = "Exit")),

    miniTabstripPanel(
      .login_tab_UI(id = "login"),
      .course_tab_UI(id = "courses"),
      .submit_tab_UI(id = "test_and_submit")
    )
  )

  server <- function(input, output) {
    # Function for the exit button
    observeEvent(input$exit, {
      return(shiny::stopApp())
    })

    callModule(.login_tab, "login")
    callModule(.course_tab, "courses")
    callModule(.submit_tab, "test_and_submit")
  }

  shiny::runGadget(app = ui, server = server)
}
