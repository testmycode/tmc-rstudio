# Need RStudio version > 1.1.67 for rstudioapi::showDialog()
# https://www.rstudio.com/products/rstudio/download/preview/ <- working version

source("R/Authentication.R")

tmc_gadget <- function() {

  ui <- miniPage(
    gadgetTitleBar(title = "TMC RStudio", right = NULL,
                   left = miniTitleBarCancelButton(inputId = "exit", label = "Exit")),

    # Tabs on bottom
    miniTabstripPanel(

      # A single tab
      miniTabPanel(

        title = "Log in",
        icon = icon("user-circle-o"),

        # The main UI content
        miniContentPanel(

          h1("Log in"),

          # inputId allows the server to access the values given by user
          textInput("username", label = "Username", value = ""),
          passwordInput("password", label = "Password", value = ""),

          actionButton(inputId = "login", label = "Log in")
        )
      ),


      miniTabPanel(
        title = "Exercises",
        icon = icon("folder-open"),

        miniContentPanel(
          selectInput(
            inputId = "course_select",
            label = "Select course",

            choices = list(

              "Introduction to Statistics and R I" = "Course 1",
              "Introduction to Statistics and R II" = "Course 2",
              "Data mining" = "Course 3"
            ),

            selected = 1
          ),

          # *Output function shows info from server
          textOutput(outputId = "course_display")
        )
      ),

      miniTabPanel(
        title = "Test & Submit",
        icon = icon("check"),

        miniContentPanel(
          actionButton(inputId = "run_tests", label = "Run tests"),
          actionButton(inputId = "submit", label = "Submit to server"),
          uiOutput(outputId = "test_results_display")
        )
      )
    )
  )

  # input gives access to relevant data from UI
  # Content to be shown in UI is given in output
  server <- function(input, output) {
    test_results <- eventReactive(input$run_tests, {
      tmcRtestrunner::run_tests(print = TRUE)
    })

    # Defining actions for buttons etc
    # UI elements and their actions are given as arguments
    observeEvent(input$login, {
      message <- ""
      title <- ""
      response <- tmcrstudioaddin::authenticate(input$username, input$password)

      # if Bearer token is retrieved login was successful
      if (grepl("Bearer", response[1])) {
        title <- "Success!"
        message <- "Login successful!"
        # show error and error message
      } else {
        title <- response$error
        message <- response$error_description
      }

      # showDialog() needs RStudion versio > 1.1.67
      rstudioapi::showDialog(title = title, message = message, url = "")
    })

    observeEvent(input$exit, {
      shiny::stopApp()
    })

    # render*-function renders UI content and corresponds to *Output-function in UI
    # Here the "textOutput(outputId = "courseDisplay")" in UI is declared to
    # render the value from "selectInput(inputId = "courseSelect", ...)"
    output$course_display <- renderText({
      input$course_select
    })

    output$test_results_display <- renderUI({
      results <- test_results()

      test_result_output_list <- lapply(1:length(results), function(i) {
        p(results[[i]]$name, ":", results[[i]]$status)
      })

      tagList(test_result_output_list)
    })
  }

  shiny::runGadget(app = ui, server = server)
}
