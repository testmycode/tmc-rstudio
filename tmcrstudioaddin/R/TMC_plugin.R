# Need RStudio version > 1.1.67 for rstudioapi::showDialog()
# https://www.rstudio.com/products/rstudio/download/preview/ <- working version

# use ?function_name to get information from function function_name

source("R/Authentication.R")
loginGadget <- function() {

  ui <- miniPage(
    gadgetTitleBar(title = "Login to TMC", left = NULL, right = NULL),

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
            inputId = "courseSelect",
            label = "Select course",

            choices = list(

              "Introduction to Statistics and R I" = "Course 1",
              "Introduction to Statistics and R II" = "Course 2",
              "Data mining" = "Course 3"
            ),

            selected = 1
          ),

          # *Output function shows info from server
          textOutput(outputId = "courseDisplay")
        )
      )
    )
  )

  # input gives access to relevant data from UI
  # Content to be shown in UI is given in output
  server <- function(input, output) {

    # Defining actions for buttons etc
    # UI elements and their actions are given as arguments
    observeEvent(input$login, {
      message <- ""
      title <- ""
      response = authenticate(input$username,input$password)
      # if Bearer token is retrieved login was successful
      if (grepl('Bearer',response[1])) {
        title <- "Success!"
        message <- "Login successful!"
        # show error and error message
      } else {
        title <- response$error
        message <- response$error_description
      }
      # showDialog() needs RStudion versio > 1.1.67
      showDialog(title = title, message = message, url = "")
    }
    )
    # render*-function renders UI content and corresponds to *Output-function in UI
    # Here the "textOutput(outputId = "courseDisplay")" in UI is declared to
    # render the value from "selectInput(inputId = "courseSelect", ...)"
    output$courseDisplay <- renderText({
      input$courseSelect
    })
  }
  runGadget(ui, server, viewer = dialogViewer("TMC login"))
}
