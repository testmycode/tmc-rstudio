library(miniUI)
library(shiny)

.log_in_tab <- miniTabPanel(
  title = "Log in",
  icon = icon("user-circle-o"),

  miniContentPanel(

    h1("Log in"),

    # inputId allows the server to access the values given by user
    textInput("username", label = "Username", value = ""),
    passwordInput("password", label = "Password", value = ""),

    actionButton(inputId = "login", label = "Log in")
  )
)

.courses_and_excersises_tab <- miniTabPanel(
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
)

.test_and_submit_tab <- miniTabPanel(
  title = "Test & Submit",
  icon = icon("check"),

  miniContentPanel(
    actionButton(inputId = "run_tests", label = "Run tests"),
    actionButton(inputId = "submit", label = "Submit to server"),
    checkboxInput(inputId = "show_all_results", label = "Show all results", value = FALSE),
    uiOutput(outputId = "test_results_display")
  )
)

.ui <- miniPage(
  gadgetTitleBar(title = "TMC RStudio", right = NULL,
                 left = miniTitleBarCancelButton(inputId = "exit", label = "Exit")),

  miniTabstripPanel(
    .log_in_tab,
    .courses_and_excersises_tab,
    .test_and_submit_tab
  )
)


