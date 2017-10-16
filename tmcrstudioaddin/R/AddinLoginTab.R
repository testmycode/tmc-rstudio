.loginTabUI <- function(id, label = "Login tab") {
  # Create a namespace function using the provided id
  ns <- shiny::NS(id)

  miniTabPanel(
    title = "Log in",
    icon = icon("user-circle-o"),

    miniContentPanel(
      h1("Log in"),
      textInput(inputId = ns("username"), label = "Username", value = ""),
      passwordInput(inputId = ns("password"), label = "Password", value = ""),
      actionButton(inputId = ns("login"), label = "Log in")
    )
  )
}

.loginTab <- function(input, output, session) {
  observeEvent(input$login, {
    # Authenticate with the values from the username and password input fields
    response <- tmcrstudioaddin::authenticate(input$username, input$password)
    titleAndMessage <- .getTitleAndMessage(response = response)

    # showDialog() needs RStudion version > 1.1.67
    rstudioapi::showDialog(title = titleAndMessage$title,
                           message = titleAndMessage$message,
                           url = "")
  })
}

# Return a title and a message string for login dialog based on authentication results
.getTitleAndMessage <- function(response) {
  # if Bearer token is retrieved login was successful
  if (grepl("Bearer", response[1])) {
    title <- "Success!"
    message <- "Login successful!"
  } else {
    title <- response$error
    message <- response$error_description
  }

  return(list("title" = title, "message" = message))
}
