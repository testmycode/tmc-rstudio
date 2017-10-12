.login_tab_ui <- function(id, label = "Login tab") {
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

.login_tab <- function(input, output, session) {
  observeEvent(input$login, {
    # Authenticate with the values from the username and password input fields
    response <- tmcrstudioaddin::authenticate(input$username, input$password)
    title_and_message <- .get_title_and_message(response = response)

    # showDialog() needs RStudion version > 1.1.67
    rstudioapi::showDialog(title = title_and_message$title,
                           message = title_and_message$message,
                           url = "")
  })
}

# Return a title and a message string for login dialog based on authentication results
.get_title_and_message <- function(response) {
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
