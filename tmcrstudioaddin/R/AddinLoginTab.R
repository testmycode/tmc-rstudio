.loginTabUI <- function(id, label = "Login tab") {
  # Create a namespace function using the provided id
  ns <- shiny::NS(id)
  credentials <-tmcrstudioaddin::getCredentials()
  miniTabPanel(
    title = "Log in",
    icon = icon("user-circle-o"),
    miniContentPanel(
      uiOutput(outputId=ns("loginPane"))
    )
  )

}
.loginPane <-function(ns){
  return(tagList(
    h1("Log in"),
    textInput(inputId = ns("username"), label = "Username", value = ""),
    passwordInput(inputId = ns("password"), label = "Password", value = ""),
    textInput(inputId = ns("serverAddress"),label="Server address", value=""),
    actionButton(inputId = ns("login"), label = "Log in")
  ))
}
.logoutPane <-function(ns){
  return(tagList(h1("Log out"),
                 actionButton(inputId = ns("logout"), label = "Log out")))
}

.loginTab <- function(input, output, session) {
  ns <- shiny::NS("login")
  output$loginPane<-renderUI({
    if(is.null(tmcrstudioaddin::getCredentials())){
      .loginPane(ns)
    }
    else{
      .logoutPane(ns)
    }})
  observeEvent(input$login, {
    # Authenticate with the values from the username and password input fields
    response <- tmcrstudioaddin::authenticate(input$username, input$password,input$serverAddress)
    titleAndMessage <- .getTitleAndMessage(response = response)
    # showDialog() needs RStudion version > 1.1.67
    rstudioapi::showDialog(title = titleAndMessage$title,
                           message = titleAndMessage$message,
                           url = "")
    # If user has saved credentials update view
    credentials<-tmcrstudioaddin::getCredentials()
    if(!is.null(credentials)){
      output$loginPane<-renderUI({
        .logoutPane(ns)
      })
    }
  })
  observeEvent(input$logout,{
    tmcrstudioaddin::deleteCredentials()
    output$loginPane<-renderUI({
      .loginPane(ns)
    })
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
