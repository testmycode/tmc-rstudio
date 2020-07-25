.loginTabUI <- function(id, label = "Login tab") {
  # Create a namespace function using the provided id
  ns <- shiny::NS(id)

  miniTabPanel(
    title = "Log in",
    icon = icon("user-circle-o"),
    miniContentPanel(
      uiOutput(outputId = ns("loginPane"))
    )
  )

}
.loginPane <- function(ns, globalReactiveValues) {
  return(tagList(
    h1("Log in"),
    textInput(inputId = ns("username"), label = "Username", value = ""),
    passwordInput(inputId = ns("password"),
                  label = "Password",
                  value = ""),
    div(style = "position:relative;",
        actionButton(inputId = ns("login"),
                     label = "Log in")),
    div(style = "margin-top:30px;",
        textInput(inputId = ns("serverAddress"),
                  label = "Server address",
                  value = globalReactiveValues$credentials$serverAddress)),
    div(style = "position:relative",
        checkboxInput(inputId = ns("changeServer"),
                      label = "Change server address",
                      value = FALSE),
        actionButton(inputId = ns("resetServer"),
                     label = "Reset server address"))
  ))
}
.logoutPane <- function(ns) {
  return(tagList(h1("Log out"),
                 actionButton(inputId = ns("logout"), label = "Log out")))
}

.loginTab <- function(input, output, session, globalReactiveValues) {
  ns <- shiny::NS("login")

  observe({
    .suggest_server(globalReactiveValues)
    output$loginPane <- renderUI({
      #if token is not defined, user is not logged in
      if (is.null(globalReactiveValues$credentials$token)) {
        .loginPane(ns, globalReactiveValues)
      } else {
        .logoutPane(ns)
      }
    })
    tmcrstudioaddin::saveCredentials(globalReactiveValues$credentials)
  })


  observeEvent(input$login, {
    if (.UI_disabled) return()
    tmcrstudioaddin::disable_login_tab()

    # Authenticate with the values from the username and password input fields
    response <- tmcrstudioaddin::authenticate(input$username,
                                              input$password,
                                              input$serverAddress)
    title_and_message <- .get_title_and_message(response = response)
    # showDialog() needs RStudio version > 1.1.67
    rstudioapi::showDialog(title = title_and_message$title,
                           message = title_and_message$message,
                           url = "")
    # If user has saved credentials update view
    globalReactiveValues$credentials <- tmcrstudioaddin::getCredentials()

    if (!is.null(globalReactiveValues$credentials$token)) {
      output$loginPane <- renderUI({
        .logoutPane(ns) })
    }

    tmcrstudioaddin::enable_login_tab()
  }, ignoreInit = TRUE)

  observeEvent(input$logout, {
    if (.UI_disabled) return()
    # overwrite credentials, so that they contain only the last login address
    tryCatch({
      globalReactiveValues$credentials <-
        list(serverAddress = globalReactiveValues$credentials$serverAddress)
      tmcrstudioaddin::saveCredentials(globalReactiveValues$credentials) })
    output$loginPane <- renderUI({
      .loginPane(ns, globalReactiveValues)})
  }, ignoreInit = TRUE)

  observeEvent(input$resetServer, {
    if (.UI_disabled) return()
    updateTextInput(session, "serverAddress", value = "https://tmc.mooc.fi")
    shinyjs::disable("serverAddress")
    updateCheckboxInput(session, "changeServer", value = FALSE)
  }, ignoreInit = TRUE)

  observe({
    shinyjs::toggleState("serverAddress", input$changeServer == TRUE)
  })

}

# Sets "https://tmc.mooc.fi" as the suggested server address
.suggest_server <- function(globalReactiveValues) {
  .ddprint(".suggest_server")
  drop_warning <- function(err) {
  }
  tryCatch({
    if (is.null(globalReactiveValues$credentials$serverAddress)) {
      default_server_address <- "https://tmc.mooc.fi"
      globalReactiveValues$credentials$serverAddress <- default_server_address
    }}, warning = drop_warning)
}

# Return a title and a message string for login dialog based on
# authentication results
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
