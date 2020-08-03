.loginTabUI <- function(id, label = "Login tab") {
  # Create a namespace function using the provided id
  .dprint(".loginTabUI launched")
  ns <- shiny::NS(id)

  inputIDs    <- c("username",
                   "password",
                   "login",
#                  "serverAddress",     # this is special, thus not
                                        # included
                   "changeServer",
                   "resetServer",
                   "logout")
  ns_inputIDs <- sapply(inputIDs, ns)
  UI_content  <- uiOutput(outputId = ns("loginPane"))
  tab_panel   <- miniTabPanel(title = "Log in",
                              icon  = icon("user-circle-o"),
                              miniContentPanel(UI_content))
  list(ns_inputIDs    = ns_inputIDs,
       mini_tab_panel = tab_panel)
}
.loginPane <- function(ns, globalReactiveValues) {
  .dprint(".loginPane()")
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
  .dprint(".logoutPane()")
  return(tagList(h1("Log out"),
                 actionButton(inputId = ns("logout"), label = "Log out")))
}

.loginTab <- function(input, output, session, globalReactiveValues) {
  .dprint(".loginTab launched")
  grv <- globalReactiveValues
  enable_tab_UI <- function() {
    print("Enabling new way")
    .ddprint("Ready to do this login tab")
    # Ok. This is just an ad hoc way to do it and is caused by mixing
    # responsibilities. Actually we should just enable and disable ALL the
    # buttons.
    shinyjs::delay(ms = 10,
                   expr = {
                     print("Launching new way...")
                     tmcrstudioaddin::enable_UI_elements(grv$UI_elements)
                     globalReactiveValues$UI_disabled <- FALSE
                   })
  }
  disable_tab_UI <- function() {
    print("Disabling new way login tab")
    tmcrstudioaddin::disable_UI_elements(grv$UI_elements)
    globalReactiveValues$UI_disabled <- TRUE
  }
  ns <- shiny::NS("login")

  observer1 <- function() {
    .dprint("loginTab observer1 launched...")
    print(str(grv$credentials))
    .suggest_server(globalReactiveValues)
    output$loginPane <- renderUI({
      #if token is not defined, user is not logged in
      if (is.null(grv$credentials$token)) {
        .loginPane(ns, globalReactiveValues)
      } else {
        .dprint("Site A*")
        .logoutPane(ns)
      }
    })
    .dprint("saveCredentials site B*")
    tmcrstudioaddin::saveCredentials(globalReactiveValues$credentials)
  }

  observer2 <- function() {
    .dprint("input$login launched...")
    if (grv$UI_disabled) {
      .ddprint("Disabled... ")
      return()
    }

    disable_tab_UI()

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
        .dprint("Site B")
        .logoutPane(ns) })
    }

    enable_tab_UI()
  }


  print("observer1...")
  observeEvent(grv$credentials, observer1())
  print("... initialised")

  print("observer2...")
  observeEvent(input$login, observer2(), ignoreInit = TRUE)
  print("... initialised")
  print("getCredentials site 1")
  grv$credentials <- getCredentials()

  observeEvent(input$logout, {
    .dprint("input$logout launched...")
    if (grv$UI_disabled) {
      .ddprint("Disabled... ")
      return()
    }
    # overwrite credentials, so that they contain only the last login address
    tryCatch({
      globalReactiveValues$credentials <-
        list(serverAddress = globalReactiveValues$credentials$serverAddress)
      .ddprint("saveCredentials site C")
      tmcrstudioaddin::saveCredentials(globalReactiveValues$credentials) })
    output$loginPane <- renderUI({
      .loginPane(ns, globalReactiveValues)})
  }, ignoreInit = TRUE)

  observeEvent(input$resetServer, {
    .dprint("input$resetServer launched...")
    if (grv$UI_disabled) {
      .ddprint("Disabled... ")
      return()
    }
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
  .dprint(".suggest_server()")
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
    .dprint(".get_title_and_message()")
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
