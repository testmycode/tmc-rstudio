library(shiny)
library(rstudioapi)

source("R/Authentication.R")

.server <- function(input, output) {
  run_testrunner <- eventReactive(input$run_tests, {
    return(tmcRtestrunner::run_tests(print = TRUE))
  })

  # Function for login button
  observeEvent(input$login, {
    # Authenticate with the values from the username and password input fields
    response <- tmcrstudioaddin::authenticate(input$username, input$password)
    title_and_message <- .get_title_and_message(response = response)

    # showDialog() needs RStudion version > 1.1.67
    return(rstudioapi::showDialog(title = title_and_message$title,
                           message = title_and_message$message,
                           url = ""))
  })

  # Function for exit button
  observeEvent(input$exit, {
    return(shiny::stopApp())
  })

  # render*-function renders UI content and corresponds to *Output-function in UI
  # Here the "textOutput(outputId = "course_display")" in UI is declared to
  # render the value from "selectInput(inputId = "course_select", ...)"
  output$course_display <- renderText({
    return(input$course_select)
  })

  # Renders a list showing the test results
  output$test_results_display <- renderUI({
    test_results <- run_testrunner()

    test_result_output_list <- lapply(1:length(test_results), function(i) {
      .create_test_result_element(i = i, test_results = test_results)
    })

    #TODO: show actual procentage in progress bar
    html <- tags$html(tags$head(
      tags$style(HTML(".progressBar { position: relative; width: 100%; top: 10px; background-color: red; }
        .progress { width: 50%; height: 30px; background-color: green; }
        .progressText { position: absolute; text-align: center; width: 100%; top: 6px;}"))),
      tags$body(
        tags$div(class = "progressBar",
          tags$div(class = "progressText", "50%"),
          tags$div(class = "progress")),
        test_result_output_list))

    return(shiny::tagList(html))
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

# Creates an individual HTML paragraph element for the list displaying test results
.create_test_result_element <- function(test_results, i) {
  test_name <- test_results[[i]]$name
  test_status <- test_results[[i]]$status

  # Assign a color depending on test status
  color <- ifelse(test = identical(x = test_status, y = "pass"), yes = "green", no = "red")

  return(tags$p(paste(test_name, ":", test_status),
         style = paste("color:", color, ";font-weight:bold")))
}
