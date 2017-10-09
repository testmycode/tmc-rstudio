library(shiny)
library(rstudioapi)

source("R/Authentication.R")

.server <- function(input, output) {
  # This function is run when the Run tests -button is pressed
  run_testrunner <- eventReactive(input$run_tests, {
    return(tmcRtestrunner::run_tests(print = TRUE))
  })

  # Function for the login button
  observeEvent(input$login, {
    # Authenticate with the values from the username and password input fields
    response <- tmcrstudioaddin::authenticate(input$username, input$password)
    title_and_message <- .get_title_and_message(response = response)

    # showDialog() needs RStudion version > 1.1.67
    return(rstudioapi::showDialog(title = title_and_message$title,
                           message = title_and_message$message,
                           url = ""))
  })

  # Function for the exit button
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
    # Tests are ran only when the run tests -button is pressed
    test_results <- run_testrunner()
    tests_passed_procentage = tests_passed_procentage(test_results)

    # Reactively displays results depending on whether the
    # show all results -checkbox is checked or not
    if (input$show_all_results) {
      test_result_output <- lapply(1:length(test_results), function(i) {
        test_result <- test_results[[i]]
        .create_test_result_element(name = test_result$name, status = test_result$status)
      })
    } else {
      test_result_output <- .create_single_result_display(test_results = test_results)
    }

    html <- tags$html(tags$head(
      tags$style(HTML(paste(sep = "", ".progressBar { position: relative; width: 100%; background-color: red; border-radius: 0px; }
        .progress { width:", tests_passed_procentage, "; height: 30px; background-color: green; border-radius: 0px; }
        .progressText { position: absolute; text-align: center; width: 100%; top: 6px;}")))),
      tags$body(
        tags$div(class = "progressBar",
          tags$div(class = "progressText", tests_passed_procentage),
          tags$div(class = "progress")),
        test_result_output))

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
.create_test_result_element <- function(name, status) {
  # Assign a color depending on test status
  color <- ifelse(test = grepl(x = status, pattern = "pass"), yes = "green", no = "red")

  return(tags$p(paste(name, ":", status),
         style = paste("color:", color, ";font-weight:bold")))
}

# Creates an HTML paragraph element for either the first failing test or a separate message
# if all tests passed
.create_single_result_display <- function(test_results) {
  for (i in 1:length(test_results)) {
    result <- test_results[[i]]

    if (identical(x = result$status, y = "fail")) {
      return(.create_test_result_element(name = result$name, status = result$status))
    }
  }

  return(.create_test_result_element(name = "All tests", status = "passed!"))
}
