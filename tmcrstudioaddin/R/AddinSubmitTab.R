.submit_tab_ui <- function(id, label = "Submit tab") {
  ns <- shiny::NS(id)

  miniTabPanel(
    title = "Test & Submit",
    icon = icon("check"),

    miniContentPanel(
      actionButton(inputId = ns("run_tests"), label = "Run tests"),
      actionButton(inputId = ns("submit"), label = "Submit to server"),
      checkboxInput(inputId = ns("show_all_results"), label = "Show all results", value = FALSE),
      uiOutput(outputId = ns("test_results_display"))
    )
  )
}

.submit_tab <- function(input, output, session) {
  # This function is run when the Run tests -button is pressed
  run_testrunner <- eventReactive(input$run_tests, {
    tmcRtestrunner::run_tests(print = TRUE)
  })

  # Renders a list showing the test results
  output$test_results_display <- renderUI({
    # Tests are ran only when the run tests -button is pressed
    test_results <- run_testrunner()
    tests_passed_procentage <- tests_passed_procentage(test_results)

    # Reactively displays results depending on whether the
    # show all results -checkbox is checked or not
    if (input$show_all_results) {
      test_result_output <- lapply(1:length(test_results), function(i) {
        test_result <- test_results[[i]]
        .create_test_result_element(name = test_result$name, status = test_result$status,
                                    index = i, message = test_result$message)
      })
    } else {
      test_result_output <- .create_single_result_display(test_results = test_results)
    }

    html <- tags$html(tags$head(
      tags$style(HTML(paste(sep = "",
        ".progressBar { position: relative; width: 100%; background-color: red; border-radius: 0px; }
        .progress { width:", tests_passed_procentage, "; height: 30px; background-color: green; border-radius: 0px; }
        .progressText { position: absolute; text-align: center; width: 100%; top: 6px;}")))),
      tags$body(
        tags$div(class = "progressBar",
                 tags$div(class = "progressText", tests_passed_procentage),
                 tags$div(class = "progress")),
        test_result_output))

    shiny::tagList(html)
  })
}


# Creates an individual HTML paragraph element for the list displaying test results
.create_test_result_element <- function(name, status, index = NULL, message = NULL) {
  # Assign a color depending on test status
  color <- ifelse(test = grepl(x = status, pattern = "pass"), yes = "green", no = "red")
  elements <- tags$p(paste(name, ":", status),
                    style = paste("color:", color, ";font-weight:bold"))

  #if status is not pass add details
  if (status != "pass"){
    elements <- list(elements, .create_detailed_message_with_button(index, message))
  }

  return(elements)
}

.create_detailed_message_with_button <- function(index = NULL, message = NULL){
  btn <- tags$button(id = paste("button_", index, sep = ""), "Toggle details")
  message <- tags$p(style = "display:none", paste("message:", message),
                   id = paste("message_", index, sep= ""))
  script <- tags$script(paste("$(\"#button_", index,
                             "\").click(function(){$(\"#message_",
                             index, "\").toggle()});", sep= ""))
  return(list(message, btn, script))
}

# Creates an HTML paragraph element for either the first failing test or a separate message
# if all tests passed
.create_single_result_display <- function(test_results) {
  for (i in 1:length(test_results)) {
    result <- test_results[[i]]

    if (identical(x = result$status, y = "fail")) {
      return(.create_test_result_element(name = result$name, status = result$status,
                                         index = i, message = result$message))
    }
  }

  return(.create_test_result_element(name = "All tests", status = "pass"))
}
