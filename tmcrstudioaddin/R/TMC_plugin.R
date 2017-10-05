# Need RStudio version > 1.1.67 for rstudioapi::showDialog()
# https://www.rstudio.com/products/rstudio/download/preview/ <- working version

source("R/Authentication.R")
source("R/Tabs.R")

tmc_gadget <- function() {
  ui <- miniPage(
    gadgetTitleBar(title = "TMC RStudio", right = NULL,
                   left = miniTitleBarCancelButton(inputId = "exit", label = "Exit")),

    miniTabstripPanel(
      .log_in_tab,
      .courses_and_excersises_tab,
      .test_and_submit_tab
    )
  )

  server <- function(input, output) {
    test_results <- eventReactive(input$run_tests, {
      tmcRtestrunner::run_tests(print = TRUE)
    })

    # Defining actions for buttons etc
    # UI elements and their actions are given as arguments
    observeEvent(input$login, {
      message <- ""
      title <- ""
      response <- tmcrstudioaddin::authenticate(input$username, input$password)

      # if Bearer token is retrieved login was successful
      if (grepl("Bearer", response[1])) {
        title <- "Success!"
        message <- "Login successful!"
        # show error and error message
      } else {
        title <- response$error
        message <- response$error_description
      }

      # showDialog() needs RStudion versio > 1.1.67
      rstudioapi::showDialog(title = title, message = message, url = "")
    })

    observeEvent(input$exit, {
      shiny::stopApp()
    })

    # render*-function renders UI content and corresponds to *Output-function in UI
    # Here the "textOutput(outputId = "course_display")" in UI is declared to
    # render the value from "selectInput(inputId = "course_select", ...)"
    output$course_display <- renderText({
      input$course_select
    })

    output$test_results_display <- renderUI({
      results <- test_results()

      test_result_output_list <- lapply(1:length(results), function(i) {
        test_name <- results[[i]]$name
        test_status <- results[[i]]$status

        # Assign a color depending on test status
        color <- ifelse(test = identical(x = test_status, y = "pass"), yes = "green", no = "red")

        tags$p(paste(results[[i]]$name, ":", results[[i]]$status),
               style = paste("color:", color, ";font-weight:bold"))
      })

      shiny::tagList(test_result_output_list)
    })
  }

  shiny::runGadget(app = ui, server = server)
}
