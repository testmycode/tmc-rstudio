.courseTabUI <- function(id, label = "Course tab") {
  ns <- shiny::NS(id)

  miniTabPanel(
    title = "Exercises",
    icon = icon("folder-open"),

    miniContentPanel(
      selectInput(
        inputId = ns("courseSelect"),
        label = "Select course",
        choices = list(
          "Introduction to Statistics and R I" = "Course 1",
          "Introduction to Statistics and R II" = "Course 2",
          "Data mining" = "Course 3"
        ),
        selected = 1
      ),
      textOutput(outputId = ns("courseDisplay"))
    )
  )
}

.courseTab <- function(input, output, session) {
  output$courseDisplay <- renderText({
    input$courseSelect
  })
}
