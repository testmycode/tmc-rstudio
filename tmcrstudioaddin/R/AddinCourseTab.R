.course_tab_ui <- function(id, label = "Course tab") {
  ns <- shiny::NS(id)

  miniTabPanel(
    title = "Exercises",
    icon = icon("folder-open"),

    miniContentPanel(
      selectInput(
        inputId = ns("course_select"),
        label = "Select course",
        choices = list(
          "Introduction to Statistics and R I" = "Course 1",
          "Introduction to Statistics and R II" = "Course 2",
          "Data mining" = "Course 3"
        ),
        selected = 1
      ),
      textOutput(outputId = ns("course_display"))
    )
  )
}

.course_tab <- function(input, output, session) {
  output$course_display <- renderText({
    input$course_select
  })
}
