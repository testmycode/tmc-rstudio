.courseTabUI <- function(id, label = "Course tab") {
  ns <- shiny::NS(id)
  choices<-list(
    "Introduction to Statistics and R I" = "Course 1",
    "Introduction to Statistics and R II" = "Course 2",
    "Data mining" = "Course 3"
  )
  miniTabPanel(
    title = "Exercises",
    icon = icon("folder-open"),

    miniContentPanel(
      actionButton(inputId = ns("refresh"), label = "Refresh courselist"),
      selectInput(
        inputId = ns("courseSelect"),
        label = "Select course",
        choices = choices,
        selected = 1
      ),
      textOutput(outputId = ns("courseDisplay"))
    )
  )
}

.courseTab <- function(input, output, session) {
  observeEvent(input$refresh, {
    courses<-tmcrstudioaddin::getAllCourses('hy')
      courseNames <- list()
      for (course in courses){
        courseNames<-c(courseNames,course$name)
      }

    choices <-courseNames
  })
  output$courseDisplay <- renderText({
    input$courseSelect
  })
}
