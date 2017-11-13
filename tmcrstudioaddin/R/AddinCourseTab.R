.courseTabUI <- function(id, label = "Course tab") {
  ns <- shiny::NS(id)
  organizations <- tmcrstudioaddin::getAllOrganizations()
  miniTabPanel(
    title = "Exercises",
    icon = icon("folder-open"),
    miniContentPanel(
      actionButton(inputId = ns("refresh"), label = "Refresh courselist"),
      selectInput(
        inputId = ns("organizationSelect"),
        label = "Select organization",
        choices = organizations,
        selected = 1
      ),
      actionButton(inputId = ns("refresh"), label = "Refresh courses"),
      selectInput(
        inputId = ns("courseSelect"),
        label = "Select course",
        choices = list(),
        selected = 1
      ),
      actionButton(inputId = ns("download"), label = "Download exercises")
    )
  )
}

.courseTab <- function(input, output, session) {
  observeEvent(input$refresh, {
    courses<-tmcrstudioaddin::getAllCourses('hy')
    print(courses)
      courseNames <- list()
      for (course in courses){
        courseNames<-c(courseNames, course)
      }

    choices <-courseNames
  })
  output$courseDisplay <- renderText({
    input$courseSelect
  })
  observeEvent(input$organizationSelect, {
    organization <- input$organizationSelect
    courses <- tmcrstudioaddin::getAllCourses(organization)
    shiny::updateSelectInput(session, "courseSelect", label = "Select course", choices = courses, selected = 1)
  })
  observeEvent(input$refresh, {
    organization <- input$organizationSelect
    courses <- tmcrstudioaddin::getAllCourses(organization)
    if (length(courses) == 0){
      credentials <- tmcrstudioaddin::getCredentials()
      if (is.null(credentials$token)){
        rstudioapi::showDialog("Not logged in", "Please log in to see courses", "")
      }
    }
    shiny::updateSelectInput(session, "courseSelect", label = "Select course", choices = courses, selected = 1)
  }, ignoreInit = TRUE)

}
