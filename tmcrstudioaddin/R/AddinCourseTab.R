.courseTabUI <- function(id, label = "Course tab") {
  ns <- shiny::NS(id)
  miniTabPanel(
    title = "Exercises",
    icon = icon("folder-open"),
    miniContentPanel(
      actionButton(inputId = ns("refreshOrganizations"), label = "Refresh organizations"),
      selectInput(
        inputId = ns("organizationSelect"),
        label = "Select organization",
        choices = list(),
        selected = 1
      ),
      actionButton(inputId = ns("refreshCourses"), label = "Refresh courses"),
      selectInput(
        inputId = ns("courseSelect"),
        label = "Select course",
        choices = list(),
        selected = 1
      ),
      actionButton(inputId = ns("download"), label = "Download exercises"),
      checkboxGroupInput(inputId = ns("exercises"),label="",choices=list())
    )
  )
}

.courseTab <- function(input, output, session) {
  observeEvent(input$refreshOrganizations, {
    choices <- tmcrstudioaddin::getAllOrganizations()
    shiny::updateSelectInput(session, "organizationSelect", label = "Select organization", choices = choices, selected = 1)
  })

  observeEvent(input$organizationSelect, {
    organization <- input$organizationSelect
    courses <- tmcrstudioaddin::getAllCourses(organization)
    choices <- courses$id
    names(choices) <- courses$name
    shiny::updateSelectInput(session, "courseSelect", label = "Select course", choices = choices, selected = 1)
  })

  observeEvent(input$courseSelect, {
    exercises <- tmcrstudioaddin::getAllExercises(input$courseSelect)
    choices <- exercises$id
    names(choices)<-exercises$name
    shiny::updateCheckboxGroupInput(session, "exercises", label = "Downloadable exercises", choices = choices)
  }, ignoreInit=TRUE)

  observeEvent(input$refresh, {
    organization <- input$organizationSelect
    courses <- tmcrstudioaddin::getAllCourses(organization)
    courses <- tmcrstudioaddin::getAllCourses(organization)
    choices <- courses$id
    names(choices) <- courses$name
    if (length(choices) == 0){
      credentials <- tmcrstudioaddin::getCredentials()
      if (is.null(credentials$token)){
        rstudioapi::showDialog("Not logged in", "Please log in to see courses", "")
      }
    }
    shiny::updateSelectInput(session, "courseSelect", label = "Select course", choices = choices, selected = 1)
  }, ignoreInit = TRUE)

  observeEvent(input$download, {
    tryCatch({
      organization <- input$organizationSelect
      courses <- tmcrstudioaddin::getAllCourses(organization)
      courseName <- courses$name[courses$id==input$courseSelect]
      if(!dir.exists(courseName)){
        dir.create(courseName)
      }
      for(exercise in input$exercises){
        tmcrstudioaddin::download_exercise(exercise,zip_name=paste(exercise,".zip"), exercise_directory = courseName)
      }
      rstudioapi::showDialog("Success","Exercises downloaded succesfully","")
    }, error = function(e){
      rstudioapi::showDialog("Error","Something went wrong","")
    })
  })
}
