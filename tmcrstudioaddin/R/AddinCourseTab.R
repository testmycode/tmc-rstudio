.courseTabUI <- function(id, label = "Course tab") {
  ns <- shiny::NS(id)
  organizations <- tmcrstudioaddin::getAllOrganizations()
  choices <- organizations$slug
  credentials<-tmcrstudioaddin::getCredentials()
  names(choices) <- organizations$name
  miniTabPanel(
    title = "Exercises",
    icon = icon("folder-open"),
    miniContentPanel(
      actionButton(inputId = ns("refreshOrganizations"), label = "Refresh organizations"),
      selectInput(
        inputId = ns("organizationSelect"),
        label = "Select organization",
        choices = choices,
        selected = credentials$organization
      ),
      actionButton(inputId = ns("refreshCourses"), label = "Refresh courses"),
      selectInput(
        inputId = ns("courseSelect"),
        label = "Select course",
        choices = choices,
        selected = 1
      ),
      actionButton(inputId = ns("download"), label = "Download exercises"),
      checkboxGroupInput(inputId = ns("exercises"),label="",choices=list())
    )
  )
}

.courseTab <- function(input, output, session) {
  observeEvent(input$refreshOrganizations, {
    organizations <- tmcrstudioaddin::getAllOrganizations()
    choices <- organizations$slug
    names(choices) <- organizations$name
    shiny::updateSelectInput(session, "organizationSelect", label = "Select organization", choices = choices, selected = 1)
  })

  observeEvent(input$organizationSelect, {
    organization <- input$organizationSelect
    credentials <- tmcrstudioaddin::getCredentials()
    credentials$organization <- organization
    tmcrstudioaddin::saveCredentials(credentials)
    courses <- tmcrstudioaddin::getAllCourses(organization)
    choices <- courses$id
    names(choices) <- courses$name
    shiny::updateSelectInput(session, "courseSelect", label = "Select course", choices = choices, selected = 1)
  })

  exercise_map <<- list()

  observeEvent(input$courseSelect, {
    shiny::updateCheckboxGroupInput(session, "exercises", label = "", choices = list())
    withProgress(message = "Fetching exercises", {
      exercises <- tmcrstudioaddin::getAllExercises(input$courseSelect)
    })
    choices <- exercises$id
    names(choices)<-exercises$name
    exercise_map <<- exercises$id
    names(exercise_map) <<- exercises$name

    if(length(choices)>0){

      shiny::updateCheckboxGroupInput(session, "exercises", label = "Downloadable exercises", choices = choices)
    }
  }, ignoreInit=TRUE)

  observeEvent(input$refreshCourses, {
    organization <- input$organizationSelect
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
      withProgress(message="Downloading exercises",{

        organization <- input$organizationSelect
        courses <- tmcrstudioaddin::getAllCourses(organization)
        courseName <- courses$name[courses$id==input$courseSelect]

        user_home <- Sys.getenv("HOME")
        r_home <- file.path(user_home, "tmcr-projects")

        course_directory_path <- file.path(r_home, courseName,
          fsep = .Platform$file.sep)


        if(!dir.exists(course_directory_path)){
          dir.create(course_directory_path)
        }

        for(exercise in input$exercises){
          name <- returnItem(exercise, exercise_map)
          tmcrstudioaddin::download_exercise(exercise,zip_name=paste(exercise,".zip"),
                                             exercise_directory = course_directory_path,
                                             exercise_name = name)
          incProgress(1/length(input$exercises))
        }
      })
      rstudioapi::showDialog("Success","Exercises downloaded succesfully","")
    }, error = function(e){
      rstudioapi::showDialog("Error","Something went wrong","")
    })
  })
}

returnItem <- function(item, list) {
  ret <- ""
  for(name in names(list)) {
    if(list[[name]] == item) {
      ret <- name
    }
  }
  return(ret)
}
