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
      hidden(
        checkboxInput(inputId = ns("all_exercises"), label = "Download all exercises")
      ),
      checkboxGroupInput(inputId = ns("exercises"),label = "",choices = list())
    )
  )
}

.courseTab <- function(input, output, session) {
  observeEvent(input$refreshOrganizations, {
    if(UI_disabled) return()

    tmcrstudioaddin::disable_course_tab()
    organizations <- tmcrstudioaddin::getAllOrganizations()
    choices <- organizations$slug
    names(choices) <- organizations$name
    shiny::updateSelectInput(session, "organizationSelect", label = "Select organization",
                             choices = choices, selected = 1)
    tmcrstudioaddin::enable_course_tab()
  })

  observeEvent(input$organizationSelect, {
    if(UI_disabled) return()

    tmcrstudioaddin::disable_course_tab()
    organization <- input$organizationSelect
    credentials <- tmcrstudioaddin::getCredentials()
    credentials$organization <- organization
    tmcrstudioaddin::saveCredentials(credentials)
    courses <- tmcrstudioaddin::getAllCourses(organization)
    choices <- courses$id
    names(choices) <- courses$name
    shiny::updateSelectInput(session, "courseSelect", label = "Select course", choices = choices, selected = 1)
    tmcrstudioaddin::enable_course_tab()
  })

  exercise_map <<- list()

  observeEvent(input$courseSelect, {
    if(UI_disabled) return()

    tmcrstudioaddin::disable_course_tab()
    hide("all_exercises")
    shiny::updateCheckboxGroupInput(session, "exercises", label = "", choices = list())
    withProgress(message = "Fetching exercises", {
      exercises <- tmcrstudioaddin::getAllExercises(input$courseSelect)
    })
    exercise_map <<- exercises$id
    names(exercise_map) <<- exercises$name

    if(length(exercise_map)>0){
      show("all_exercises")
      shiny::updateCheckboxGroupInput(session, "exercises", label = "Downloadable exercises", choices = exercise_map)
    }

    tmcrstudioaddin::enable_course_tab()
  }, ignoreInit=TRUE)

  observeEvent(input$refreshCourses, {
    if(UI_disabled) return()

    tmcrstudioaddin::disable_course_tab()
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
    tmcrstudioaddin::enable_course_tab()
  }, ignoreInit = TRUE)

  observeEvent(input$all_exercises, {
    disable("all_exercises")

    if(input$all_exercises){
      shiny::updateCheckboxGroupInput(session,"exercises",choices = exercise_map,selected = exercise_map)
    }
    else{
      shiny::updateCheckboxGroupInput(session,"exercises",choices = exercise_map,selected = list())
    }
    enable("all_exercises")
  })

  observeEvent(input$download, {
    if(UI_disabled) return()

    tmcrstudioaddin::disable_course_tab()

    tryCatch({
      withProgress(message="Downloading exercises",{

        organization <- input$organizationSelect
        courses <- tmcrstudioaddin::getAllCourses(organization)
        courseName <- courses$name[courses$id==input$courseSelect]

        if(!check_if_properties_exist()) {
          create_properties_file()
        }

        course_directory_path <- file.path(get_projects_folder(), courseName,
          fsep = .Platform$file.sep)

        if(!dir.exists(course_directory_path)){
          dir.create(course_directory_path, recursive = TRUE)
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

    tmcrstudioaddin::enable_course_tab()
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
