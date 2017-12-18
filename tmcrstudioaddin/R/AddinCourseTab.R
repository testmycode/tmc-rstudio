.courseTabUI <- function(id, label = "Course tab") {
  ns <- shiny::NS(id)
  miniTabPanel(
    title = "Exercises",
    icon = icon("folder-open"),
    miniContentPanel(
      fluidPage(style="padding:0px;margin:0px;",
        fluidRow(
            column(6,class="col-xs-6",
              selectInput(
                inputId = ns("organizationSelect"),
                label = "Select organization",
                choices = list(),
                selected = 1
              )
            ),
            column(6,class="col-xs-6",
              actionButton(
                 inputId = ns("refreshOrganizations"),
                 label = "Refresh organizations",
                 style="margin-top:25px;"
              )
            )
        ),
        fluidRow(
          column(6,class="col-xs-6",
            selectInput(
              inputId = ns("courseSelect"),
              label = "Select course",
              choices = list(),
              selected = 1
            )
          ),
          column(6,class="col-xs-6",
            actionButton(
              inputId = ns("refreshCourses"),
              label = "Refresh courses",
              style="margin-top:25px;"
            )
          )
        )
      ),
      actionButton(
        inputId = ns("download"),
        label = "Download exercises"),
        hidden(
          checkboxInput(
            inputId = ns("all_exercises"),
            label = "Download all exercises"
          )
        ),
      checkboxGroupInput(
        inputId = ns("exercises"),
        label = "",
        choices = list()
      )
    )
  )
}

.courseTab <- function(input, output, session, globalReactiveValues) {
  observeEvent(input$refreshOrganizations, {
    if(UI_disabled) return()

    tmcrstudioaddin::disable_course_tab()
    if(!is.null(globalReactiveValues$credentials$token)){
      organizations <- tmcrstudioaddin::getAllOrganizations()
      choices <- organizations$slug
      names(choices) <- organizations$name
      shiny::updateSelectInput(session, "organizationSelect", label = "Select organization",
                             choices = choices, selected = 1)
    }
    else{
      rstudioapi::showDialog("Not logged in", "Please log in to see organizations", "")
    }
    tmcrstudioaddin::enable_course_tab()
  })

  observeEvent(input$organizationSelect, {
    if(UI_disabled) return()

    tmcrstudioaddin::disable_course_tab()
    organization <- input$organizationSelect
    globalReactiveValues$credentials$organization <- organization
    courses <- tmcrstudioaddin::getAllCourses(organization)
    choices <- courses$id
    names(choices) <- courses$title
    shiny::updateSelectInput(session, "courseSelect", label = "Select course", choices = choices, selected = 1)
    tmcrstudioaddin::enable_course_tab()
  },ignoreInit = TRUE)

  exercise_map <<- list()
  observeEvent(input$refreshCourses, {
    if(UI_disabled) return()

    tmcrstudioaddin::disable_course_tab()
    if (!is.null(globalReactiveValues$credentials$token)){
      organization <- input$organizationSelect
      courses <- tmcrstudioaddin::getAllCourses(organization)
      choices <- courses$id
      names(choices) <- courses$title
      shiny::updateSelectInput(session, "courseSelect", label = "Select course", choices = choices, selected = 1)
    }
    else{
      rstudioapi::showDialog("Not logged in", "Please log in to see courses", "")
    }
    tmcrstudioaddin::enable_course_tab()
  }, ignoreInit = TRUE)

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
  observe({
    if(is.null(globalReactiveValues$credentials$token)){
      shiny::updateSelectInput(session, "organizationSelect", label = "Select organization",
                               choices = list(), selected = 1)
      shiny::updateSelectInput(session, "courseSelect", label = "Select course", choices = list(), selected = 1)
      hide("all_exercises")
      shiny::updateCheckboxGroupInput(session, "exercises", label = "", choices = list())
    }
    else {
      organizations <- tmcrstudioaddin::getAllOrganizations()
      choices <- organizations$slug
      names(choices) <- organizations$name
      shiny::updateSelectInput(session, "organizationSelect", label = "Select organization",
                               choices = choices, selected = ifelse(!is.null(globalReactiveValues$credentials$organization),
                                                                    globalReactiveValues$credentials$organization,
                                                                    1))
      courses <- tmcrstudioaddin::getAllCourses(ifelse(!is.null(globalReactiveValues$credentials$organization),
                                                       globalReactiveValues$credentials$organization,
                                                       1))
      choices2 <- courses$id
      names(choices2) <- courses$title
      shiny::updateSelectInput(session, "courseSelect", label = "Select course", choices = choices2, selected = 1)
    }
  })


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

    #Call submitTab module, which updates exercises
    globalReactiveValues$downloadedExercises <- downloadedExercisesPaths()

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
