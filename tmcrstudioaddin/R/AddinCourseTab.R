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
      ),
      fluidRow(
        column(6,class="col-xs-6",
               checkboxGroupInput(
                 inputId = ns("downloadedExercises"),
                 label = "",
                 choices = list(),
               )
        ),
        column(6,class="col-xs-6",
               hidden(
                 checkboxInput(
                   inputId = ns("updateAllExercises"),
                   label = "Redownload all downloaded exercises (Note: redownloading WILL overwrite your code)"
                 )
               )
        )
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
  downloadedExercisesMap <<- list()

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

    exercise_map <<- list()
    downloadedExercisesMap <<- list()

    hide("all_exercises")
    hide("updateAllExercises")
    shiny::updateCheckboxGroupInput(session, "exercises", label = "", choices = list())
    shiny::updateCheckboxGroupInput(session, "downloadedExercises", label = "", choices = list())
    withProgress(message = "Fetching exercises", {
      exercises <- tmcrstudioaddin::getAllExercises(input$courseSelect)
    })

    separateDownloadedExercises(exercises)

    if(length(exercise_map) > 0){
      show("all_exercises")
      shiny::updateCheckboxGroupInput(session, "exercises", label = "Downloadable exercises", choices = exercise_map)
    }
    if (length(downloadedExercisesMap) > 0) {
      show("updateAllExercises")
      shiny::updateCheckboxGroupInput(session, "downloadedExercises", label = "Redownload Downloaded Exercises", choices = downloadedExercisesMap)
    }

    tmcrstudioaddin::enable_course_tab()
  }, ignoreInit=TRUE)

  separateDownloadedExercises <- function(exercises) {
    exercise_map <<- list()
    downloadedExercisesMap <<- list()

    allExercises <- list()
    exercisePaths <- downloadedExercisesPaths()

    downloadedExercisesId <- c()
    downloadedExercisesName <- c()
    exerciseId <- c()
    exerciseName <- c()

    exerciseNumber <- 1

    for (name in exercises$name) {
      downloaded <- FALSE
      for (exercisePath in exercisePaths)
        if(name == getExerciseName(exercisePath)) {
          downloaded <- TRUE
          break
        }
      if (downloaded) {
        downloadedExercisesId <- c(downloadedExercisesId, exercises$id[[exerciseNumber]])
        downloadedExercisesName <- c(downloadedExercisesName, exercises$name[[exerciseNumber]])
      } else {
        exerciseId <- c(exerciseId, exercises$id[[exerciseNumber]])
        exerciseName <- c(exerciseName, exercises$name[[exerciseNumber]])
      }
      exerciseNumber <- exerciseNumber + 1
    }

    downloadedExercisesMap <<- downloadedExercisesId
    names(downloadedExercisesMap) <<- downloadedExercisesName
    downloadedExercisesMap <<- sort(downloadedExercisesMap)

    exercise_map <<- exerciseId
    names(exercise_map) <<- exerciseName
    exercise_map <<- sort(exercise_map)
  }

  observe({
    if(is.null(globalReactiveValues$credentials$token)){
      shiny::updateSelectInput(session, "organizationSelect", label = "Select organization",
                               choices = list(), selected = 1)
      shiny::updateSelectInput(session, "courseSelect", label = "Select course", choices = list(), selected = 1)
      hide("all_exercises")
      shiny::updateCheckboxGroupInput(session, "exercises", label = "", choices = list())
      hide("updateAllExercises")
      shiny::updateCheckboxGroupInput(session, "downloadedExercises", label = "", choices = list())
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

  observeEvent(input$updateAllExercises, {
    disable("updateAllExercises")

    if(input$updateAllExercises){
      shiny::updateCheckboxGroupInput(session,"downloadedExercises",choices = downloadedExercisesMap , selected = downloadedExercisesMap)
    }
    else{
      shiny::updateCheckboxGroupInput(session,"downloadedExercises" , choices = downloadedExercisesMap , selected = list())
    }
    enable("updateAllExercises")
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

        downloadFromList(course_directory_path)

      })
      rstudioapi::showDialog("Success","Exercises downloaded succesfully","")
    }, error = function(e) {
      rstudioapi::showDialog("Error","Something went wrong","")
    })

    #Call submitTab module, which updates exercises
    globalReactiveValues$downloadedExercises <- downloadedExercisesPaths()

    tmcrstudioaddin::enable_course_tab()
  })

  downloadFromList <- function(course_directory_path) {
    exercises <- c(input$exercises, input$downloadedExercises)
    exerciseNames <- c()
    for (id in input$exercises) {
      exerciseNames <- c(exerciseNames, returnItem(id, exercise_map))
    }
    for (id in input$downloadedExercises) {
      exerciseNames <- c(exerciseNames, returnItem(id, downloadedExercisesMap))
    }
    names(exercises) <- exerciseNames
    for (name in names(exercises)) {
      tmcrstudioaddin::download_exercise(exercises[[name]], zip_name=paste(exercises[[name]],".zip"),
                                         exercise_directory = course_directory_path,
                                         exercise_name = name)
      incProgress(1 / length(exercises))
    }
  }
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
