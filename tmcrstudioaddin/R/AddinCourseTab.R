#downloadedExercisesPaths <- downloadedExercisesPaths

globalVariables(c(".UI_disabled", ".selectedExercisePath"))
.courseTabUI <- function(id, label = "Course tab") {
  ns <- shiny::NS(id)
  miniTabPanel(
    title = "Exercises",
    icon = icon("folder-open"),
    miniContentPanel(
      fluidPage(style = "padding:0px;margin:0px;",
        fluidRow(
            column(6, class = "col-xs-6",
              selectInput(
                inputId = ns("organizationSelect"),
                label = "Select organization",
                choices = list(),
                selected = 1
              )
            ),
            column(6, class = "col-xs-6",
              actionButton(
                 inputId = ns("refreshOrganizations"),
                 label = "Refresh organizations",
                 style = "margin-top:25px;"
              )
            )
        ),
        fluidRow(
          column(6, class = "col-xs-6",
            selectInput(
              inputId = ns("courseSelect"),
              label = "Select course",
              choices = list(),
              selected = 1
            )
          ),
          column(6, class = "col-xs-6",
            actionButton(
              inputId = ns("refreshCourses"),
              label = "Refresh courses",
              style = "margin-top:25px;"
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
          label = "Download all exercises",
          value = FALSE
        )
      ),
      checkboxGroupInput(
        inputId = ns("exercises"),
        label = "",
        choices = list()
      ),
      fluidRow(
        column(6, class = "col-xs-6",
               checkboxGroupInput(
                 inputId = ns("downloadedExercises"),
                 label = "",
                 choices = list(),
               )
        ),
        column(6, class = "col-xs-6",
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
    if (.UI_disabled) return()

    tmcrstudioaddin::disable_course_tab()
    if (!is.null(globalReactiveValues$credentials$token)) {
      organizations <- tmcrstudioaddin::getAllOrganizations()
      choices <- organizations$slug
      names(choices) <- organizations$name
      shiny::updateSelectInput(session,
                               "organizationSelect",
                               label = "Select organization",
                               choices = choices,
                               selected = 1)
    }
    else{
      rstudioapi::showDialog("Not logged in",
                             "Please log in to see organizations",
                             "")
    }
    tmcrstudioaddin::enable_course_tab()
    .dprint("refresh organizations")
  })

  observeEvent(input$organizationSelect, {
    if (.UI_disabled) return()

    tmcrstudioaddin::disable_course_tab()
    organization <- input$organizationSelect
    globalReactiveValues$credentials$organization <- organization
    courses <- tmcrstudioaddin::getAllCourses(organization)
    globalReactiveValues$coursesInfo$all_courses <- courses
    choices <- courses$id
    names(choices) <- courses$title
    shiny::updateSelectInput(session,
                             "courseSelect",
                             label = "Select course",
                             choices = choices,
                             selected = 1)
    tmcrstudioaddin::enable_course_tab()
  }, ignoreInit = TRUE)

  observeEvent(input$refreshCourses, {
    if (.UI_disabled) return()

    tmcrstudioaddin::disable_course_tab()
    if (!is.null(globalReactiveValues$credentials$token)) {
      organization <- input$organizationSelect
      courses <- tmcrstudioaddin::getAllCourses(organization)
      choices <- courses$id
      names(choices) <- courses$title
      shiny::updateSelectInput(session,
                               "courseSelect",
                               label = "Select course",
                               choices = choices,
                               selected = 1)
    }
    else{
      rstudioapi::showDialog("Not logged in",
                             "Please log in to see courses",
                             "")
    }
    tmcrstudioaddin::enable_course_tab()
    .dprint("refresh courses")
  }, ignoreInit = TRUE)

  observeEvent(input$courseSelect, {
    if (.UI_disabled) return()
    .dprint("courseSelect")

    tmcrstudioaddin::disable_course_tab()

    hideCourseExercises()

    withProgress(message = "Fetching exercises", {
      exercises <- tmcrstudioaddin::getAllExercises(input$courseSelect)
    })

    separateDownloadedExercises(exercises, NA, globalReactiveValues, input$courseSelect)

    tmcrstudioaddin::enable_course_tab()
  }, ignoreInit = TRUE)

  separateDownloadedExercises <- function(exercises, exercises_old, globalReactiveValues,
                                          courseid = NA) {
    .dprint("separateDownloadedExercises()")
    globalReactiveValues$exerciseMap             <- list()
    globalReactiveValues$downloadedExercisesMap  <- list()
    # store the coursesId and exercises
    if (!is.null(exercises)) {
      globalReactiveValues$coursesInfo$exercises   <- exercises
    } else {
      exercises <- globalReactiveValues$coursesInfo$exercises
      # exercises <- exercises_old
    }
    if (!is.na(courseid))
      globalReactiveValues$coursesInfo$courseid  <- courseid


    #allExercises <- list()
    .ddprint(courseid)
    .ddprint(str(globalReactiveValues$coursesInfo))
    exercisePaths <- downloadedExercisesPaths()
    all_courses   <- globalReactiveValues$coursesInfo$all_courses
    courseid      <- globalReactiveValues$coursesInfo$courseid
    course_name   <- all_courses$name[all_courses$id == courseid]
    #
    .dprint(course_name)
    .dprint(all_courses$title[all_courses$id == courseid])

    downloadedExercise <- list()
    exercise <- list()

    .ddprint(exercisePaths)
    exercise_names <- sapply(exercisePaths, getExerciseName)
    .ddprint(exercise_names)
    for (exercise_number in seq_along(exercises$name)) {
      short_name  <- exercises$name[[exercise_number]]
      long_name   <- paste(course_name, short_name, sep = ":")
      exercise_id <- exercises$id[[exercise_number]]
      if (long_name %in% exercise_names) {
        downloadedExercise[[short_name]] <- exercise_id
      } else {
        .ddprint(exercise_number)
        .ddprint(str(exercises))
        #print(str(exercises[exercise_number, ]))
        exercise[[short_name]] <- exercise_id
      }
    }
    .ddprint(str(downloadedExercise))
    .ddprint(str(exercise))

    globalReactiveValues$downloadedExercisesMap <- downloadedExercise
    # rewrite this in a proper R way
    if (length(globalReactiveValues$downloadedExercisesMap) > 0) {
      globalReactiveValues$downloadedExercisesMap <-
        .sort_list(globalReactiveValues$downloadedExercisesMap)
    }
    globalReactiveValues$exerciseMap <- exercise
    if (length(globalReactiveValues$exerciseMap) > 0) {
      globalReactiveValues$exerciseMap <-
        .sort_list(globalReactiveValues$exerciseMap)
    }
    .ddprint(str(globalReactiveValues$downloadedExercisesMap))
  }

  observe({
    .dprint("observer()")
    if (is.null(globalReactiveValues$credentials$token)) {
      shiny::updateSelectInput(session,
                               "organizationSelect",
                               label = "Select organization",
                               choices = list(),
                               selected = 1)
      shiny::updateSelectInput(session,
                               "courseSelect",
                               label = "Select course",
                               choices = list(),
                               selected = 1)
      hideCourseExercises()
    }
    else {
      organizations <- tmcrstudioaddin::getAllOrganizations()
      choices <- organizations$slug
      names(choices) <- organizations$name
      shiny::updateSelectInput(session,
                               "organizationSelect",
                               label = "Select organization",
                               choices = choices,
                               selected = ifelse(!is.null(globalReactiveValues$credentials$organization),
                                                 globalReactiveValues$credentials$organization,
                                                 1))
      courses <-
        tmcrstudioaddin::getAllCourses(ifelse(!is.null(globalReactiveValues$credentials$organization),
                                              globalReactiveValues$credentials$organization,
                                              1))
      choices2 <- courses$id
      names(choices2) <- courses$title
      shiny::updateSelectInput(session,
                               "courseSelect",
                               label = "Select course",
                               choices = choices2,
                               selected = 1)
    }
  })


  observeEvent(input$all_exercises, {
    .dprint("all_exercises()")
    shinyjs::disable("all_exercises")

    if (input$all_exercises) {
      shiny::updateCheckboxGroupInput(session,
                                      "exercises",
                                      choices = globalReactiveValues$exerciseMap,
                                      selected = globalReactiveValues$exerciseMap)
    }
    else{
      shiny::updateCheckboxGroupInput(session,
                                      "exercises",
                                      choices = globalReactiveValues$exerciseMap,
                                      selected = list())
    }
    shinyjs::enable("all_exercises")
  })

  observeEvent(input$updateAllExercises, {
    shinyjs::disable("updateAllExercises")
    .dprint("updateAllExercises()")

    if (input$updateAllExercises) {
      shiny::updateCheckboxGroupInput(session,
                                      "downloadedExercises",
                                      choices = globalReactiveValues$downloadedExercisesMap,
                                      selected = globalReactiveValues$downloadedExercisesMap)
    }
    else{
      shiny::updateCheckboxGroupInput(session,
                                      "downloadedExercises",
                                      choices = globalReactiveValues$downloadedExercisesMap,
                                      selected = list())
    }
    shinyjs::enable("updateAllExercises")
  })

  observeEvent(input$download, {
    if (.UI_disabled) return()

    tmcrstudioaddin::disable_course_tab()

    tryCatch({
      withProgress(message = "Downloading exercises", {
        organization <- input$organizationSelect
        courses <- tmcrstudioaddin::getAllCourses(organization)
        courseName <- courses$name[courses$id == input$courseSelect]

        course_directory_path <- file.path(get_projects_folder(), courseName,
                                           fsep = .Platform$file.sep)

        if (!dir.exists(course_directory_path)) {
          dir.create(course_directory_path, recursive = TRUE)
        }
        .dprint(course_directory_path)
        num_of_downloaded <- downloadFromList(course_directory_path,
                                              globalReactiveValues)
      })
      download_success_message <-
        if (num_of_downloaded == 0) {
          "You didn't choose any exercises i.e. download was successful"
        } else if (num_of_downloaded == 1) {
          paste("You downloaded one exercise successfully", sep = " ")
        } else {
          paste("You downloaded",
                as.character(num_of_downloaded),
                "exercises successfully", sep = " ")
        }
      rstudioapi::showDialog("Success", download_success_message, "")
    }, error = function(e) {
    pre_error <- e$message
    download_errormsgs <- list(keys = c("Path exists and overwrite is FALSE",
                                        "argument is of length zero",
                                        "Forbidden (HTTP 403)",
                                        pre_error))
    download_errormsgs$msgs_win <- c("Your download failed \
(Path exists and overwrite is FALSE). There is something wrong with the \
file permissions or previous download failed in halfway.<p>Please \
contact the course instructors in this case.",
"Please select first select both organization and course. \
Remember to refresh the lists.",
"One or more exercises you chose have not been published yet",
pre_error)
    download_errormsgs$msgs_unix <- c("Your download failed \
(Path exists and overwrite is FALSE). There is something wrong with the \
file permissions or previous download failed in halfway.<p>Please \
contact the course instructors in this case.",
"Please select first select both organization and course. \
Remember to refresh the lists.",
"One or more exercises you chose have not been published yet",
pre_error)
    if (!is.null(.Platform$OS.type) && .Platform$OS.type == "windows") {
      errormsg <-
        download_errormsgs$msgs_win[download_errormsgs$keys == pre_error][1]
    } else {
      errormsg <-
        download_errormsgs$msgs_unix[download_errormsgs$keys == pre_error][1]
    }
    if (is.null(.Platform$OS.type)) {
      print("This is mysterious machine")
    }
      download_error_message <- errormsg
      cat("Error")
      cat(" : ")
      cat(pre_error)
      cat("\n")
      rstudioapi::showDialog("Error", download_error_message, "")
    })

    #Call submitTab module, which updates exercises
    globalReactiveValues$downloadedExercises <- downloadedExercisesPaths()

    exercises <- list()

    exercises$id <- .as_unnamed_list(c(globalReactiveValues$downloadedExercisesMap,
                                   globalReactiveValues$exerciseMap))

    .ddprint("exercises$id")
    .ddprint(str(exercises$id))
    exercises$name <- .as_unnamed_list(c(names(globalReactiveValues$downloadedExercisesMap),
                                     names(globalReactiveValues$exerciseMap)))
    .ddprint("exercises$name")
    .ddprint(str(exercises$name))

    hideCourseExercises()
    separateDownloadedExercises(exercises = NULL, exercises, globalReactiveValues)

    tmcrstudioaddin::enable_course_tab()
  })

  downloadFromList <- function(course_directory_path, globalReactiveValues) {
    grv <- globalReactiveValues
    exercises2 <- grv$exerciseMap[grv$exerciseMap %in% input$exercises]
    exercises3 <- grv$downloadedExercisesMap[grv$downloadedExercisesMap %in% input$downloadedExercises]
    exercises <- c(exercises2, exercises3)
    .ddprint(str(exercises))
    .dprint("downloadFromList()")
    for (name in names(exercises)) {
      zip_name <- paste0(exercises[[name]], ".zip")
      tmcrstudioaddin::download_exercise(exercises[[name]],
                                         zip_target    = tempdir(),
                                         zip_name      = zip_name,
                                         exercise_directory = course_directory_path,
                                         exercise_name = name,
                                         unique_random = TRUE)
      incProgress(1 / length(exercises))
    }
    length(exercises)
  }

  .dprint("downloadFromList()-2")

  observeEvent(globalReactiveValues$downloadedExercisesMap, {
    if (length(globalReactiveValues$downloadedExercisesMap) > 0) {
      .ddprint("update all exercises")
      show("updateAllExercises")
      shiny::updateCheckboxInput(session,
                                 "updateAllExercises",
                                 label = "Redownload all downloaded exercises (Note: redownloading WILL overwrite your code)",
                                 value = FALSE)
      shiny::updateCheckboxGroupInput(session,
                                      "downloadedExercises",
                                      label = "Redownload Downloaded Exercises",
                                      choices = globalReactiveValues$downloadedExercisesMap)
    }
  })

  observeEvent(globalReactiveValues$exerciseMap, {
    if (length(globalReactiveValues$exerciseMap) > 0) {
      .ddprint("showing all exercises")
      show("all_exercises")
      shiny::updateCheckboxInput(session, "all_exercises",
                                 label = "Downloaded all exercises",
                                 value = FALSE)
      shiny::updateCheckboxGroupInput(session,
                                      "exercises",
                                      label = "Downloadable exercises",
                                      choices = globalReactiveValues$exerciseMap)
    }
  })

  hideCourseExercises <- function() {
    .ddprint("hideCourseExercises")
    shinyjs::hide("all_exercises")
    shinyjs::hide("updateAllExercises")
    shiny::updateCheckboxGroupInput(session,
                                    "exercises",
                                    label = "",
                                    choices = list())
    shiny::updateCheckboxGroupInput(session,
                                    "downloadedExercises",
                                    label = "",
                                    choices = list())
  }
}


.as_unnamed_list <- function(x) {
  return(unname(as.list(x)))
}

# Sorts list based on the names of items
.sort_list <- function(list_to_sort) {
  list_items        <- unlist(names(list_to_sort))
  names(list_items) <- unlist(.as_unnamed_list(list_to_sort))
  list_items        <- sort(list_items)
  ret_list2         <- as.list(names(list_items))
  names(ret_list2)  <- unname(list_items)
  return(ret_list2)
}

.debug_set <- function() {
  assign(x = ".__tmc_debug", value = FALSE, envir = .GlobalEnv)
}

.ddebug_set <- function() {
  assign(x = ".__tmc_debug", value = TRUE, envir = .GlobalEnv)
}

.debug_unset <- function() {
  if (exists(".__tmc_debug")) rm(".__tmc_debug", envir = .GlobalEnv)
}

.ddprint <- function(x) {
  if (exists(".__tmc_debug") && get(".__tmc_debug")) {
    print(x)
  }
}
.dprint <- function(x) {
  if (exists(".__tmc_debug")) {
    print(x)
  }
}
