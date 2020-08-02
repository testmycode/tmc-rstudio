#downloadedExercisesPaths <- downloadedExercisesPaths

.courseTabUI <- function(id, label = "Course tab") {
  ns <- shiny::NS(id)
  .ddprint(ns("selectExercise"))
  inputIDs    <- c("organizationSelect",
                   "refreshOrganizations",
                   "courseSelect",
                   "refreshCourses",
                   "download",
                   "exercises",
#                  "unpublished_exercises", # this is special.
                   "downloadedExercises",
                   "all_exercises",
                   "updateAllExercises")
  ns_inputIDs <- sapply(inputIDs, ns)
  fluid_row1 <- fluidRow(column(width = 6,
                                class = "col-xs-6",
                                selectInput(inputId   = ns("organizationSelect"),
                                            label     = "Select organization",
                                            choices   = list(),
                                            selected  = 1)),
                         column(width = 6,
                                class = "col-xs-6",
                                actionButton(inputId  = ns("refreshOrganizations"),
                                             label    = "Refresh organizations",
                                             style    = "margin-top:25px;")))
  fluid_row2 <-  fluidRow(column(width = 6,
                                 class = "col-xs-6",
                                 selectInput(inputId  = ns("courseSelect"),
                                             label    = "Select course",
                                             choices  = list(),
                                             selected = 1)),
                          column(width = 6,
                                 class = "col-xs-6",
                                 actionButton(inputId = ns("refreshCourses"),
                                              label   = "Refresh courses",
                                              style   = "margin-top:25px;")))
  checkbox_group1 <- checkboxGroupInput(inputId = ns("exercises"),
                                        label   = "",
                                        choices = list())
  checkbox_group2 <- checkboxGroupInput(inputId = ns("unpublished_exercises"),
                                        label   = "",
                                        choices = list())
  checkbox_group3 <- checkboxGroupInput(inputId = ns("downloadedExercises"),
                                        label   = "",
                                        choices = list())
  checkbox1 <- hidden(checkboxInput(inputId = ns("all_exercises"),
                                    label   = "Download all exercises",
                                    value   = FALSE))
  checkbox2 <- hidden(checkboxInput(inputId = ns("updateAllExercises"),
                                    label   = paste("Redownload all downloaded exercises",
                                                    "(Note: redownloading WILL overwrite",
                                                    "your code)")))
  fluid_row3 <- fluidRow(column(width = 6, class = "col-xs-6", checkbox_group3),
                         column(width = 6, class = "col-xs-6", checkbox2))
  tab_panel <- miniTabPanel(title = "Exercises",
                            icon  = icon("folder-open"),
                            miniContentPanel(fluidPage(style = "padding:0px;margin:0px;",
                                                       fluid_row1, fluid_row2),
                                             actionButton(inputId = ns("download"),
                                                          label   = "Download exercises"),
                                             checkbox1,
                                             checkbox_group1,
                                             checkbox_group2, fluid_row3))
  list(ns_inputIDs    = ns_inputIDs,
       mini_tab_panel = tab_panel)
}

.courseTab <- function(input, output, session, globalReactiveValues) {
  grv <- globalReactiveValues
  enable_tab_UI <- function() {
    .dprint("Enabling new way")
    .ddprint("Ready to do this courseTab")
    # Ok. This is just an ad hoc way to do it and is caused by mixing
    # responsibilities. Actually we should just enable and disable ALL the
    # buttons.
    shinyjs::delay(ms = 10,
                   expr = {
                     .dprint("Launching new way...")
                     tmcrstudioaddin::enable_UI_elements(grv$UI_elements)
                     globalReactiveValues$UI_disabled <- FALSE
                   })
  }
  disable_tab_UI <- function() {
    .dprint("Disabling new way courseTab")
    tmcrstudioaddin::disable_UI_elements(grv$UI_elements)
    globalReactiveValues$UI_disabled <- TRUE
  }
  observeEvent(input$refreshOrganizations, {
    if (grv$UI_disabled) {
      .ddprint("Disabled... ")
      return()
    }

    disable_tab_UI()
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
    enable_tab_UI()
    .dprint("refresh organizations")
  })

  observeEvent(input$organizationSelect, {
    if (grv$UI_disabled) {
      .ddprint("Disabled... ")
      return()
    }

    disable_tab_UI()
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
    enable_tab_UI()
  }, ignoreInit = TRUE)

  observeEvent(input$refreshCourses, {
    if (grv$UI_disabled) {
      .ddprint("Disabled... ")
      return()
    }

    disable_tab_UI()
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
    enable_tab_UI()
    .dprint("refresh courses")
  }, ignoreInit = TRUE)

  observeEvent(input$courseSelect, {
    if (grv$UI_disabled) {
      .ddprint("Disabled... ")
      return()
    }
    .dprint("courseSelect")

    disable_tab_UI()

    hideCourseExercises()

    withProgress(message = "Fetching exercises", {
      exercises <- tmcrstudioaddin::getAllExercises(input$courseSelect)
    })

    separateDownloadedExercises(exercises, NA, globalReactiveValues, input$courseSelect)

    enable_tab_UI()
  }, ignoreInit = TRUE)

  separateDownloadedExercises <- function(exercises, exercises_old, globalReactiveValues,
                                          courseid = NA) {
    .dprint("separateDownloadedExercises()")
    globalReactiveValues$exerciseMap             <- list()
    globalReactiveValues$unpublishedExercisesMap <- list()
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
    exercise           <- list()
    unpublished        <- list()

    .ddprint(exercisePaths)
    exercise_names <- sapply(exercisePaths, getExerciseName)
    .ddprint(exercise_names)
    for (exercise_number in seq_along(exercises$name)) {
      short_name  <- exercises$name[[exercise_number]]
      long_name   <- paste(course_name, short_name, sep = ":")
      exercise_id <- exercises$id[[exercise_number]]
      exercise_published <- exercises$unlocked[[exercise_number]]
      if (long_name %in% exercise_names) {
        downloadedExercise[[short_name]] <- exercise_id
      } else if (exercise_published) {
        exercise[[short_name]] <- exercise_id
      } else {
        .ddprint(exercise_number)
        .ddprint(str(exercises))
        .ddprint("Not downloaded")
        .dprint("Not published")
        .ddprint(str(exercises[exercise_number, ]))
        unpublished[[short_name]] <- exercise_id
      }
    }
    .ddprint(str(downloadedExercise))
    .ddprint(str(exercise))
    .ddprint(str(unpublished))

    if (length(downloadedExercise) > 0) {
      globalReactiveValues$downloadedExercisesMap <- .sort_list(downloadedExercise)
    }
    if (length(exercise) > 0) {
      globalReactiveValues$exerciseMap <- .sort_list(exercise)
    }
    if (length(unpublished) > 0) {
      globalReactiveValues$unpublishedExercisesMap <- .sort_list(unpublished)
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
    if (grv$UI_disabled) {
      .ddprint("Disabled... ")
      return()
    }

    disable_tab_UI()

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

    enable_tab_UI()
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

  observeEvent(globalReactiveValues$unpublishedExercisesMap, {
    if (length(globalReactiveValues$unpublishedExercisesMap) > 0) {
      .dprint("unpublished_exercises")
      shiny::updateCheckboxGroupInput(session,
                                      "unpublished_exercises",
                                      label = "Unpublished exercises",
                                      choices = globalReactiveValues$unpublishedExercisesMap)
      .dprint("disabling_unpublished_exercises")
      # delay is needed, otherwise this will not work
      shinyjs::delay(ms = 0,
                     expr = shinyjs::disable("unpublished_exercises"))
    }
  })

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
                                      label = "Redownload already downloaded exercises",
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
                                    "unpublished_exercises",
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
