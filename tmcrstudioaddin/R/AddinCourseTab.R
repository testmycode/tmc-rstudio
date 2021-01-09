#downloadedExercisesPaths <- downloadedExercisesPaths

.courseTabUI <- function(id, label = "Course tab") {
  .dprint(".courseTabUI launched")
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
  .dprint(".courseTab launched")
  grv <- globalReactiveValues
  rv  <- reactiveValues(selection  = list(course = character(0),
                                          org    = character(0)),
                        state      = list(logged_in       = FALSE,
                                          org_chosen      = FALSE,
                                          has_courses     = FALSE,
                                          course_chosen   = FALSE,
                                          org_visible     = FALSE,
                                          course_visible  = FALSE),
                        logged_in  = FALSE,
                        stored_org = NULL,
                        organization_toggle = FALSE)
#
# normal functions
#

  enable_tab_UI <- function() {
    .dprint("Enabling new way")
    .ddprint("Ready to do this courseTab")
    # Ok. This is just an ad hoc way to do it and is caused by mixing
    # responsibilities. Actually we should just enable and disable ALL the
    # buttons.
    shinyjs::delay(ms = 10,
                   expr = {
                     .dprint("Launching new way...")
                     tmcrstudioaddin::enable_UI_elements(grv$UI_elements, grv$UI_state)
                     globalReactiveValues$UI_disabled <- FALSE
                   })
  }
  disable_tab_UI <- function() {
    .dprint("Disabling new way courseTab")
    tmcrstudioaddin::disable_UI_elements(grv$UI_elements)
    globalReactiveValues$UI_disabled <- TRUE
  }

  separateDownloadedExercises <- function(exercises,
                                          exercises_old,
                                          globalReactiveValues,
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

  downloadFromList <- function(course_directory_path, globalReactiveValues) {
    grv <- globalReactiveValues
    exercises2 <- grv$exerciseMap[grv$exerciseMap %in% input$exercises]
    exercises3 <- grv$downloadedExercisesMap[grv$downloadedExercisesMap %in% input$downloadedExercises]
    exercises <- c(exercises2, exercises3)
    .ddprint(str(exercises))
    .dprint("downloadFromList()")
    for (i in seq_along(names(exercises))) {
      name     <- names(exercises)[[i]]
      zip_name <- paste0(exercises[[name]], ".zip")
      tmcrstudioaddin::download_exercise(exercises[[name]],
                                         zip_target    = tempdir(),
                                         zip_name      = zip_name,
                                         exercise_directory = course_directory_path,
                                         exercise_name = name,
                                         credentials   = grv$credentials,
                                         unique_random = TRUE)
      incProgress(message = paste("Downloaded", i, "/", length(exercises)),
                  amount  = 1 / length(exercises))
      Sys.sleep(0.2)
    }
    length(exercises)
  }

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

#
# CT_observer functions
#

  CT_observer1 <- function() {
    .dprint("CT_observer1 launched...")
    disable_tab_UI()
      # .dprint("getAllOrganizations site 2")
      # when pressing refersh, so this is ok
    organizations <- tmcrstudioaddin::get_all_organizations(grv$credentials)
    choices <- organizations$slug
    names(choices) <- organizations$name
    #
    selected_org   <- rv$selection$org
    print_rv()
    shiny::updateSelectInput(session,
                             "organizationSelect",
                             label    = "Select organization",
                             choices  = choices,
                             selected = selected_org)
#                             selected = character(0))
    enable_tab_UI()
    .dprint("CT_observer1 done...")
  }

  CT_observer2b <- function() {
    .dprint("CT_observer2b launched...")

    disable_tab_UI()
    organization <- input$organizationSelect
    grv$credentials$organization <- organization
    rv$stored_org    <- organization
    rv$selection$org <- organization
    # $stored_org == $selection$org
    rv$state$org_chosen    <- TRUE
    rv$state$org_visible   <- TRUE
    # this is initialisation, so this is ok
    courses <- tmcrstudioaddin::get_all_courses(organization, grv$credentials)
    rv$state$has_courses <- TRUE
    # it is now stored
    grv$coursesInfo$all_courses <- courses
    #
    choices <- courses$id
    names(choices) <- courses$title
    if (is.null(courses$id)) {
      choices <- character(0)
      rv$state$has_courses <- FALSE
    }
    rv$state$course_chosen   <- as.numeric(input$courseSelect) %in% courses$id
    rv$state$course_visible <- rv$state$course_chosen
    rv$selection$course <-
      if (rv$state$course_visible) input$courseSelect else character(0)
    #
    print_rv()
    shiny::updateSelectInput(session,
                             "courseSelect",
                             label    = "Select course",
                             choices  = choices,
                             selected = rv$selection$course)
    if (rv$state$course_chosen) {
      # enable_tab_UI()
      fetch_exercises()
    } else {
      .dprint("NOT FETCHING")
      hideCourseExercises()
      enable_tab_UI()
    }
    .dprint("CT_observer2b done")
  }

  CT_observer3 <- function() {
    .dprint("CT_observer3 launched...")
    disable_tab_UI()
    organization <- input$organizationSelect
    # this is refresh, so this is ok
    courses <- tmcrstudioaddin::get_all_courses(organization, grv$credentials)
    rv$state$has_courses <- TRUE
    choices <- courses$id
    names(choices) <- courses$title
    if (is.null(courses$id)) {
      choices <- character(0)
      rv$state$has_courses <- FALSE
    }
    rv$state$course_chosen   <- as.numeric(input$courseSelect) %in% courses$id
    rv$state$course_visible <- rv$state$course_chosen
    rv$selection$course <-
      if (rv$state$course_visible) input$courseSelect else character(0)
    print_rv()
    shiny::updateSelectInput(session,
                             "courseSelect",
                             label = "Select course",
                             choices = choices,
                             selected = rv$selection$course)
    if (rv$state$course_chosen) {
      # enable_tab_UI()
      fetch_exercises()
    } else {
      .dprint("NOT FETCHING")
      hideCourseExercises()
      enable_tab_UI()
    }
    .dprint("CT_observer3 done")
  }

  fetch_exercises <- function() {
    get_exercises <- function() {
      exercises <- tmcrstudioaddin::get_all_exercises(rv$selection$course, grv$credentials)
      shiny::setProgress(value = 1)
      Sys.sleep(0.5)
      exercises
    }

    .dprint("fetch_exercises()")
    disable_tab_UI()
    hideCourseExercises()
    shiny::withProgress(message = "Fetching exercises",
                        value   = 1/2,
                        { exercises <- get_exercises() })
    score_setup <- sub(pattern = " ",
                       replacement = "",
                       unlist(strsplit(Sys.getenv("TMCR_UNTESTED"),
                                       split = ",")))
    score_set   <- any(grepl(pattern = "score", score_setup))
    if (length(exercises) & score_set ) print_points(exercises)
    separateDownloadedExercises(exercises, NA, globalReactiveValues, input$courseSelect)
    enable_tab_UI()
  }

  num_of_exercises <- function(exercises) {
    nrow(exercises)
  }
  names_of_exercises <- function(exercises) {
    exercises$name
  }
  max_point_width <- function(num) {
    if (num == 0) return(1)
    ceiling(log10(num + 1))
  }
  max_points <- function(exercises) {
    fn <- function(ex) {
      # print(str(ex$available_points$name))
      length(ex$available_points$name)
    }
    points <- apply(exercises, MARGIN = 1, fn)
    # print(str(exercises$available_points))
    #print(points)
    #print(str(sum(points)))
    sum(points)
  }
  max_name_length <- function(exercises) {
    max(nchar(exercises$name))
  }
  print_distribution <- function(awarded_points, available_points) {
    xx <- available_points %in% awarded_points
    # print(xx)
    zz <- sapply(xx + 1, function(x) c(" ", "x")[x])
    n  <- length(zz)
    N  <- 2 * n - 1
    if ( n > 1 ) {
      yy <- character(N)
      yy[seq(1, N, by = 2)] <- zz
      yy[seq(2, N, by = 2)] <- rep("|", n - 1)
    } else {
      yy <- zz
    }
    cat("(")
    cat(paste(yy, collapse = ""))
    cat(")")
  }

  print_num <- function(num, point_width) {
    num_char <- as.character(num)
  #  print(num_char)
    paste(c(rep(" ", point_width - nchar(num_char)),
            num_char),
          collapse = "")
  }
  print_exercise_for <- function(exercise, max_len, point_width, cumulative) {
    padding <- function(len) {
      paste(rep(" ", max_len + 1- len), collapse = "")
    }
    exercise_name    <- exercise$name
    awarded_points   <- exercise$awarded_points[[1]]        # list(), char(0) or char vector
    available_points <- exercise$available_points[[1]]$name # char
    published        <- exercise$unlocked
    points_total     <- length(available_points)
    points           <- length(awarded_points)
    if (points_total > 0 & published) {
      points           <- length(awarded_points)
      cat(exercise_name)
      cat(":",
          padding(nchar(exercise_name)),
          print_num(points, point_width),
          "/",
          print_num(points_total, point_width),
          " ")
      print_distribution(awarded_points, available_points)
      cat("\n")
    } 
    if (points_total > 0 & !published) {
      cat(exercise_name)
      cat(":",
          padding(nchar(exercise_name)),
          print_num("x", point_width),
          "/",
          print_num(points_total, point_width),
          "\n")
    }
    # note, using [..] preserves name and this would not work
    c(points = (cumulative[["points"]] + points),
      total  = (cumulative[["total"]]  + points_total))
    # print(str(exercise))
  }
  print_exercise_apply <- function(exercise) {
    exercise_name    <- exercise$name
    awarded_points   <- exercise$awarded_points        # list(), char(0) or char vector
    available_points <- exercise$available_points$name # char
    published        <- exercise$unlocked
    if (length(available_points) > 0 & published) {
      cat(exercise_name)
      cat(":", length(awarded_points), "/", length(available_points), " ")
      print_distribution(awarded_points, available_points)
      cat("\n")
      # cat("Available points: ", available_points, "\n")
      # cat("Awarded points:   ", awarded_points, "\n")
      # cat("Awarded points:   ", length(awarded_points), "/", length(available_points), "\n")
    } 
    if (length(available_points) > 0 & !published) {
      cat(exercise_name, ": x /", length(available_points), "\n")
    }
    # print(str(exercise))
  }
  course_title <- function() {
    all_courses <- grv$coursesInfo$all_courses
    all_courses$title[all_courses$id == rv$selection$course]
  }
  print_points <- function(exercises) {
    cat(paste0('\033', "[", "3", "2", "m"))
    cat("NOTE: ")
    cat(paste0('\033', "[", "3", "9", "m"))
    cat("This is untested and preliminary version of RTMC score table.\n")
    cat("This might crash, so unset this and report the problem to instructors.\n")
    cat("\n\n")
    cat(course_title(), "\n")
    cat("\n")
    cat("Your current score on the server\n")
    cat("--------------------------------\n")
##     cat("Number of exercises: ", num_of_exercises(exercises), "\n")
##     cat("Names of exercises: ",  names_of_exercises(exercises), "\n")
##     cat("Maximum points: ",  max_points(exercises), "\n")
    max_len <- max_name_length(exercises)
##     cat("max length: ", max_len, "\n")
    cumulative <- c(points = 0, total = 0)
    point_width <- max_point_width(max_points(exercises))
##     cat("point_width: ", point_width, "\n")
    for (i in seq_len(nrow(exercises))) {
      # print("---------------")
      # print(i)
      ex <- exercises[i, ]
      cumulative <- print_exercise_for(ex, max_len, point_width, cumulative)
      # print(cumulative)
    }
    cat("\n")
    cat("Total points:  ")
    cat(paste(rep(" ", max_len + 1 - nchar("Total points")), collapse = ""))
    cat(print_num(cumulative[["points"]], point_width),
        "/", 
        print_num(cumulative[["total"]], point_width),
        "\n\n")

    # using for to add folding
    # apply(X = exercises, MARGIN = 1, FUN = print_exercise_apply)
  }


  CT_observer4 <- function() {
    .dprint("CT_observer4 launched...")
    rv$selection$course <- input$courseSelect
    rv$state$course_chosen   <- TRUE
    rv$state$course_visible <- TRUE
    print_rv()
    fetch_exercises()
    .dprint("CT_observer4 done")
  }

  CT_observer5 <- function() {
    .dprint("CT_observer5 launched...")
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
  }

  CT_observer6 <- function() {
    .dprint("CT_observer6 launched...")
    shinyjs::disable("updateAllExercises")

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
  }
  CT_observer7 <- function() {
    download_exercises <- function() {
      organization <- input$organizationSelect
      # .dprint("getAllCourses site 3")
      courses <- grv$coursesInfo$all_courses
      # courses <- tmcrstudioaddin::get_all_courses(organization, grv$credentials)
      courseName <- courses$name[courses$id == input$courseSelect]

      course_directory_path <- file.path(get_projects_folder(), courseName,
                                         fsep = .Platform$file.sep)

      if (!dir.exists(course_directory_path)) {
        dir.create(course_directory_path, recursive = TRUE)
      }
      .dprint(course_directory_path)
      num_of_downloaded <- downloadFromList(course_directory_path,
                                            globalReactiveValues)
      shiny::setProgress(value = 1)
      Sys.sleep(0.2)
      num_of_downloaded
    }
    .dprint("CT_observer7 launched...")
    disable_tab_UI()

    tryCatch({
      withProgress(message = "Downloading exercises",
                   value   = 0,
                   { num_of_downloaded <- download_exercises() })
      download_success_message <-
        if (num_of_downloaded == 1) {
          paste("You downloaded one exercise successfully", sep = " ")
        } else {
          paste("You downloaded",
                as.character(num_of_downloaded),
                "exercises successfully", sep = " ")
        }
      rstudioapi::showDialog("Success", download_success_message, "")
    },
    error = function(e) {
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
  }

  CT_observer8 <- function() {
    .dprint("CT_observer8 launched...")
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
    .dprint("CT_observer8 done ...")
  }

  CT_observer9 <- function() {
    .dprint("CT_observer9 launched...")
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
    .dprint("CT_observer9 done ...")
  }

  CT_observer10 <- function() {
    .dprint("CT_observer10 launched...")
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
    .dprint("CT_observer10 launched...")
  }

  
  CT_observer11b_logged_out <- function() {
    print_rv()
    .dprint(input$organizationSelect)
    .dprint(grv$credentials$organization)
    .dprint(input$courseSelect)
    if (input$organizationSelect != "" & input$courseSelect != "") {
      .dprint("RELOADING THE STATE, SO BOTH BECOME VISIBLE DURING LOGOUT")
      rv$selection$org <- input$organizationSelect
      rv$state$org_chosen  <- TRUE
      rv$state$org_visible <- TRUE
      grv$credentials$organization <- rv$selection$org
      rv$selection$course <- input$courseSelect
      rv$state$course_chosen   <- TRUE
      rv$state$course_visible <- TRUE
      print_rv()
      return()
    } 
    if (input$organizationSelect != "") {
      print("RELOADING THE STATE, SO ORG BECOMES VISIBLE DURING LOGOUT")
      rv$selection$org <- input$organizationSelect
      rv$state$org_chosen  <- TRUE
      rv$state$org_visible <- TRUE
      grv$credentials$organization <- rv$selection$org
    } else if (!is.null(grv$credentials$organization)) {
      print("ORGANIZATION WAS LOADED... AND WE ARE NOT LOGGED IN")
      print("BUT AT LOG IN THIS IS GLOBBERED... SO THIS HAS TO BE FIXED LATER...")
      print(grv$credentials$organization)
      # rv$selection$org <- grv$credentials$organization
      # rv$state$org_chosen  <- TRUE 
      # the correct ones after fix
      # 
      # temporary ones
      rv$selection$org <- character(0)
      rv$state$org_chosen  <- FALSE
      # 
      rv$state$org_visible <- FALSE
      shiny::updateSelectInput(session,
                               "organizationSelect",
                               label    = "Select organization",
                               choices  = character(0),
                               selected = character(0))
    } else {
      print("NO ORGANIZATION WAS LOADED... AND WE ARE NOT LOGGED IN")
      rv$selection$org <- character(0)
      rv$state$org_chosen  <- FALSE
      rv$state$org_visible <- FALSE
      shiny::updateSelectInput(session,
                               "organizationSelect",
                               label    = "Select organization",
                               choices  = character(0),
                               selected = character(0))
    }
    print_rv()
  }
  CT_observer_meta <- function() {
    .dprint("CT_observer_meta launched..")
    rv$logged_in <- rv$state$logged_in
    .dprint("CT_observer_meta done")
  }

  CT_observer11b <- function() {
    .dprint("CT_observer11b launched...")
    .dprint("This follows logged_in status")
    if (is.null(globalReactiveValues$credentials$token)) {
      CT_observer11b_logged_out()
    } else {
      # .dprint("getAllOrganizations site 1")
      # this is initalisation so this is ok
      organizations  <- tmcrstudioaddin::get_all_organizations(grv$credentials)
      choices        <- organizations$slug
      # ... this is the guard
      if (!is.null(rv$stored_org) & is.null(grv$credentials$organization)) {
        .dprint("Old stored version: restored")
        rv$selection$org <- rv$stored_org
        rv$state$org_chosen  <- TRUE
        rv$state$org_visible <- TRUE
        grv$credentials$organization <- rv$stored_org
        # rv$stored_org     <- grv$credentials$organization
        # this makes the next event launch
        rv$organization_toggle <- !rv$organization_toggle
      } else if (!is.null(grv$credentials$organization)) {
        .dprint("Using grv version: restored")
        rv$selection$org <- grv$credentials$organization
        rv$stored_org    <- rv$selection$org
        rv$state$org_chosen  <- TRUE
        rv$state$org_visible <- TRUE
      } else {
        .dprint("NULL grv")
        rv$selection$org <- character(0)
        rv$stored_org    <- NULL
        rv$state$org_chosen  <- FALSE
        rv$state$org_visible <- FALSE
      }
      print_rv()
      names(choices) <- organizations$name
      selected_org   <- rv$selection$org
      shiny::updateSelectInput(session,
                               "organizationSelect",
                               label = "Select organization",
                               choices = choices,
                               selected = selected_org)
      .dprint(input$courseSelect)
    }
    .dprint("CT_observer11b done")
  }
  print_rv <- function() {
    if (exists(".__tmc_debug")) {
      cat("Printing rv....\n")
      cat("rv$logged_in:", rv$logged_in)
      cat("\n")
      cat("rv$stored_org:", rv$stored_org)
      cat("\n")
      cat("rv$organization_toggle:", rv$organization_toggle)
      cat("\nrv$selection:\n")
      print(rv$selection)
      cat("\nrv$state:\n")
      tmp <- as.logical(rv$state)
      names(tmp) <- names(rv$state)
      print(tmp)
      cat(".... printed\n")
    }
  }
  CT_observer11a <- function() {
    .dprint("CT_observer11a launched...")
    print_rv()
    # for printing
    # rv$selection$state["logged_in"] <- !is.null(grv$credentials$token)
    rv$state$logged_in             <- !is.null(grv$credentials$token)
    .dprint("CT_observer11a done")
  }


  CT_observer12 <- function(tab_UI_list, UI_state) {
    .dprint("CT_observer12 launched...")
    not_logged_in      <- is.null(grv$credentials$token)
    tab_UI_list        <- grv$UI_elements
    grv$UI_state["not_logged_in"] <- not_logged_in
    UI_state           <- grv$UI_state
    tmcrstudioaddin::enable_UI_elements(tab_UI_list, UI_state)
  }

  CT_observer13 <- function() {
    .dprint("CT_observer13 launched...")
    grv$UI_state["not_downloading"] <-
      is.null(input$exercises) &
      is.null(input$downloadedExercises)
    tmcrstudioaddin::enable_UI_elements(grv$UI_elements, grv$UI_state)
    .dprint("CT_observer13 done")
  }


#
# CT_observer initializers
#
##### -------------------
##    the refresh buttons
##### -------------------


  .dprint("CT_observer1...")
  observeEvent(input$refreshOrganizations, CT_observer1())
  .dprint("... initialised")

  .dprint("CT_observer3...")
  observeEvent(input$refreshCourses, CT_observer3(), ignoreInit = TRUE)
  .dprint("... initialised")


##### -------------------
##    the select buttons
##### -------------------

  .dprint("CT_observer5...")
  observeEvent(input$all_exercises, CT_observer5())
  .dprint("... initialised")

  .dprint("CT_observer6...")
  observeEvent(input$updateAllExercises, CT_observer6())
  .dprint("... initialised")

   .dprint("CT_observer8 ...")
   observeEvent(grv$unpublishedExercisesMap, CT_observer8())
   .dprint("... initialised")

   .dprint("CT_observer9 ...")
   observeEvent(grv$downloadedExercisesMap, CT_observer9())
   .dprint("... initialised")

   .dprint("CT_observer10 ...")
   observeEvent(grv$exerciseMap, CT_observer10())
   .dprint("... initialised")


##### -------------------

  .dprint("CT_observer2b ...")
  observeEvent(c(input$organizationSelect, 
                 rv$organization_toggle),
               CT_observer2b(), ignoreInit = TRUE)
  .dprint("... initialised")

  .dprint("CT_observer4...")
  observeEvent(input$courseSelect, CT_observer4(), ignoreInit = TRUE)
  .dprint("... initialised")

  .dprint("CT_observer7...")
  observeEvent(input$download, CT_observer7())
  .dprint("... initialised")

  .dprint("CT_observer11a ...")
  observeEvent(grv$credentials$token, { CT_observer11a() }, ignoreNULL = FALSE)
  .dprint("... initialised")
  .dprint("CT_observer11b ...")
  # ok... having a state vector triggers this event always...
  # so each trigger reactive needs to be separate... not good...
  observeEvent({ rv$logged_in }, { CT_observer11b() }, ignoreInit = TRUE)
  observeEvent({ rv$state },     { CT_observer_meta() })

   .dprint("CT_observer12 ...")
   observeEvent(grv$credentials$token, { CT_observer12() }, ignoreNULL = FALSE)
   .dprint("... initialised")

#### ----- disable/enable observers

  .dprint("CT_observer13 ...")
  observeEvent(c(input$exercises, input$downloadedExercises), ignoreNULL = FALSE,
               { CT_observer13() })
  .dprint("... initialised")

#
# CT_observers
#

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
