#
# public (all need work)
#
# download_exercise
# get_json_from_submission_url      # ok
# upload_exercise                   # ok
# get_submission_json               # ok
# get_all_organizations             # mostly ok (refactor)
# get_all_courses                   # mostly ok
# get_all_exercises                 # mostly ok

#
# private
#
# .simplify_error_message

  .randomize_name  <- function(body, tmp_dir) {
    zip_name <- ""
    repeat {
      zip_name <- paste0(paste0(sample(c(0:9, letters[1:6]),
                                       size    = 12,
                                       replace = TRUE),
                                collapse = ""),
                         "_",
                         body)
      full_name <- paste0(tmp_dir, "/", zip_name)
      .ddprint("RANDOMIZE")
      .ddprint(zip_name)
      .ddprint(full_name)
      if (!file.exists(paste0(tmp_dir, "/", zip_name))) break
    }
    zip_name
  }

#' @title Download an exercise from the TMC server
#'
#' @description Download an exercise from the TMC server.
#'
#' @usage download_exercise(exercise_id,
#'                          zip_target = getwd(),
#'                          zip_name = "temp.zip",
#'                          exercise_directory,
#'                          exercise_name,
#'                          credentials,
#'                          unique_random = FALSE)
#'
#' @param exercise_id ID of the exercise.
#' @param zip_target Path to where the temporary \code{zip} file is
#' stored. Defaults to the current working directory.
#' @param zip_name Name of the temporary \code{zip} file. Default is
#' \code{temp.zip}.
#' @param exercise_directory Path to the directory where the exercise
#' directory is unzipped to.
#' @param exercise_name Name of the downloaded exercise.
#' @param credentials List of user credentials.
#' @param unique_random logical: should the name of the file be unique
#' and random? Default is FALSE.
#'
#' @details Downloads an zipped exercise matching the given id from the TMC
#' server, unzips it to the given directory and creates a \code{JSON}
#' file containing the exercise name and id on the exercise directory.
#'
#' @return \code{HTTP} response to the download attempt.
#'
#' @seealso \code{\link{getCredentials}}, \code{\link[httr]{GET}},
#' \code{\link[httr]{add_headers}}, \code{\link[httr]{write_disk}},
#' \code{\link[base]{file.remove}}, \code{\link{create_exercise_metadata}}

# Exercise_id is the identifier of the exercise. For example, 36463.
# Target is the place where zip-file is stored, if it's not deleted.
download_exercise <- function(exercise_id,
                              zip_target = getwd(),
                              zip_name = "temp.zip",
                              exercise_directory,
                              exercise_name,
                              credentials,
                              unique_random = FALSE) {
  .dprint("download_exercise()")
  token <- credentials$token

  if (unique_random) {
    zip_name <- .randomize_name(zip_name, zip_target)
  }
  zip_path <- paste(sep = "", zip_target, "/", zip_name)
  .ddprint(str(zip_path))

  exercises_url <- paste(credentials$serverAddress,
                         "/", "api/v8/core/exercises/",
                         exercise_id, "/", "download",
                         sep = "")
  exercises_response <- httr::GET(exercises_url,
                                  httr::add_headers(Authorization = token),
                                  config = timeout(10),
                                  write_disk(zip_path, overwrite = FALSE))

  .dprint("exercises_response")
  .ddprint(str(exercises_response))

  # move this to better location
  exercise_forbidden_num <- 403
  # exercise_ok_num value would be 200
  # move this to better location
  if (exercises_response$status_code == exercise_forbidden_num) {
    file.remove(zip_path)
    stop(paste("Forbidden (HTTP ", as.character(exercise_forbidden_num),
               ")",
               sep = ""))
  }
  .tmc_unzip(zipfile_name = zip_path, target_folder = exercise_directory)
  file.remove(zip_path)

  create_exercise_metadata(exercise_id, exercise_directory, exercise_name)

  return(exercises_response)
}

  .upload_exercise1 <- function(token, exercise_id, server_address) {
    exercises_url      <- paste0(server_address,
                                 "api/v8/core/exercises/",
                                 exercise_id,
                                 "/",
                                 "submissions")
    url_config         <- httr::add_headers(Authorization = token)
    list(exercises_url      = exercises_url,
         url_config         = url_config)
  }

  .create_submission_zip_file <- function(project_path) {
    zip_path <- paste0(tempfile(), ".zip")
    tryCatch({
      .tmc_zip(project_path, zip_path)
      # TODO. check .tmc_zip is working properly
    }, error = function(err) {
      cat("Creating submission failed.\n")
      stop(err)
    }, warning = function(warn) {
      cat("Creating submission failed with warning.\n")
    })
    return(zip_path)
  }

    .prepare_httr_upload_content <- function(zip_path) {
      cat("Sending submission package to server...\n")
      if (!is.null(shiny::getDefaultReactiveDomain())) {
        shiny::setProgress(message = "Sending submission package",
                           value = 1/4)
      }
      submission_file <- httr::upload_file(zip_path)
      return(submission_file)
    }

    .upload_content_to_server <- function(url_data, submission_file) {
      exercises_url      <- url_data$exercises_url
      url_config         <- url_data$url_config
      post_body          <- list("submission[file]" = submission_file)
      tryCatch({
        response <- httr::POST(url      = exercises_url,
                               config   = url_config,
                               encode   = "multipart",
                               body     = post_body)
        list(data = response)
      }, error = function(err) {
        cat("Upload content uploading failed.\n")
        stop(err)
      })
    }

    .check_upload_status <- function(response) {
      tryCatch({
        response$data <- httr::stop_for_status(response$data)
        response
      },
      error = function(err) {
        cat("Upload was not accepted by server.\n")
        response$error <- err
        return(response)
      })
    }

    .clean_up_submission <- function(zip_path, remove_zip) {
      if (remove_zip) {
        cat("Cleaning up submission zip package.\n")
        file.remove(zip_path)
      }
    }

  .upload_with_zip_package <- function(zip_path, url_data) {
    submission_file     <- .prepare_httr_upload_content(zip_path)
    exercises_response  <- .upload_content_to_server(url_data, submission_file)
    .check_upload_status(exercises_response)
  }

  .upload_exercise2 <- function(url_data, project_path, remove_zip) {
    zip_path            <- .create_submission_zip_file(project_path)
    tryCatch({
      exercises_response <- .upload_with_zip_package(zip_path, url_data)
      .clean_up_submission(zip_path, remove_zip)
    },
    error = function(err) {
      .clean_up_submission(zip_path, remove_zip)
      stop(err)
    })
    return(exercises_response)
  }


#' @title Upload an exercise to the TMC server
#'
#' @description Upload an exercise to the TMC server
#'
#' @usage upload_exercise(token, exercise_id, project_path, server_address,
#'         zip_name = "temp", remove_zip = TRUE)
#'
#' @param token \code{OAuth2} token associated with the current login
#' session.
#' @param exercise_id ID of the submitted exercise.
#' @param project_path File path to the directory of the submitted
#' exercise.
#' @param server_address Address of the TMC server where the exercise is
#' submitted to.
#' @param zip_name Name of the \code{zip} file which contains the
#' exercise submission. Default is \code{temp}
#' @param remove_zip \code{TRUE} or \code{FALSE} depending on if you
#' wish to delete the submission \code{zip} file after sending it to the
#' server. Defaults to \code{TRUE}.
#'
#' @details Packs the exercise directory into a \code{zip} file and
#' sends it to the TMC server as a list.
#'
#' @return \code{HTTP} response to the submission attempt.
#'
#' @seealso \code{\link[httr]{add_headers}},
#' \code{\link[httr]{upload_file}}, \code{\link[httr]{POST}},
#' \code{\link[base]{file.remove}}

# Zips and uploads a single exercise, which is located in project_path.
# Returns the response, which contains a field $submission_url
# containing the details of the submission.
upload_exercise <- function(token, exercise_id, project_path,
                            server_address, zip_name = "temp",
                            remove_zip = TRUE) {
  url_data <- .upload_exercise1(token, exercise_id, server_address)
  .upload_exercise2(url_data, project_path, remove_zip)
}

#' @title Get exercise submission result JSON
#'
#' @description Get the exercise submission result \code{JSON} file from
#' the TMC server.
#'
#' @usage get_submission_json(token, url)
#'
#' @param token \code{OAuth2} token associated with the the current
#' login session to the TMC server.
#' @param url URL where the exercise submission \code{JSON} is fetched from.
#'
#' @details Makes a \code{HTTP-GET} request to fetch the exercise
#' submission result \code{JSON} from the specified URL.
#'
#' @return \code{HTTP} response from the TMC server which contains the
#' submission result \code{JSON} if the server has finished processing
#' the exercise submission.
#'
#' @seealso \code{\link[httr]{add_headers}}, \code{\link[httr]{GET}}

# Returns details of the submission in url
get_submission_json <- function(token, url) {
  url_config <- httr::add_headers(Authorization = token)

  exercises_response <- httr::GET(url, config = url_config, timeout(5))

  return(exercises_response)
}

#' @title Get all TMC organizations
#'
#' @description Get all TMC organizations.
#'
#' @usage get_all_organizations(credentials)
#'
#' @param credentials List of user credentials.
#'
#' @details Reads the \code{OAuth2} token and server address from
#' \code{credentials} and uses them to make a \code{HTTP-GET}
#' request for the list of organizations.
#'
#' @return List of TMC organization names and slugs. If
#' \code{credentials} are not proper or sending the \code{HTTP-GET} request
#' failed, returns a list with 2 empty sublists called \code{name}
#' and \code{slug}.
#'
#' @seealso \code{\link{getCredentials}},
#' \code{\link[httr]{stop_for_status}}, \code{\link[httr]{add_headers}},
#' \code{\link[jsonlite]{fromJSON}}
get_all_organizations <- function(credentials) {
  .do_with_progress <- function() {
    url     <- paste(credentials$serverAddress, "/api/v8/org.json", sep = "")
    token   <- credentials$token
    headers <- httr::add_headers(Authorization = token)
    req     <-
      httr::stop_for_status(httr::GET(url    = url,
                                      headers,
                                      config = httr::timeout(10),
                                      encode = "json"))
    Sys.sleep(0.1)
    cat("connected.\n")
    shiny::setProgress(message = "Connected to server",
                       value = 0.9)
    Sys.sleep(0.2)
    return_value <- jsonlite::fromJSON(httr::content(req, "text"))
    shiny::setProgress(value = 1)
    Sys.sleep(0.2)
    return_value
  }
  organizations <- tryCatch({
    cat("Conneting to server...\t")
    shiny::withProgress(message = "Connecting to server",
                        value   = 1/2,
                        .do_with_progress())
  }, error = function(e) {
    cat("An error occured while connecting to server:\n")
    cat(.simplify_error_message(e$message))
    cat("\n")
    list(name = list(), slug = list())
  })
  .dprint("get_all_organizations done...")
  return(organizations)
}

#' @title Get all courses offered by a TMC organization
#'
#' @description Get all courses offered by a TMC organization.
#'
#' @usage get_all_courses(organization, credentials)
#'
#' @param organization Organization slug (Identifying URL-friendly name).
#'
#' @param credentials List of user credentials.
#'
#' @details Reads the \code{OAuth2} token and server address from
#' \code{.credentials.json} and uses them to make a \code{HTTP-GET}
#' request for the list of courses belonging to the organization,
#' given the user has proper credentials.
#'
#' @return List of course names and course ids belonging to the given
#' organization.  If \code{credentials} are not proper or sending the
#' \code{HTTP-GET} request failed, returns a list with 2 empty sublists
#' called \code{id} and \code{name}.
#'
#' @seealso \code{\link{getCredentials}},
#' \code{\link[httr]{stop_for_status}}, \code{\link[jsonlite]{fromJSON}}
get_all_courses <- function(organization, credentials) {
  # no access to globalReactiveValues
  .do_with_progress <- function() {
    url     <- paste(credentials$serverAddress, "/api/v8/core/org/",
                     organization, "/courses",
                     sep = "")
    token   <- credentials$token
    headers <- httr::add_headers(Authorization = token)
    req     <-
      httr::stop_for_status(httr::GET(url    = url,
                                      config = headers,
                                      encode = "json",
                                      timeout(10)))
    Sys.sleep(0.1)
    cat("getting courses.\n")
    shiny::setProgress(message = "Getting courses",
                       value = 0.9)
    Sys.sleep(0.2)
    return_value <- jsonlite::fromJSON(httr::content(req, "text"))
    shiny::setProgress(value = 1)
    Sys.sleep(0.2)
    return_value
  }
  courses <- tryCatch({
    cat("Conneting to server...\t")
    shiny::withProgress(message = "Connecting to server",
                        value   = 1/2,
                        .do_with_progress())
  }, error = function(e) {
    cat("An error occured while connecting to server.\n")
    cat(.simplify_error_message(e$message))
    cat("\n")
    list(id = list(), name = list(), title = list())
  })
  .dprint("get_all_courses done...")
  return(list(id = courses$id, name = courses$name, title = courses$title))
}

#' @title Get all exercises of a TMC course
#'
#' @description Get all exercises of a TMC course.
#'
#' @usage get_all_exercises(course, credentials)
#'
#' @param course ID of the course.
#'
#' @param credentials List of user credentials.
#'
#' @details Reads the \code{OAuth2} token and server address from
#' \code{credentials} and uses them to make a \code{HTTP-GET}
#' request for the list of exercises in the course.
#'
#' @return List of exercises in the course. If
#' \code{credentials} are not proper or sending the \code{HTTP-GET} request
#' failed, returns an empty list.
#'
#' @seealso \code{\link{getCredentials}},
#' \code{\link[httr]{stop_for_status}}, \code{\link[jsonlite]{fromJSON}}
get_all_exercises <- function(course, credentials) {
  .do_without_progress <- function() {
    # shiny::withProgress is done at the call site
    #
    url     <- paste(credentials$serverAddress, "/api/v8/courses/",
                     course, "/exercises", sep = "")
    token   <- credentials$token
    headers <- httr::add_headers(Authorization = token)
    req     <-
      httr::stop_for_status(httr::GET(url = url,
                                      headers,
                                      config = httr::timeout(10),
                                      encode = "json"))
    jsonlite::fromJSON(httr::content(req, "text"))
  }
  exercises <- tryCatch(.do_without_progress(),
                        error = function(e) {
                          cat("An error occured while connecting to server.\n")
                          cat(.simplify_error_message(e$message))
                          list()
                        })
  exercises
}

.simplify_error_message <- function(msg) {
  standard_reply <- paste("Most likely you don't have working connection,",
                          "something",
                          "is blocking your access or the server is down.",
                          "Check connection and try again.")
  translations <- c("LibreSSL SSL_read: SSL_ERROR_SYSCALL, errno 60",
                    standard_reply,
                    "Could not resolve host: tmc.mooc.fi",
                    standard_reply,
                    "Couldn't connect to server",
                    standard_reply)


  translation_df <-
    as.data.frame(
      stringsAsFactors = FALSE,
      matrix(byrow = TRUE,
             ncol = 2,
             c(translations,
               msg, msg)))
  names(translation_df) <- c("key", "translation")
  val <- translation_df$translation[translation_df$key == msg]
  if (length(val) > 1) {
    paste0(val[1], " (", val[2], ")")
  } else {
    val
  }
}

#' @title Get exercise submission result JSON
#'
#' @description Get exercise submission result \code{JSON} from the TMC
#' server.
#'
#' @usage get_json_from_submission_url(response, token)
#'
#' @param response \code{HTTP} response to the exercise submission.
#' @param token \code{OAuth2} token associated with the current login
#' session to the TMC server.
#'
#' @details Extracts the exercise submission result url from the given
#' response and makes an \code{HTTP-GET} request for the exercise
#' submission result \code{JSON}.
#'
#' @return \code{HTTP} response as a list from the TMC server containing
#' the submission result \code{JSON} if the server has finished
#' processing the exercise submission. List containing \code{error} key
#' with an error message if the \code{HTTP-GET} request failed.
#'
#' @seealso \code{\link[httr]{content}}, \code{\link{get_submission_json}}
get_json_from_submission_url <- function(response, token) {
  submit_json <- list()
  url <- list()
  url$error <- NULL
  submit_json <- tryCatch({
    url <- httr::content(response)
    submit_json$results <-
      httr::content(get_submission_json(token, url$submission_url))
    .dprint(str(submit_json))
    submit_json
  }, error = function(e) {
    if (!is.null(url$error)) {
      .dprint("Case1")
      .dprint(str(url$error))
      submit_json$error <- url$error
    }
    .dprint("Case2")
    .dprint(str(e))
    submit_json$results$error <- e
    if (grepl("Timeout was reached", e$message)) {
      submit_json$results$status <- "timeout"
    } else {
      submit_json$results$status <- "error"
    }
    .dprint(str(submit_json))
    submit_json
  })
  return(submit_json)
}
