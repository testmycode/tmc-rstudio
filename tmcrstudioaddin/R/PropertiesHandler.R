#' @title Create the properties file
#'
#' @description Create the \code{properties.rds} file, which includes
#' the path to the directory where the exercises are downloaded into.
#'
#' @usage create_properties_file(tmcr_projects = "tmcr-projects")
#'
#' @param tmcr_projects Name of the directory where the exercises are
#' downloaded into.  \code{tmcr-projects} by default.
#'
#' @return Always \code{NULL}.
#'
#' @seealso \code{\link{get_tmcr_directory}}, \code{\link[base]{saveRDS}}
create_properties_file <- function(tmcr_projects = "tmcr-projects") {
  tmcr_directory <- tmcrstudioaddin::get_tmcr_directory()
  properties_path <- paste(tmcr_directory,
                           ".properties.rds",
                            sep = .Platform$file.sep)
  properties <- list("tmcr-dir" = paste(tmcrstudioaddin::get_tmcr_directory(),
                                        tmcr_projects,
                                        sep = .Platform$file.sep),
                     "relative" = FALSE)
  saveRDS(properties, properties_path)
}

#' @title Check for the existence of the properties file.
#'
#' @description Check for the existence of the \code{properties.rds}
#' file at the root of the \code{tmcr} directory.
#'
#' @usage check_if_properties_exist()
#'
#' @return \code{TRUE} if the \code{properties.rds} exists at the root
#' of the \code{tmcr} directory, otherwise \code{FALSE}.
#'
#' @seealso \code{\link{get_tmcr_directory}}, \code{\link[base]{file.exists}}
check_if_properties_exist <- function() {
  properties_path <- paste(tmcrstudioaddin::get_tmcr_directory(),
                           ".properties.rds",
                           sep = .Platform$file.sep)
  return(file.exists(properties_path))
}

#' @title Get the tmcr directory path
#'
#' @description Get the file path to the \code{tmcr} directory path.
#'
#' @usage get_tmcr_directory()
#'
#' @details Creates a file path to the \code{tmcr} directory. It is
#' located where the user's \code{HOME} environment variable points to:
#' by default to the \code{home} directory on Linux and
#' \code{user/documents} on Windows. If the \code{tmcr} directory does
#' not exist at the file path then it is created to be at that location.
#'
#' @return File path to the location of the \code{tmcr} directory
#'
#' @seealso \code{\link[base]{normalizePath}},
#' \code{\link[base]{file.path}}, \code{\link[base]{dir.exists}}
get_tmcr_directory <- function() {
  user_home <- normalizePath("~", winslash = "/")
  tmcr_directory <- file.path(user_home, "tmcr")

  if (!dir.exists(tmcr_directory)) {
    dir.create(tmcr_directory)
  }
  return(tmcr_directory)
}

#' @title Get the TMC R-project directory path
#'
#' @description Get the file path to the \code{tmcr-projects} folder,
#' which contains the TMC R exercise projects.
#'
#' @usage get_projects_folder()
#'
#' @details TODO: Do this after PropertiesHandler.R refactoring
get_projects_folder <- function() {
  properties_list <- tmcrstudioaddin::read_properties()
  if (length(properties_list$`tmcr-dir`[1]) == 0) {
    return(paste(tmcrstudioaddin::get_tmcr_directory(),
                 "tmcr-projects",
                 sep = .Platform$file.sep))
  } else {
    return(properties_list$`tmcr-dir`[1])
  }
}

#' @title Read the properties file
#'
#' @description Read and return the data from \code{properties.rds}.
#'
#' @usage read_properties()
#'
#' @details Reads the data from \code{properties.rds} and returns it. If
#' the \code{properites.rds} file does not exist at the \code{tmcr}
#' directory, it is created there.
#'
#' @return An R object created from the data of \code{properties.rds}.
#'
#' @seealso \code{\link{check_if_properties_exist}},
#' \code{\link{create_properties_file}},
#' \code{\link{get_tmcr_directory}}, \code{\link[base]{readRDS}}
read_properties <- function() {
  if (!check_if_properties_exist()) {
    create_properties_file()
  }

  properties_path <- paste(tmcrstudioaddin::get_tmcr_directory(),
                           ".properties.rds",
                           sep = .Platform$file.sep)
  return(readRDS(properties_path))
}
