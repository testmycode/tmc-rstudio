create_properties_file <- function(tmcr_projects = "tmcr-projects") {

  tmcr_directory <- tmcrstudioaddin::get_tmcr_directory()
  properties_path <- paste(tmcr_directory, ".properties.rds",
                            sep = .Platform$file.sep)

  properties <- list("tmcr-dir" = paste(tmcrstudioaddin::get_tmcr_directory(),
          tmcr_projects, sep = .Platform$file.sep), "relative" = FALSE)

  saveRDS(properties, properties_path)
}

check_if_properties_exist <- function() {
  properties_path <- paste(tmcrstudioaddin::get_tmcr_directory(),
                        ".properties.rds", sep = .Platform$file.sep)

  return(file.exists(properties_path))
}


get_tmcr_directory <- function() {
  user_home <- normalizePath("~", winslash = "/")
  tmcr_directory <- file.path(user_home, "tmcr")

  if (!dir.exists(tmcr_directory)) {
    dir.create(tmcr_directory)
  }

  return(tmcr_directory)
}

get_projects_folder <- function() {
  properties_list <- tmcrstudioaddin::read_properties()
  if (length(properties_list$`tmcr-dir`[1]) == 0) {
    return(paste(tmcrstudioaddin::get_tmcr_directory(),
      "tmcr-projects", sep = .Platform$file.sep))
  } else {
    return(properties_list$`tmcr-dir`[1])
  }

}

read_properties <- function() {
  if (!check_if_properties_exist()) {
    create_properties_file()
  }

  properties_path <- paste(tmcrstudioaddin::get_tmcr_directory(),
      ".properties.rds", sep = .Platform$file.sep)
  return(readRDS(properties_path))
}
