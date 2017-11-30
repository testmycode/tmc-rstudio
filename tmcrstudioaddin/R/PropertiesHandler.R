create_properties_file <- function(tmcr_projects = "tmcr-projects",
    relative_to_tmcr = TRUE) {

  tmcr_directory <- get_tmcr_directory()
  properties_path <- paste(tmcr_directory, ".properties.rds",
                            sep = .Platform$file.sep)

  if(relative_to_tmcr) {
    properties <- list("tmcr-dir" = tmcr_projects, "relative" = TRUE)
  }
  saveRDS(properties, properties_path)
}

check_if_properties_exist <- function() {
  properties_path <- paste(get_tmcr_directory(), ".properties.rds",
                            sep = .Platform$file.sep)

  return(file.exists(properties_path))
}


get_tmcr_directory <- function() {
  user_home <- Sys.getenv("HOME")
  tmcr_directory <- file.path(user_home, "tmcr")

  if(!dir.exists(tmcr_directory)) {
    dir.create(tmcr_directory)
  }

  return(tmcr_directory)
}

get_projects_folder <- function() {
  properties_list <- read_properties()
  if (length(properties_list$`tmc_dir`) == 0) {
    return(paste(get_tmcr_directory(), "tmcr-projects", sep = .Platform$file.sep))
  } else {
    return(paste(get_tmcr_directory(), properties_list$`tmc_dir`,
        sep = .Platform$file.sep))
  }

}

read_properties <- function() {
  properties_path <- paste(get_tmcr_directory(), ".properties.rds",
        sep = .Platform$file.sep)
  return(readRDS(properties_path))
}
