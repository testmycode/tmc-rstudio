user_home <- normalizePath("~", winslash = "/")
properties_file <- file.path(user_home, "tmcr", ".properties.rds")
properties_file_backup <- file.path(user_home, "tmcr", ".properties.rds.backup")

# Backups properties file if it exists
backup_properties_file <- function() {
  if (file.exists(properties_file)) {
    file.copy(properties_file, properties_file_backup, overwrite = TRUE)
    file.remove(properties_file)
  }
}

# Restores properties file backup if it exists
restore_properties_file_backup <- function() {
  if (file.exists(properties_file_backup)) {
    file.copy(properties_file_backup, properties_file, overwrite = TRUE)
    file.remove(properties_file_backup)
  }
}
