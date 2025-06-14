.zip_warn <- function(old_wd, zip_path, files_to_zip) {
  function(e) {
    cat("Could not zip using normal zip functionality.\n")
    if (.Platform$OS.type == "windows") {
      cat("This is Windows, trying another method for zip\n")
      tryCatch({
        system2("tar", c("acf", zip_path, files_to_zip), stdout = TRUE)
      }, warning = function(e2) {
        cat("Still could not zip due to a warning. Giving up.\n")
      }, error   = function(e2) {
        cat("Still could not zip due to an error. Giving up.\n")
      })
    }
    setwd(old_wd)
  }
}
.zip_error <- function(old_wd) {
  function(e) {
    cat("Could not zip due to an error\n")
    setwd(old_wd)
    stop(e)
  }
}
.tmc_zip <- function(folder, zip_path) {
  .dprint(".zip")
  old_wd <- getwd()
  setwd(folder)
  if (file.exists(zip_path)) {
    cat("Zip file was already created. This should not happen.\n")
    setwd(old_wd)
    stop("Zip file already exists")
  }
  tryCatch({
    files_to_zip <- dir(folder, recursive = TRUE, include.dirs = TRUE)
    zip(zipfile = zip_path, files = files_to_zip, flags = "-q")
  }, warning = .zip_warn(old_wd, zip_path, files_to_zip),
     error   = .zip_error(old_wd))
  setwd(old_wd)
  if (!file.exists(zip_path)) {
    cat("Zip has not not been created.\n")
    stop("Zip file not created")
  }
}

.tmc_unzip <- function(zipfile_name, target_folder) {
  unzip(zipfile = zipfile_name, exdir = target_folder)
}

.tmc_tar <- function(folder, target) {
  files_to_tar <- dir(folder, full.names = TRUE)
  tar(tarfile = paste(sep = "", target, ".tar"),
      files = files_to_tar);
}

.tmc_untar <- function(tarfile_name, target_folder) {
    untar(tarfile = tarfile_name, exdir = target_folder)
}
