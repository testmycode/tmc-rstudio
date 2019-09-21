.tmc_zip <- function(folder, zip_path) {
  .dprint(".zip")
  oldWd <- getwd()
  setwd(folder)
  tryCatch({
    files_to_zip <- dir(folder, recursive = TRUE, include.dirs = TRUE)
    zip(zipfile = zip_path, files = files_to_zip);
  }, error = function(e) {
    print("Could not Zip")
    setwd(oldWd)
    stop(e)
  })
  setwd(oldWd)
}

.tmc_unzip <- function(zipfile_name, target_folder) {
  unzip(zipfile = zipfile_name, exdir = target_folder)
}

.tmc_tar <- function(folder, target) {
  files_to_tar <- dir(folder, full.names = TRUE)
  tar(tarfile = paste(sep = "", target, ".tar"), files = files_to_tar);
}

.tmc_untar <- function(tarfile_name, target_folder) {
    untar(tarfile = tarfile_name, exdir = target_folder)
}
