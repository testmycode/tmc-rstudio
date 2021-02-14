.listener <- function(rx, res_name = "val", count = 0, env = NULL) {
  if (is.null(env)) {
    env1 <- parent.frame()
  } else {
    env1 <- env
  }
  assign(res_name, list(value = NULL, status = "processing"), envir = env1)
  cat("Listener started.",
      paste0("Saving value to '", res_name, "'"),
      sep = "\n")
  listener_loop <- function(count) {
    if (rx$is_alive()) {
      output <- rx$read_output()
      cat(output)
      # if (output == "") {
      #  cat(".")
      # }
      later::later(function() {
        listener_loop(count + 1)
        42L
      },
      delay = 0.2)
    } else {
      cat(rx$read_output())
      cat("-------------\n")
      cat("Listener process ended.\n")
      cat("Elapsed time (approximately):", count / 5, "sec \n")
      assign(res_name, list(value = rx$get_result(), status = "ready"), envir = env1)
    }
  }
  listener_loop(count)
}

