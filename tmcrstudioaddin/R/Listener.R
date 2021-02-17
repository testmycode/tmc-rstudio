.run_time_count <- function(count) {
  # optimal  <- 5
  observed <- 169 / 36
  optimal  <- observed
  hours    <- floor(count / (optimal * 3600))
  minutes  <- floor(count / (optimal * 60) - hours * 60)
  seconds  <- floor(count / (optimal * 1)  - (hours * 3600 + minutes * 60))
  mseconds <- floor((count - optimal * (hours * 3600 + minutes * 60 + seconds)) * 200)
  c(hours, minutes, seconds, mseconds)
}

.run_time <- function(listener_env) {
  count <- listener_env$count
  .run_time_count(count)
}

.time_str <- function(time_vec) {
  paste(formatC(time_vec[-4], width = 2, flag = "0"), collapse  = ":")
}

.listener <- function(rx, res_name = "val", count = 0, env = NULL) {
  cmd_mode <- TRUE
  if (is.null(env)) {
    env1 <- parent.frame()
  } else {
    env1 <- env
  }
  assign(res_name, list(value = NULL, status = "processing"), envir = env1)
  cat("Listener started.",
      paste0("Saving value to '", res_name, "'"),
      sep = "\n")
  env_listener	       <- new.env(parent = emptyenv())
  env_listener$count   <- count
  env_listener$elapsed <- function() {
    .time_str(.run_time_count(env_listener$count))
  }
  env_listener$running <- TRUE
  env_listener$rx      <- rx
  listener_loop <- function(count) {
    if (rx$is_alive()) {
      output <- .parse_cmd(rx$read_output(), cmd_mode)
      cat(output)
      # if (output == "") {
      #  cat(".")
      # }
      count <- count + 1
      env_listener$count <- count
      later::later(function() {
        listener_loop(count)
        42L
      },
      delay = 0.2)
    } else {
      cat(rx$read_output())
      cat("-------------\n")
      cat("Listener process ended.\n")
      env_listener$running <- FALSE
      cat("Elapsed time (approximately):", count / 5, "sec \n")
      assign(res_name, list(value = rx$get_result(), status = "ready"), envir = env1)
    }
  }
  listener_loop(count)
  env_listener
}

.parse_cmd <- function(output_str, cmd_mode) {
  if (cmd_mode & output_str != "") {
    cat("+")
    matches <- gregexpr(pattern = "\n@@@@ >LISTENER ::: REQ,", text = output_str)[[1]]
    if (matches[1] > 0) {
      # cat("LISTENER> LISTENING REQ.\n")
      output_str <- .process_matches(output_str, matches)
      # cat("\n")
    }
  }
  output_str
}

.process_matches <- function(output_str, matches) {
  n1 <- nchar("\n@@@@ >LISTENER ::: REQ,")
  n2 <- nchar(output_str)
  skip_start <- 1
  new_output_str <- ""
  for (match_idx in seq_along(matches)) {
    new_output_str <- paste0(new_output_str,
                             substr(output_str,
                                    start = skip_start,
                                    stop = matches[match_idx] - 1))
    skip_start <- matches[match_idx] + n1 - 1
    tmp_str <- substr(output_str, start = skip_start, stop = n2)
    num     <- as.integer(substr(tmp_str, start = 2, stop = 2))
    # cat("-----------------\n")
    # cat("Processing match:", matches[match_idx], "\n")
    # cat("Number:", as.integer(num), "\n")
    tmp_str <- substr(tmp_str, start = 5, stop = nchar(tmp_str))
    skip    <- 5 - 1
    cmd_end <- regexpr(pattern = " ", text = tmp_str)[[1]]
    if (cmd_end < 0) {
      stop("Listener crashed 1.")
    }
    cmd_str <- substr(tmp_str, start = 1, stop = cmd_end - 1)
    tmp_str <- substr(tmp_str, start = cmd_end + 1, stop = nchar(tmp_str))
    skip    <- skip + cmd_end
    # cat("Command:", cmd_str, "\n")
    cmd_args_end <- regexpr(pattern = "\n", text = tmp_str)[[1]]
    if (cmd_args_end < 0) {
      stop("Listener crashed 2.")
    }
    cmd_args <- substr(tmp_str, start = 1, stop = cmd_args_end - 1)
    skip    <- skip + cmd_args_end
    # cat("Command args:", cmd_args, "\n")
#     cat("Skipped string:", substr(output_str,
#                                   start = skip_start,
#                                   stop = skip_start + skip - 1),
#         "\n")
    skip_start  <- skip_start + skip
    val <- .process_command(as.integer(num), cmd_str, cmd_args)
#     cat("new_output_str: <", new_output_str, ">new_output_str", sep = "\n")
  }
  # cat("-----------------\n")
#   cat("skip_start:", skip_start, "\n")
#   cat("skip_start:", n2, "\n")
#   cat("missing:", substr(output_str, start = skip_start, stop = n2), "\n")
  if (skip_start <= n2) {
    new_output_str <- paste0(new_output_str,
                             substr(output_str,
                                    start = skip_start,
                                    stop = n2))
  }
#   cat("new_output_str: <", new_output_str, ">new_output_str", sep = "\n")
  new_output_str
}

.process_command <- function(num, cmd, cmd_args) {
  switch(cmd,
         OPEN = .process_OPEN(num, cmd_args),
         SAVE = .process_SAVE(num, cmd_args),
         SHOW = .process_SHOW(num, cmd_args),
         .process_unknown(num, cmd, cmd_args))
}

.process_SAVE <-function(num, cmd_args) {
  .process_unknown(num, "SAVE", cmd_args)
}

.process_SHOW <- function(num, cmd_args) {
  if (num == 4) {
    args_list <- eval(parse(text = cmd_args))
    if (rstudioapi::isAvailable()) {
      do.call(rstudioapi::showDialog, args_list)
    }
    TRUE
  } else {
    .process_unknown(num, "SHOW", cmd_args)
  }
}

.process_OPEN <- function(num, cmd_args) {
  if (num == 2 && substr(cmd_args, 1, 1) == "'") {
    filename <- eval(parse(text = cmd_args))
    cat(paste0("Opening file: ", filename),"\n")
    rstudioapi::isAvailable(rstudioapi::navigateToFile(filename))
    # FIX: should reopen with encoding if it does not match
    # the default on the system
    TRUE
  } else {
    .process_unknown(num, "OPEN", cmd_args)
  }
}

.process_unknown <- function(num, cmd, cmd_args) {
  cat("Received unknown request:",
      "------------------------", sep = "\n")
  cat("Number:", num, "\n")
  cat("Command:", cmd, "\n")
  cat("Command args:", cmd_args, "\n")
  FALSE
}

