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

.listener <- function(rx, res_name = NULL, count = 0, env = NULL) {
  cmd_mode <- TRUE
  if (is.null(env)) {
    env1 <- parent.frame()
  } else {
    env1 <- env
  }
  if (!is.null(res_name)) {
    assign(res_name, list(value = NULL, status = "processing"), envir = env1)
    cat("Listener started.",
        paste0("Saving value to '", res_name, "'"),
        sep = "\n")
  }
  listener_env	       <- new.env(parent = emptyenv())
  listener_env$count   <- count
  listener_env$elapsed <- function() {
    .time_str(.run_time_count(listener_env$count))
  }
  listener_env$running <- TRUE
  listener_env$rx      <- rx
  listener_env$port    <- ""
  server_port          <- ""
  listener_init_loop <- function(count, server_port) {
    if (server_port == "" & rx$is_alive()) {
      init_message <- .parse_init(rx$read_output(), cmd_mode)
      output       <- init_message[[1]]
      server_port  <- init_message[[2]]
      count        <- count + 1
      listener_env$count <- count
      init_loop <- function() {
        listener_init_loop(count, server_port)
        42L
      }
      cat(output)
      later::later(init_loop, delay = 0.2)
    } else {
      if (server_port != "") {
        cat("RTMC has started.\n")
        listener_env$port <- server_port
        rstudioapi::viewer(server_port)
      }
      normal_loop <- function() {
        listener_loop(count)
        42L
      }
      later::later(normal_loop, delay = 0.2)
    }
  }
  listener_loop <- function(count) {
    if (rx$is_alive()) {
      output <- .parse_cmd(rx$read_output(), cmd_mode)
      count  <- count + 1
      listener_env$count <- count
      cat(output)
      normal_loop <- function() {
        listener_loop(count)
        42L
      }
      later::later(normal_loop, delay = 0.2)
    } else {
      cat(rx$read_output())
      cat("-------------\n")
      cat("RTMC session has ended.\n")
      listener_env$running <- FALSE
      cat("Elapsed time (approximately):", listener_env$elapsed(), "\n")

      if (!is.null(res_name)) {
        assign(res_name, list(value = rx$get_result(), status = "ready"), envir = env1)
      }
    }
  }
  listener_init_loop(count, server_port)
  listener_env
}

.process_init_matches <- function(output_str, matches) {
  n1 <- nchar("\nListening on ")
  new_output_str <- substr(output_str, start = 1, stop = matches - 1)
  tmp_str <- substr(output_str, start = matches + n1, stop = nchar(output_str))
  listen_end <- regexpr(pattern = "\n", text = tmp_str)[[1]]
  if (listen_end < 0) {
    cat("Listener init crash\n")
    stop("Crash")
  }
  server_port <- substr(tmp_str, start = 1, stop = listen_end - 1)
  output_str <- paste0(new_output_str, substr(tmp_str,
                                              start = listen_end + 1,
                                              stop = nchar(tmp_str)))
  c(output_str, server_port)
}

.parse_init <- function(output_str, cmd_mode) {
  server_port <- ""
  if (cmd_mode & output_str != "") {
    matches <- gregexpr(pattern = "\nListening on http://", text = output_str)[[1]]
    if (matches[1] > 0) {
      parsed_message <- .process_init_matches(output_str, matches)
      output_str  <- parsed_message[1]
      server_port <- parsed_message[2]
    }
  }
  c(output_str, server_port)
}

.parse_cmd <- function(output_str, cmd_mode) {
  if (cmd_mode & output_str != "") {
    matches <- gregexpr(pattern = "\n@@@@ >LISTENER ::: REQ,", text = output_str)[[1]]
    if (matches[1] > 0) {
      # cat("LISTENER> LISTENING REQ.\n")
      output_str <- .process_matches(output_str, matches)
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
    # -----------------
    tmp_str <- substr(tmp_str, start = 5, stop = nchar(tmp_str))
    skip    <- 5 - 1
    cmd_end <- regexpr(pattern = " ", text = tmp_str)[[1]]
    if (cmd_end < 0) {
      stop("Listener crashed 1.")
    }
    cmd_str <- substr(tmp_str, start = 1, stop = cmd_end - 1)
    tmp_str <- substr(tmp_str, start = cmd_end + 1, stop = nchar(tmp_str))
    skip    <- skip + cmd_end
    #
    cmd_args_end <- regexpr(pattern = "\n", text = tmp_str)[[1]]
    if (cmd_args_end < 0) {
      stop("Listener crashed 2.")
    }
    cmd_args <- substr(tmp_str, start = 1, stop = cmd_args_end - 1)
    skip    <- skip + cmd_args_end
    #
    skip_start  <- skip_start + skip
    val <- .process_command(as.integer(num), cmd_str, cmd_args)
    #
  }
  if (skip_start <= n2) {
    new_output_str <- paste0(new_output_str,
                             substr(output_str,
                                    start = skip_start,
                                    stop = n2))
  }
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
  if (num == 1) {
    args_list <- eval(parse(text = cmd_args))
    if (rstudioapi::isAvailable()) {
      rstudioapi::documentSaveAll()
    }
    TRUE
  } else {
    .process_unknown(num, "SAVE", cmd_args)
  }
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
  if (num == 2) {
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

.listener_req_cmd_args <- function(cmd_args_list) {
  if (length(cmd_args_list) == 0) {
    cat("")
  } else if (length(cmd_args_list) == 1) {
    cat(deparse(cmd_args_list[[1]]))
  } else {
    cat("list(")
    cat(do.call(paste, c(lapply(cmd_args_list, FUN = deparse), sep = ", ")))
    cat(")")
  }
}

.send_listener_request <- function(cmd, cmd_args_list) {
  cat("\n")
  cat("@@@@ >LISTENER ::: REQ,")
  cat(length(cmd_args_list) + 1)
  cat(",\n")
  cat(cmd)
  cat(" ")
  .listener_req_cmd_args(cmd_args_list)
  cat("\n")
}
