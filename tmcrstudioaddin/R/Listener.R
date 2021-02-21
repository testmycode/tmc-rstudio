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
  listener_env         <- new.env(parent = emptyenv())
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
      } else {
        cat("RTMC failed to start. This happens sometimes. Just try again.\n")
      }
      normal_loop <- function() {
        listener_loop(count,
                      list(cmd_mode = cmd_mode, unlock_code = ""),
                      "")
        42L
      }
      later::later(normal_loop, delay = 0.2)
    }
  }
  listener_loop <- function(count, lock_data, unhandled) {
    if (rx$is_alive()) {
      raw_output  <- paste0(unhandled, rx$read_output())
      output_data <- .parse_cmd(raw_output, lock_data)
#       if (output_data$unhandled != "") {
#         .dcat("unhandled", output_data$unhandled)
#       }
      lock_data   <- .handle_locking_and_printing(output_data, lock_data)
      count       <- count + 1
      listener_env$count <- count
      normal_loop <- function() {
        listener_loop(count, lock_data, output_data$unhandled)
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

.handle_locking_and_printing <- function(output_data, lock_data) {
  output      <- output_data$output
  lock_code   <- output_data$unlocking_data[[1]]
  if (lock_code[1] != "") {
    if (lock_data$cmd_mode) {
      lock_data <- list(cmd_mode = FALSE, unlock_code = lock_code) #lock_data$unlock_code)
#       .dcat("LOCKING NOW", lock_data)
#       .dcat("lock_code", lock_code)
      cat(output)
    } else {
      lock_data <- .try_unlocking(output_data, lock_data)
    }
  } else {
    cat(output)
  }
  lock_data
}

.try_unlocking <- function(output_data, lock_data) {
  unlock_code <- lock_data$unlock_code
  cmd_mode    <- lock_data$cmd_mode
  output      <- output_data$output
  lock_codes  <- output_data$unlocking_data[[1]]
  matches     <- output_data$unlocking_data[[2]]
#   cat("\n.try_unlocking\n----------\n")
#   .dcat("unlock_code", unlock_code)
#   .dcat("cmd_mode", cmd_mode)
#   .dcat("output", output)
#   .dcat("lock_codes", lock_codes)
#   .dcat("matches", matches)
#   cat("BUT NOW WE SHOULD BE SKIPPING THIS...WE DON'T USE THIS ANYMORE\n")
  if (lock_codes[1] == "unlock") {
    # cat("FINALLY UNLOCKING!!!!!")
    cmd_mode <- TRUE
    unlock_code <- ""
  } else {
    cat("")
    # cat("Not unlocking\n")
  }
#   for (ind in seq_along(lock_codes)) {
#     lock_code <- lock_codes[ind]
#     if (lock_code == unlock_code) {
#       cmd_mode <- TRUE
#       unlock_code <- ""
#     } else {
#       match1 <- matches[ind]
#       stop("BUT NOW WE ARE NERE!")
#       output <- paste0(substr(output, start = 1, stop = match1 - 1),
#                        "\n@@@@ >LISTENER ::: UNLOCK,",
#                        lock_code,
#                        "\n",
#                        substr(output, start = match1, stop = nchar(output)))
#     }
#   }
  lock_data <- list(cmd_mode = cmd_mode, unlock_code = unlock_code)
  cat(output)
  lock_data
}

.process_unlock_matches <- function(output_str, matches, unlock_code) {
  n1 <- nchar("\n@@@@ >LISTENER ::: UNLOCK,")
#   .dcat("UNLOCKING START", unlock_code)
#   .dcat("matches", matches)
#   .dcat("output_str", output_str)
  new_output_str <- ""
  unlock_codes <- c()
  skip <- 1
  n2 <- nchar(output_str)
  for (match1 in matches) {
    # .dcat("new_output_str before", new_output_str)
    new_output_str <- paste0(new_output_str, substr(output_str, start = skip, stop = match1 - 1))
    # .dcat("new_output_str after", new_output_str)
    tmp_str <- substr(output_str, start = match1 + n1, stop = nchar(output_str))
    unlock_end <- regexpr(pattern = "\n", text = tmp_str)[[1]]
    if (unlock_end < 0) {
#       cat("\nABOUT TO CRASH\n--------\n")
#       .dcat("output_str", output_str)
#       .dcat("new_output_str", new_output_str)
#       .dcat("tmp_str", tmp_str)
#       .dcat("unlock_end", unlock_end)
      cat("Listener unlocking partial match crash fixed.\n")
      unhandled <- .remaining_part(output_str, match1)
#       .dcat("unhandled", unhandled)
      return(list(output = new_output_str, unlocking_data = list("lock", -1),
                  unhandled = unhandled))
    }
    attempted_unlock_code <- substr(tmp_str, start = 1, stop = unlock_end - 1)
    # .dcat("lock_code", attempted_unlock_code)
    # .dcat("unlock_code", unlock_code)
    if (attempted_unlock_code == unlock_code) {
      # .dcat("new_output_str", new_output_str)
      unhandled <- .remaining_part(tmp_str, unlock_end + 1)
      # cat("NOW WE UNLOCK...\n")
      return(list(output = new_output_str, unlocking_data = list("unlock", -1),
                  unhandled = unhandled))
    } else {
      # cat("This is no match\n")
      unlock_codes <- c(unlock_codes, "")
      skip <- match1 # + n1 + unlock_end
    }
  }
  # .dcat("original", output_str)
  # .dcat("current", new_output_str)
  # .dcat("skip", skip)
  # cat("FOUND NO MATCHING UNLOCKS, JUST RETURNING ORIGINAL\n")
#   output_str <- paste0(new_output_str, substr(tmp_str,
#                                               start = unlock_end + 1,
#                                               stop = nchar(tmp_str)))
  list(output = output_str, unlocking_data = list("lock", -1),
       unhandled = "")
}

.process_lock_matches <- function(output_str, matches) {
  n1 <- nchar("\n@@@@ >LISTENER ::: LOCK,")
  n2 <- nchar(output_str)
#   .dcat("matches", matches)
  match1 <- matches[1]
#   .dcat("match1", match1)
  new_output_str <- substr(output_str, start = 1, stop = match1 - 1)
  tmp_str <- substr(output_str, start = match1 + n1, stop = nchar(output_str))
  lock_end <- regexpr(pattern = "\n", text = tmp_str)[[1]]
  if (lock_end < 0) {
#     cat("\nABOUT TO CRASH\n--------\n")
#     .dcat("output_str", output_str)
#     .dcat("new_output_str", new_output_str)
#     .dcat("tmp_str", tmp_str)
#     .dcat("lock_end", lock_end)
#     cat("Listener locking crash\n")
    cat("Listener locking partial match crash fixed!\n")
    unhandled <- .remaining_part(output_str, match1)
    return(list(output = new_output_str, unlocking_data = list("", -1),
                unhandled = unhandled))
  }
  lock_code <- substr(tmp_str, start = 1, stop = lock_end - 1)
#   output_str <- paste0(new_output_str, substr(tmp_str,
#                                               start = lock_end + 1,
#                                               stop = nchar(tmp_str)))
  list(output         = new_output_str,
       unlocking_data = list(lock_code, match1),
       unhandled      = substr(tmp_str, start = lock_end + 1, stop = nchar(tmp_str)))
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

.remaining_part <- function(output_str, rest_start) {
  substr(output_str, start = rest_start, stop = nchar(output_str))
}

.parse_cmd <- function(output_str, lock_data) {
  cmd_mode    <- lock_data$cmd_mode
  unlock_code <- lock_data$unlock_code
  unlock_data <- list("", -1)
  output_data <- list(output         = output_str,
                      unlocking_data = unlock_data,
                      unhandled      = "")
  if (output_str != "") {
    if (cmd_mode) {
      lock_matches <- gregexpr(pattern = "\n@@@@ >LISTENER ::: LOCK,", text = output_str)[[1]]
      req_matches  <- gregexpr(pattern = "\n@@@@ >LISTENER ::: REQ,", text = output_str)[[1]]
      if (lock_matches[1] > 0 & req_matches[1] > 0) {
        if (lock_matches[1] < req_matches[1]) {
          output_data <- .process_lock_matches(output_str, lock_matches)
          cat("\nSimultaneous req/lock with lock before\n---------\n")
          # .dcat("output_data", output_data)
          # stop("Simultaneous req/lock with lock before")
        } else {
          .before_part <- function(output_str, before_end) {
            substr(output_str, start = 1, stop = before_end - 1)
          }
          match_data  <- .process_matches(.before_part(output_str, lock_matches[1]),
                                          req_matches[req_matches < lock_matches[1]])
          output_data <- list(output    = match_data$output,
                              unlocking_data = unlock_data,
                              unhandled = paste0(match_data$unhandled,
                                                 .remaining_part(output_str, lock_matches[1])))
          cat("\nSimultaneous req/lock with req before\n---------\n")
          # .dcat("output_data", output_data)
        }
      } else if (lock_matches[1] > 0) {
        output_data <- .process_lock_matches(output_str, lock_matches)
      } else if (req_matches[1] > 0) {
        output_data <- .process_matches2(output_str, req_matches, unlock_data)
        output_data
#         output_data <- list(output = .process_matches(output_str, req_matches),
#                             unlocking_data = unlock_data,
#                             unhandled = "")
      }
    } else {
#       .dcat("output_str", output_str)
#       .dcat("lock_data$unlock_code", lock_data$unlock_code)
#       .dcat("unlock_data", unlock_data)
#       .dcat("unlock_code", unlock_code)
      unlock_matches <- gregexpr(pattern = "\n@@@@ >LISTENER ::: UNLOCK,", text = output_str)[[1]]
      if (unlock_matches[1] > 0) {
        output_data <- .process_unlock_matches(output_str, unlock_matches,
                                               lock_data$unlock_code)
      }
    }
  }
  output_data
}

.process_matches2 <- function(output_str, req_matches, unlock_data) {
  match_data  <- .process_matches(output_str, req_matches)
  if (match_data$unhandled != "") {
    .dcat("This might do the trick", match_data$unhandled)
  }
  output_data <- list(output = match_data$output,
                      unlocking_data = unlock_data,
                      unhandled = match_data$unhandled)
  output_data
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
#      cat("\nABOUT TO CRASH\n--------\n")
#      .dcat("output_str", output_str)
#      .dcat("match_idx", match_idx)
#      .dcat("match_start", matches[match_idx])
#      .dcat("remaingn", .remaining_part(output_str, matches[match_idx]))
#      .dcat("new_output_str", new_output_str)
#      .dcat("num", num)
#      .dcat("cmd_str", cmd_str)
#      .dcat("tmp_str", tmp_str)
#      .dcat("cmd_args_end", cmd_args_end)
     cat("Listener locking crash 2 fixed!\n")
     return(list(output = new_output_str,
                 unhandled = .remaining_part(output_str, matches[match_idx])))
      # stop("Listener crashed 2.")
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
  list(output = new_output_str, unhandled = "")
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

.send_listener_lock <- function() {
  lock_code <- "some_magic_123451_q4hkafbl@!k"
  if (!rstudioapi::isAvailable()) {
    cat("\n")
    cat("@@@@ >LISTENER ::: LOCK,")
    cat(lock_code)
    cat("\n")
  }
  lock_code
}

.send_listener_unlock <- function(lock_code) {
  if (!rstudioapi::isAvailable()) {
    Sys.sleep(0.3) # to prevent this to be dismissed
    cat("\n")
    cat("@@@@ >LISTENER ::: UNLOCK,")
    cat(lock_code)
    cat("\n")
  }
}
