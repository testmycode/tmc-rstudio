source("R/Authentication.R")
source("R/Zipper.R")
source("R/HTTPQueries.R")

token <- tmcrstudioaddin::authenticate(username = "rtest", password = "asdasdasd", serverAddress = "https://tmc.mooc.fi")
path <- paste(sep = "", getwd(), "/hello_world")
setwd(path)
status <- upload_current_exercise(token = token)
