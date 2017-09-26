source("R/Authentication.R")

token <- authenticate(username = "usernameHere", password = "passwordHere")
course <- tempGetCourse(token = token)