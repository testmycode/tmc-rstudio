source("R/Authentication.R")

token <- authenticate(username = "usernameHere", password = "passwordHere")
course <- tmcrstudioaddin::temp_get_course(token = token)

