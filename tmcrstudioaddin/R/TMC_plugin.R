# Need RStudio version > 1.1.67 for rstudioapi::showDialog()
# https://www.rstudio.com/products/rstudio/download/preview/ <- working version

source("R/AddinUI.R")
source("R/AddinServer.R")

tmc_gadget <- function() {
  ui <- .ui

  server <- .server

  shiny::runGadget(app = ui, server = server)
}
