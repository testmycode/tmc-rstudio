% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TMC_plugin.R
\name{tmcGadget_nonblock}
\alias{tmcGadget_nonblock}
\title{Run the nonblocking TMC addin}
\usage{
tmcGadget_nonblock()
}
\value{
An listener environment with access to the R session running the
the shiny application and to the listener status
}
\description{
Run the nonblocking TMC addin on the \code{RStudio} viewer pane.
The console is working normally during the RTMC session.
}
\details{
The TMC \code{RStudio} addin was made using
\code{\link[shiny]{shiny-package}}, which allows making web
applications and \code{RStudio} addins using \code{R}.
The nonblocking version uses \code{\link[callr]{callr-package}}
to run the shiny server in another R session. The listener
is implemented using \code{later}.
}
