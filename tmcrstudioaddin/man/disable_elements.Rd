% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/InputToggle.R
\name{disable_elements}
\alias{disable_elements}
\title{Disable Shiny input elements}
\usage{
disable_elements(...)
}
\arguments{
\item{...}{IDs of the \code{Shiny} input elements which are to be
disabled.}
}
\description{
Disable \code{Shiny} input elements.
}
\details{
Disables the \code{Shiny} input elements.
This is done in order to
prevent excessive buffering of button actions if the user starts
clicking buttons in other tabs during an operation that is taking a
long itme.
}
\seealso{
\code{\link[base]{as.list}},\code{\link[base]{substitute}},
\code{\link[base]{lapply}}, \code{\link[shinyjs]{disable}}
}
