% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PropertiesHandler.R
\name{read_properties}
\alias{read_properties}
\title{Read the properties file}
\usage{
read_properties()
}
\value{
An R object created from the data of \code{properties.rds}.
}
\description{
Read and return the data from \code{properties.rds}.
}
\details{
Reads the data from \code{properties.rds} and returns it. If
the \code{properites.rds} file does not exist at the \code{tmcr}
directory, it is created there.
}
\seealso{
\code{\link{check_if_properties_exist}},
\code{\link{create_properties_file}},
\code{\link{get_tmcr_directory}}, \code{\link[base]{readRDS}}
}
