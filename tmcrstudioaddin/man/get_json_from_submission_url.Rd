% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HTTPQueries.R
\name{get_json_from_submission_url}
\alias{get_json_from_submission_url}
\title{Get exercise submission result JSON}
\usage{
get_json_from_submission_url(response, token)
}
\arguments{
\item{response}{\code{HTTP} response to the exercise submission.}

\item{token}{\code{OAuth2} token associated with the current login
session to the TMC server.}
}
\value{
\code{HTTP} response as a list from the TMC server containing
the submission result \code{JSON} if the server has finished
processing the exercise submission. List containing \code{error} key
with an error message if the \code{HTTP-GET} request failed.
}
\description{
Get exercise submission result \code{JSON} from the TMC
server.
}
\details{
Extracts the exercise submission result url from the given
response and makes an \code{HTTP-GET} request for the exercise
submission result \code{JSON}.
}
\seealso{
\code{\link[httr]{content}}, \code{\link{get_submission_json}}
}
