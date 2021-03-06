% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Submissions.R
\name{processSubmissionJson}
\alias{processSubmissionJson}
\title{Read data from the submission result JSON}
\usage{
processSubmissionJson(submitJson)
}
\arguments{
\item{submitJson}{\code{HTTP} response containg the submission result
\code{JSON}.}
}
\value{
List of data read from the submission result \code{JSON}.
List keys: \code{tests}, \code{exercise_name},
\code{all_tests_passed}, \code{points}
}
\description{
Read data from the submission result \code{JSON}.
}
\details{
Reads the test results, exercise name, boolean depending on
if all tests passed or not and the received points form the
submission result \code{JSON}.
}
\seealso{
\code{\link{processSubmission}}
}
