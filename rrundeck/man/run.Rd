% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/job.R
\name{run}
\alias{run}
\title{Run Rundeck Job}
\usage{
run(job, options = list(), follow = FALSE,
  logLevel = RUNDECK_LOG_LEVELS, asUser = NULL, filter = NULL,
  runAtTime = NULL, ...)
}
\arguments{
\item{options}{named list}

\item{follow}{follow the output and print to console}
}
\value{
object of class \code{RundeckExecution}
}
\description{
Run Rundeck Job
}
