% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preprocessing.R
\name{cut_sessions}
\alias{cut_sessions}
\title{Cut outliers based on minimum and maximum limits of ConnectionHours and ConnectionStartDateTime variables}
\usage{
cut_sessions(
  sessions,
  connection_hours_min = NA,
  connection_hours_max = NA,
  connection_start_min = NA,
  connection_start_max = NA,
  log = FALSE
)
}
\arguments{
\item{sessions}{sessions data set in standard format}

\item{connection_hours_min}{numeric, minimum of connection hours (duration). If NA the minimum value is considered.}

\item{connection_hours_max}{numeric, maximum of connection hours (duration). If NA the maximum value is considered.}

\item{connection_start_min}{numeric, minimum hour of connection start (hour as numeric). If NA the minimum value is considered.}

\item{connection_start_max}{numeric, maximum hour of connection start (hour as numeric). If NA the maximum value is considered.}

\item{log}{Logical. Whether to transform ConnectionStartDateTime and ConnectionHours variables to natural logarithmic scale (base = \code{exp(1)}).}
}
\value{
session dataframe
}
\description{
Cut outliers based on minimum and maximum limits of ConnectionHours and ConnectionStartDateTime variables
}
