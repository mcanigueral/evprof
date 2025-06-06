% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exploration.R
\name{get_daily_avg_n_sessions}
\alias{get_daily_avg_n_sessions}
\title{Get the daily average number of sessions given a range of years, months and weekdays}
\usage{
get_daily_avg_n_sessions(sessions, years, months, wdays)
}
\arguments{
\item{sessions}{tibble, sessions data set in evprof standard format}

\item{years}{vector of integers, range of years to consider}

\item{months}{vector of integers, range of months to consider}

\item{wdays}{vector of integers, range of weekdays to consider}
}
\value{
tibble with the number of sessions of each date in the given time period
}
\description{
Get the daily average number of sessions given a range of years, months and weekdays
}
\examples{
get_daily_avg_n_sessions(
  california_ev_sessions,
  year = 2018, months = c(5, 6), wdays = 1
)

}
