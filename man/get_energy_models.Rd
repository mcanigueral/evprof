% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modelling.R
\name{get_energy_models}
\alias{get_energy_models}
\title{Title}
\usage{
get_energy_models(sessions_profiles, k, maxit = 5000)
}
\arguments{
\item{sessions_profiles}{sessions data set with user profile attribute}

\item{k}{named numeric vector with the number of univariate Gaussian Mixture Models for each profile.
The names of the vector should correspond exactly with all user profiles in \code{sessions_profiles} tibble.}

\item{maxit}{maximum number of iterations (int)}
}
\value{
tibble
}
\description{
Title
}