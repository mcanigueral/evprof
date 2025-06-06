% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modelling.R
\name{get_energy_models}
\alias{get_energy_models}
\title{Get a tibble of energy GMM for every user profile}
\usage{
get_energy_models(sessions_profiles, log = TRUE, by_power = FALSE)
}
\arguments{
\item{sessions_profiles}{tibble, sessions data set in evprof
\href{https://mcanigueral.github.io/evprof/articles/sessions-format.html}{ standard format}
with user profile attribute \code{Profile}}

\item{log}{logical, whether to transform \code{ConnectionStartDateTime} and
\code{ConnectionHours} variables to natural logarithmic scale (base = \code{exp(1)}).}

\item{by_power}{Logical, true to fit the energy models for every charging rate separately}
}
\value{
tibble
}
\description{
This function simulates random energy values, makes the density curve and overlaps
the simulated density curve with the real density curve of the user profile's energy values.
This is useful to appreciate how the modeled values fit the real ones and increase
or decrease the number of Gaussian components.
}
\examples{
\donttest{
library(dplyr)

# Classify each session to the corresponding user profile
sessions_profiles <- california_ev_sessions_profiles \%>\%
  dplyr::sample_frac(0.05)

# Get a table with the energy GMM parameters
get_energy_models(sessions_profiles, log = TRUE)

# If there is a `Power` variable in the data set
# you can create an energy model per power rate and user profile
# First it is convenient to round the `Power` values for more generic models
sessions_profiles <- sessions_profiles \%>\%
  mutate(Power = round_to_interval(Power, 3.7)) \%>\%
  filter(Power < 11)
sessions_profiles$Power[sessions_profiles$Power == 0] <- 3.7
get_energy_models(sessions_profiles, log = TRUE, by_power = TRUE)

}



}
