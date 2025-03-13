load("vignettes/california_data/california_sessions_models.RData")

california_GMM <- list(
  workdays = list(
    connection_models = workday_connection_models,
    energy_models = workday_energy_models
  ),
  weekends = list(
    connection_models = weekend_connection_models,
    energy_models = weekend_energy_models
  )
)

usethis::use_data(california_GMM, overwrite = TRUE)
