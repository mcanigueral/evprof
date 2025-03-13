
california_ev_sessions <- readRDS(
  "vignettes/california_data/california_sessions.RDS"
)
usethis::use_data(california_ev_sessions, overwrite = TRUE)
