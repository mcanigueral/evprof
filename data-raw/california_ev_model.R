
california_ev_model <- evprof::read_ev_model(
  'vignettes/california_data/california_evmodel.json'
)

usethis::use_data(california_ev_model, overwrite = TRUE)
