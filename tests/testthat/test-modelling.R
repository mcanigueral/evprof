library(testthat)        # load testthat package
library(evprof)
library(dplyr)
library(tibble)
library(lubridate)
library(purrr)
library(ggplot2)

# Get the example `evmodel` and `sessions` included in the package
ev_model <- evprof::california_ev_model
sessions <- evprof::california_ev_sessions
temp_dir <- tempdir()

# Test modelling ----------------------------------------------------------

test_that("Get the connection models", {
  sessions_clusters <- sessions %>%
    head(1000) %>%
    cluster_sessions(k = 2, seed = 123, log = TRUE)
  clusters_definition <- define_clusters(
    sessions_clusters$models,
    interpretations = c("Morning sessions", "Afternoon sessions"),
    profile_names = c("Morning", "Afternoon")
  )
  connection_GMM <- get_connection_models(list(sessions_clusters), list(clusters_definition))
  expect_true(is.data.frame(connection_GMM))
  expect_true(all.equal(c("profile", "ratio", "connection_models"), names(connection_GMM)))
  expect_true(all.equal(c("mu", "sigma", "ratio"), names(connection_GMM$connection_models[[1]])))
})

test_that("Plot of connection GMM are generated without errors", {
  sessions_clusters <- sessions %>%
    head(1000) %>%
    cluster_sessions(k = 2, seed = 123, log = TRUE)
  clusters_definition <- define_clusters(
    sessions_clusters$models,
    interpretations = c("Morning sessions", "Afternoon sessions"),
    profile_names = c("Morning", "Afternoon")
  )
  connection_GMM <- get_connection_models(list(sessions_clusters), list(clusters_definition))
  expect_true(is.ggplot(
    plot_model_clusters(list(sessions_clusters), list(clusters_definition), connection_GMM)
  ))
})


test_that("Get and plot the energy models with `by_power = FALSE`", {
  sessions_clusters <- sessions %>%
    head(1000) %>%
    cluster_sessions(k = 2, seed = 123, log = TRUE)
  clusters_definition <- define_clusters(
    sessions_clusters$models,
    interpretations = c("Morning sessions", "Afternoon sessions"),
    profile_names = c("Morning", "Afternoon")
  )
  sessions_profiles <- set_profiles(list(sessions_clusters$sessions), list(clusters_definition))
  energy_GMM <- get_energy_models(sessions_profiles, log = TRUE, by_power = FALSE)
  expect_true(is.data.frame(energy_GMM))
  expect_true(all.equal(c("profile", "energy_models"), names(energy_GMM)))
  expect_true(all.equal(c("charging_rate", "ratio", "energy_models", "mclust"), names(energy_GMM$energy_models[[1]])))
  expect_true(all.equal(c("mu", "sigma", "ratio"), names(energy_GMM$energy_models[[1]]$energy_models[[1]])))
})

test_that("Plot of energy GMM are generated without errors", {
  sessions_clusters <- sessions %>%
    head(1000) %>%
    cluster_sessions(k = 2, seed = 123, log = TRUE)
  clusters_definition <- define_clusters(
    sessions_clusters$models,
    interpretations = c("Morning sessions", "Afternoon sessions"),
    profile_names = c("Morning", "Afternoon")
  )
  sessions_profiles <- set_profiles(list(sessions_clusters$sessions), list(clusters_definition))
  energy_GMM <- get_energy_models(sessions_profiles, log = TRUE, by_power = FALSE)
  expect_true(is.ggplot(
    plot_energy_models(energy_GMM)
  ))
})


test_that("Get and plot the energy models with `by_power = TRUE`", {
  sessions_clusters <- sessions %>%
    head(1000) %>%
    cluster_sessions(k = 2, seed = 123, log = TRUE)
  clusters_definition <- define_clusters(
    sessions_clusters$models,
    interpretations = c("Morning sessions", "Afternoon sessions"),
    profile_names = c("Morning", "Afternoon")
  )
  sessions_profiles <- set_profiles(list(sessions_clusters$sessions), list(clusters_definition))
  sessions_profiles <- sessions_profiles %>%
    mutate(Power = round_to_interval(Power, 3.7)) %>%
    filter(Power < 11)
  sessions_profiles$Power[sessions_profiles$Power == 0] <- 3.7

  energy_GMM <- get_energy_models(sessions_profiles, log = TRUE, by_power = TRUE)
  expect_true(is.data.frame(energy_GMM))
  expect_true(all.equal(c("profile", "energy_models"), names(energy_GMM)))
  expect_true(all.equal(c("charging_rate", "ratio", "energy_models", "mclust"), names(energy_GMM$energy_models[[1]])))
  expect_true(all.equal(c("mu", "sigma", "ratio"), names(energy_GMM$energy_models[[1]]$energy_models[[1]])))

  energy_plot <- plot_energy_models(energy_GMM)
  expect_true(is.ggplot(energy_plot))
})

test_that("Model file is created and saved correctly",  {
  skip_on_cran()
  sessions_clusters <- sessions %>%
    head(1000) %>%
    cluster_sessions(k = 2, seed = 123, log = TRUE)
  clusters_definition <- define_clusters(
    sessions_clusters$models,
    interpretations = c("Morning sessions", "Afternoon sessions"),
    profile_names = c("Morning", "Afternoon")
  )
  sessions_profiles <- set_profiles(list(sessions_clusters$sessions), list(clusters_definition))

  connection_GMM <- get_connection_models(list(sessions_clusters), list(clusters_definition))
  energy_GMM <- get_energy_models(sessions_profiles, log = TRUE, by_power = FALSE)

  evmodel <- get_ev_model(
    names = c("Weekday", "Weekend"),
    months_lst = list(1:12),
    wdays_lst = list(1:5, 6:7),
    connection_GMM = list(connection_GMM, connection_GMM),
    energy_GMM = list(energy_GMM, energy_GMM),
    connection_log = T, energy_log = T,
    data_tz = "America/Los_Angeles"
  )

  temp_model_file <- file.path(temp_dir, "model.json")
  save_ev_model(evmodel, file = temp_model_file)

  expect_true(file.exists(temp_model_file))
})

test_that("Model file is read correctly",  {
  skip_on_cran()
  temp_model_file <- file.path(temp_dir, "model.json")
  evmodel <- read_ev_model(file = temp_model_file)

  expect_true(class(evmodel) == "evmodel")
})
