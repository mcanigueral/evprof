library(testthat)        # load testthat package
library(evprof)       # load our package
library(dplyr)
library(lubridate)
library(purrr)
library(ggplot2)

# Get the example `evmodel` included in the package
ev_model <- evprof::california_ev_model

# Simulating 1000 sessions of two normal distributions
set.seed(123)
sessions <- tibble(
  ConnectionStartDateTime = c(
    dmy_hm("01/01/2023 00:00") + minutes(abs(round(rnorm(1000, 9*60, 60)))),
    dmy_hm("01/01/2023 05:00") + minutes(abs(round(rnorm(1000, 9*60, 60))))
  )
) %>%
  mutate(
    ConnectionEndDateTime = ConnectionStartDateTime + minutes(abs(round(rnorm(nrow(.), 8*60, 60)))),
    ConnectionHours = as.numeric(ConnectionEndDateTime - ConnectionStartDateTime, units = "hours"),
    Energy = abs(rnorm(nrow(.), 30, 10)),
    Power = sample(c(3.7, 7.4, 11), nrow(.), replace = T, prob =c(0.2, 0.3, 0.5)),
    Session = paste0("S", row_number())
  )

# plot_points(sessions)


# Test preprocessing ------------------------------------------------------
# In the outliers detection function we depend on DBSCAN package
test_that("The outliers are detected properly with automatic MinPts and eps setting", {
  sessions_outliers <- detect_outliers(sessions, noise_th = 1, log = TRUE)
  expect_true(is.logical(sessions_outliers$Outlier))
})


# Test clustering ---------------------------------------------------------
# In the clustering function we depend on MCLUST package
test_that("Clusers are found correctly", {
  sessions_clusters <<- cluster_sessions(sessions, k = 2, seed = 1234, log = TRUE)
  expect_equal(names(sessions_clusters), c("sessions", "models"))
  expect_true("Cluster" %in% names(sessions_clusters$sessions))
  expect_true(nrow(sessions_clusters$models) == 2) # Number of clusters == k


})
test_that("Clusers are plotted correctly", {
  plot_clusters <- plot_bivarGMM(sessions_clusters$sessions, sessions_clusters$models)
  expect_true(is.ggplot(plot_clusters))
})


# Test profiling ----------------------------------------------------------
test_that("Profiles are identified through cluster definitions", {
  clusters_definition <<- define_clusters(sessions_clusters$models, profile_names = c("Morning", "Afternoon"))
  sessions_profiles <<- set_profiles(list(sessions_clusters$sessions), list(clusters_definition))
  expect_true(is.data.frame(sessions_profiles))
})


# Test modelling ----------------------------------------------------------
test_that("Get the connection models", {
  connection_GMM <- get_connection_models(list(sessions_clusters), list(clusters_definition))
  expect_true(is.data.frame(connection_GMM))
  expect_true(all.equal(c("profile", "ratio", "connection_models"), names(connection_GMM)))
  expect_true(all.equal(c("mu", "sigma", "ratio"), names(connection_GMM$connection_models[[1]])))
})

test_that("Get the energy models with `by_power = FALSE`", {
  energy_GMM <- get_energy_models(sessions_profiles, log = TRUE, by_power = FALSE)
  expect_true(is.data.frame(energy_GMM))
  expect_true(all.equal(c("profile", "energy_models"), names(energy_GMM)))
  expect_true(all.equal(c("charging_rate", "energy_models", "mclust"), names(energy_GMM$energy_models[[1]])))
  expect_true(all.equal(c("mu", "sigma", "ratio"), names(energy_GMM$energy_models[[1]]$energy_models[[1]])))
})

test_that("Get the energy models with `by_power = TRUE`", {
  energy_GMM <- get_energy_models(sessions_profiles, log = TRUE, by_power = TRUE)
  expect_true(is.data.frame(energy_GMM))
  expect_true(all.equal(c("profile", "energy_models"), names(energy_GMM)))
  expect_true(all.equal(c("charging_rate", "energy_models", "mclust"), names(energy_GMM$energy_models[[1]])))
  expect_true(all.equal(c("mu", "sigma", "ratio"), names(energy_GMM$energy_models[[1]]$energy_models[[1]])))
})

