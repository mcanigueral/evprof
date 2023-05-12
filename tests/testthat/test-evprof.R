library(testthat)        # load testthat package
library(evprof)       # load our package
library(dplyr)
library(lubridate)
library(purrr)
library(ggplot2)


# Simulate EV charging sessions -------------------------------------------------------
# The test of this package is done in a single script since all functions must
# use an EV charging sessions data set. For these tests, this data set is
# simulated at the beginning of the script and used in all functions below.

# Get the example `evmodel` included in the package
ev_model <- evprof::california_ev_model

# Simulating 1000 sessions of two normal distributions
set.seed(1234)
sessions <- tibble(
  ConnectionStartDateTime = c(
    dmy_hm("01/01/2023 00:00") + minutes(abs(round(rnorm(1000, 9*60, 60)))),
    dmy_hm("01/01/2023 05:00") + minutes(abs(round(rnorm(1000, 9*60, 60))))
  )
) %>%
  mutate(
    ConnectionHours = evprof:::round_to_half(abs(rnorm(nrow(.), 8*60, 60)/60)), # 30 minutes resolution
    ConnectionEndDateTime = ConnectionStartDateTime + evprof:::convert_time_num_to_period(ConnectionHours),
    Energy = abs(rnorm(nrow(.), 30, 10)),
    Power = sample(c(3.7, 7.4, 11), nrow(.), replace = T, prob =c(0.2, 0.3, 0.5)),
    Session = paste0("S", row_number())
  )


# Test exploration --------------------------------------------------------
test_that("Charging rates ratios calculation", {
  expect_true(is.data.frame(get_charging_rates_distribution(sessions)))
})
test_that("Average daily sessions calculation", {
  expect_true(is.numeric(get_daily_avg_n_sessions(sessions, 2023, 1, 7)))
})

# Exploration plots
summarise_sessions(sessions, mean, vars = c("Power", "Energy", "ConnectionHours"))
plot_histogram_grid(sessions, vars = c("Power", "Energy", "ConnectionHours"))
plot_points(sessions, log = FALSE)
plot_points(sessions, log = TRUE)
plot_density_2D(sessions, by = "wday", log = FALSE)
plot_density_2D(sessions, by = "month", log = FALSE)
plot_density_2D(sessions, by = "year", log = FALSE)
plot_density_2D(sessions, by = "wday", log = TRUE)
plot_density_2D(sessions, by = "month", log = TRUE)
plot_density_2D(sessions, by = "year", log = TRUE)
plot_density_3D(sessions, log = FALSE)
plot_density_3D(sessions, log = TRUE)


# Test preprocessing ------------------------------------------------------
# Cut outlying sessions from threshold
test_that("The outliers are removed by cutting", {
  sessions2_log <- cut_sessions(sessions, connection_start_max = 3, log = TRUE)
  sessions2 <- cut_sessions(sessions, connection_start_max = 20, log = FALSE)
  expect_true(nrow(sessions2_log) < nrow(sessions))
  expect_true(nrow(sessions2) < nrow(sessions))
})

# kNN plot
plot_kNNdist(sessions, log = TRUE)
plot_kNNdist(sessions, log = FALSE)

# In the outliers detection function we depend on DBSCAN package
test_that("The outliers are detected properly with automatic MinPts and eps setting", {
  sessions_outliers <<- detect_outliers(sessions, noise_th = 1, log = TRUE)
  sessions_outliers2 <- detect_outliers(sessions, noise_th = 1, log = FALSE)
  expect_true(is.logical(sessions_outliers$Outlier))
  expect_true(is.logical(sessions_outliers2$Outlier))
})

plot_outliers(sessions_outliers, log = TRUE)
plot_outliers(sessions_outliers, log = FALSE)

test_that("The outliers are removed by filtering", {
  sessions_outliers2 <- drop_outliers(sessions_outliers)
  expect_true(nrow(sessions_outliers2) < nrow(sessions_outliers))
})

# Disconnection day division lines
plot_points(sessions) %>%
  plot_division_lines(n_lines = 1, division_hour = 3)

# Divisions by Disconnection day and Time-cycle
test_that("The divisions are done", {
  sessions_divided <- sessions %>%
    divide_by_disconnection(days = 1, division_hour = 3) %>%
    divide_by_timecycle()
  expect_true(ncol(sessions_divided) > ncol(sessions))
})


# Test clustering ---------------------------------------------------------
# BIC plot
choose_k_GMM(sessions, 1:3)

test_that("Clustering iteration file is saved correctly",  {
  temp_file <- file.path(tempdir(), "iteration.pdf")
  save_clustering_iterations(sessions, 2, 2, filename = temp_file)
  expect_true(file.exists(temp_file))
})

# In the clustering function we depend on MCLUST package
test_that("Clusers are found correctly", {
  sessions_clusters <<- cluster_sessions(sessions, k = 2, seed = 1234, log = TRUE)
  sessions_clusters2 <- cluster_sessions(sessions, k = 2, seed = 1234, log = FALSE)
  expect_equal(names(sessions_clusters), c("sessions", "models"))
  expect_true("Cluster" %in% names(sessions_clusters$sessions))
  expect_true(nrow(sessions_clusters$models) == 2) # Number of clusters == k
})

test_that("Clusers are plotted correctly", {
  plot_clusters <- plot_bivarGMM(sessions_clusters$sessions, sessions_clusters$models, log = FALSE)
  plot_clusters_log <- plot_bivarGMM(sessions_clusters$sessions, sessions_clusters$models, log = TRUE)
  expect_true(is.ggplot(plot_clusters))
  expect_true(is.ggplot(plot_clusters_log))
})


# Test profiling ----------------------------------------------------------
test_that("Profiles are identified through cluster definitions", {
  clusters_definition <<- define_clusters(
    sessions_clusters$models,
    interpretations = c("Morning sessions", "Afternoon sessions"),
    profile_names = c("Morning", "Afternoon")
  )
  sessions_profiles <<- set_profiles(list(sessions_clusters$sessions), list(clusters_definition))
  expect_true(is.data.frame(sessions_profiles))
})


# Test modelling ----------------------------------------------------------
test_that("Get the connection models", {
  connection_GMM <<- get_connection_models(list(sessions_clusters), list(clusters_definition))
  expect_true(is.data.frame(connection_GMM))
  expect_true(all.equal(c("profile", "ratio", "connection_models"), names(connection_GMM)))
  expect_true(all.equal(c("mu", "sigma", "ratio"), names(connection_GMM$connection_models[[1]])))
})

print_connection_models_table(connection_GMM, full_width = TRUE, label = "tab:conn", caption = "connection GMM")
plot_model_clusters(list(sessions_clusters), list(clusters_definition), connection_GMM)

test_that("Get the energy models with `by_power = FALSE`", {
  energy_GMM <<- get_energy_models(sessions_profiles, log = TRUE, by_power = FALSE)
  expect_true(is.data.frame(energy_GMM))
  expect_true(all.equal(c("profile", "energy_models"), names(energy_GMM)))
  expect_true(all.equal(c("charging_rate", "energy_models", "mclust"), names(energy_GMM$energy_models[[1]])))
  expect_true(all.equal(c("mu", "sigma", "ratio"), names(energy_GMM$energy_models[[1]]$energy_models[[1]])))
})

print_user_profile_energy_models_table(energy_GMM$energy_models[[1]], full_width = TRUE, label = "tab:en", caption = "energy GMM")
plot_energy_models(energy_GMM)

test_that("Get the energy models with `by_power = TRUE`", {
  energy_GMM <- get_energy_models(sessions_profiles, log = TRUE, by_power = TRUE)
  expect_true(is.data.frame(energy_GMM))
  expect_true(all.equal(c("profile", "energy_models"), names(energy_GMM)))
  expect_true(all.equal(c("charging_rate", "energy_models", "mclust"), names(energy_GMM$energy_models[[1]])))
  expect_true(all.equal(c("mu", "sigma", "ratio"), names(energy_GMM$energy_models[[1]]$energy_models[[1]])))
})

test_that("Model file is saved correctly",  {
  evmodel <<- get_ev_model(
    names = c("Weekday", "Weekend"),
    months_lst = list(1:12),
    wdays_lst = list(1:5, 6:7),
    connection_GMM = list(connection_GMM, connection_GMM),
    energy_GMM = list(energy_GMM, energy_GMM),
    connection_log = T, energy_log = T,
    data_tz = "UTC"
  )
  print(evmodel)

  temp_model_file <<- file.path(tempdir(), "model.json")
  save_ev_model(evmodel, filename = temp_model_file)
  expect_true(file.exists(temp_model_file))
})

test_that("Model file is read correctly",  {
  evmodel <- read_ev_model(temp_model_file)
  expect_true(class(evmodel) == "evmodel")
})
