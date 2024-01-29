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


# Test clustering ---------------------------------------------------------

# BIC plot
test_that("BIC plot is executed without errors", {
  skip_on_cran()
  expect_no_error(
    sessions %>%
      head(1000) %>%
      choose_k_GMM(k = 1:3)
  )
})

# Clustering iteration
test_that("Clustering iteration file is saved correctly",  {
  skip_on_cran()
  temp_file <- file.path(temp_dir, "iteration.pdf")
  sessions %>%
    head(1000) %>%
    save_clustering_iterations(2, 2, filename = temp_file)
  expect_true(file.exists(temp_file))
})

# In the clustering function we depend on MCLUST package
test_that("Clusers are found correctly with log", {
  sessions_clusters <- sessions %>%
    head(1000) %>%
    cluster_sessions(k = 2, seed = 123, log = TRUE)
  expect_equal(names(sessions_clusters), c("sessions", "models"))
  expect_true("Cluster" %in% names(sessions_clusters$sessions))
  expect_true(nrow(sessions_clusters$models) == 2) # Number of clusters == k
})

test_that("Clusers are found correctly without log", {
  sessions_clusters2 <- sessions %>%
    head(1000) %>%
    cluster_sessions(k = 2, seed = 123, log = FALSE)
  expect_equal(names(sessions_clusters2), c("sessions", "models"))
  expect_true("Cluster" %in% names(sessions_clusters2$sessions))
  expect_true(nrow(sessions_clusters2$models) == 2) # Number of clusters == k
})

test_that("Clusers are plotted correctly", {3
  sessions_clusters <- sessions %>%
    head(1000) %>%
    cluster_sessions(k = 2, seed = 123, log = TRUE)
  plot_clusters <- plot_bivarGMM(sessions_clusters$sessions, sessions_clusters$models, log = FALSE)
  plot_clusters_log <- plot_bivarGMM(sessions_clusters$sessions, sessions_clusters$models, log = TRUE)
  expect_true(is.ggplot(plot_clusters))
  expect_true(is.ggplot(plot_clusters_log))
})

