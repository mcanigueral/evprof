
options(
  evprof.start.hour = 3
)

# Get the example `evmodel` and `sessions` included in the package
ev_model <- evprof::california_ev_model
sessions <- evprof::california_ev_sessions
temp_dir <- tempdir()


# Test preprocessing ------------------------------------------------------

# Cut outlying sessions from threshold
test_that("The outliers are removed by cutting", {

  sessions2 <- cut_sessions(sessions, connection_start_max = 24, log = FALSE)
  expect_true(nrow(sessions2) < nrow(sessions))

  sessions2 <- cut_sessions(sessions, connection_hours_max = 20, log = FALSE)
  expect_true(nrow(sessions2) < nrow(sessions))

  sessions2_log <- cut_sessions(sessions, connection_start_min = 1.5, log = TRUE)
  expect_true(nrow(sessions2_log) < nrow(sessions))

  sessions2_log <- cut_sessions(sessions, connection_hours_min = -1, log = TRUE)
  expect_true(nrow(sessions2_log) < nrow(sessions))

})


# kNN plot
test_that("kNN plots", {
  expect_true(ggplot2::is.ggplot(
    plot_kNNdist(sessions, log = FALSE)
  ))
  expect_true(ggplot2::is.ggplot(
    plot_kNNdist(sessions, log = TRUE)
  ))
})


# In the outliers detection function we depend on DBSCAN package
test_that("The outliers are detected properly with automatic MinPts and eps setting with log", {
  sessions_outliers <- sessions %>%
    head(1000) %>%
    detect_outliers(noise_th = 1, log = TRUE, MinPts = 200, eps = 0.66)
  expect_true(is.logical(sessions_outliers$Outlier))
})
test_that("The outliers are detected properly with automatic MinPts and eps setting", {
  sessions_outliers2 <- sessions %>%
    head(1000) %>%
    detect_outliers(noise_th = 1, log = FALSE, MinPts = 200, eps = 3.3)
  expect_true(is.logical(sessions_outliers2$Outlier))
})

# Outliers plots
test_that("Outliers plots", {
  sessions_outliers <- sessions %>%
    head(1000) %>%
    detect_outliers(noise_th = 1, log = TRUE, MinPts = 200, eps = 0.66)
  expect_true(ggplot2::is.ggplot(
    plot_outliers(sessions_outliers, log = TRUE)
  ))
  expect_true(ggplot2::is.ggplot(
    plot_outliers(sessions_outliers, log = FALSE)
  ))
})

# Remove outliers
test_that("The outliers are removed by filtering", {
  sessions_outliers <- sessions %>%
    head(1000) %>%
    detect_outliers(noise_th = 1, log = TRUE, MinPts = 200, eps = 0.66)
  sessions_outliers2 <- drop_outliers(sessions_outliers)
  expect_true(nrow(sessions_outliers2) < nrow(sessions_outliers))
})

# Disconnection day division lines
test_that("Disconnection day division lines plot", {
  expect_true(ggplot2::is.ggplot(
    sessions %>%
      head(1000) %>%
      plot_points() %>%
      plot_division_lines(n_lines = 1, division_hour = 10)
  ))
})


# Divisions by Disconnection day and Time-cycle
test_that("The divisions are done", {
  sessions_divided <- sessions %>%
    head(1000) %>%
    divide_by_disconnection(division_hour = 10) %>%
    divide_by_timecycle()
  expect_true(ncol(sessions_divided) > ncol(sessions))
})
