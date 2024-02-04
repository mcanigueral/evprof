
options(
  evprof.start.hour = 3
)

# Get the example `evmodel` and `sessions` included in the package
ev_model <- evprof::california_ev_model
sessions <- evprof::california_ev_sessions
temp_dir <- tempdir()

# Test exploration --------------------------------------------------------

test_that("Charging rates ratios calculation", {
  expect_true(tibble::is_tibble(get_charging_rates_distribution(sessions)))
})
test_that("Average daily sessions calculation", {
  expect_true(is.numeric(get_daily_avg_n_sessions(sessions, 2023, 1, 7)))
})

# Exploration plots
test_that("Statistic summary of sessions' features", {
  expect_true(tibble::is_tibble(
    summarise_sessions(sessions, mean, vars = c("Power", "Energy", "ConnectionHours"))
  ))
})

test_that("Statistic plot of sessions' features", {
  expect_true(ggplot2::is.ggplot(
    plot_histogram_grid(sessions, vars = c("Power", "Energy", "ConnectionHours"))
  ))
})

test_that("Points plot", {
  expect_true(ggplot2::is.ggplot(
    plot_points(sessions, log = FALSE)
  ))
  expect_true(ggplot2::is.ggplot(
    plot_points(sessions, log = TRUE)
  ))
})

test_that("Density 2D plots", {
  expect_true(ggplot2::is.ggplot(
    plot_density_2D(sessions, by = "wday", log = FALSE)
  ))
  expect_true(ggplot2::is.ggplot(
    plot_density_2D(sessions, by = "month", log = FALSE)
  ))
  expect_true(ggplot2::is.ggplot(
    plot_density_2D(sessions, by = "year", log = FALSE)
  ))
  expect_true(ggplot2::is.ggplot(
    plot_density_2D(sessions, by = "wday", log = TRUE)
  ))
  expect_true(ggplot2::is.ggplot(
    plot_density_2D(sessions, by = "month", log = TRUE)
  ))
  expect_true(ggplot2::is.ggplot(
    plot_density_2D(sessions, by = "year", log = TRUE)
  ))
})

test_that("Density 3D plots", {
  expect_true("plotly" %in% class(
    plot_density_3D(sessions, log = FALSE)
  ))
  expect_true("plotly" %in% class(
    plot_density_3D(sessions, log = TRUE)
  ))
})

