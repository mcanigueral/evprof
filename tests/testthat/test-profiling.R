
options(
  evprof.start.hour = 3
)

# Get the example `evmodel` and `sessions` included in the package
ev_model <- evprof::california_ev_model
sessions <- evprof::california_ev_sessions
temp_dir <- tempdir()



# Test profiling ----------------------------------------------------------

test_that("Profiles are identified through cluster definitions", {
  sessions_clusters <- sessions %>%
    head(1000) %>%
    cluster_sessions(k = 2, seed = 123, log = TRUE)
  clusters_definition <- define_clusters(
    sessions_clusters$models,
    interpretations = c("Morning sessions", "Afternoon sessions"),
    profile_names = c("Morning", "Afternoon")
  )
  sessions_profiles <- set_profiles(list(sessions_clusters$sessions), list(clusters_definition))
  expect_true(is.data.frame(sessions_profiles))
})

