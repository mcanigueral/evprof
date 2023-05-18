
#' Define each cluster with a user profile interpretation
#'
#' Every cluster has a centroid (i.e. average start time and duration) that can
#' be related to a daily human behaviour or connection pattern
#' (e.g. Worktime, Dinner, etc.). In this function, a user profile name is
#' assigned to every cluster.
#'
#' @param models tibble, parameters of the clusters' GMM models obtained with
#' function `cluster_sessions()` (object `models` of the returned list)
#' @param interpretations character vector with interpretation sentences of each cluster (arranged by cluster number)
#' @param profile_names character vector with user profile assigned to each cluster (arranged by cluster number)
#' @param log logical, whether to transform `ConnectionStartDateTime` and
#' `ConnectionHours` variables to natural logarithmic scale (base = `exp(1)`).
#'
#' @returns tibble object
#' @export
#'
#' @importFrom dplyr %>% tibble arrange
#' @importFrom rlang .data
#'
#' @examples
#' library(dplyr)
#'
#' # Select working day sessions (`Timecycle == 1`) that
#' # disconnect the same day (`Disconnection == 1`)
#' sessions_day <- california_ev_sessions %>%
#'   divide_by_timecycle(
#'     months_cycles = list(1:12), # Not differentiation between months
#'     wdays_cycles = list(1:5, 6:7) # Differentiation between workdays/weekends
#'   ) %>%
#'   divide_by_disconnection(
#'     division_hour = 10, start = 3
#'   ) %>%
#'   filter(
#'     Disconnection == 1, Timecycle == 1
#'   )
#' plot_points(sessions_day, start = 3)
#'
#' # Identify two clusters
#' sessions_clusters <- cluster_sessions(
#'   sessions_day, k=2, seed = 1234, log = TRUE
#' )
#'
#' # Plot the clusters found
#' plot_bivarGMM(
#'   sessions = sessions_clusters$sessions,
#'   models = sessions_clusters$models,
#'   log = TRUE, start = 3
#' )
#'
#' # Define the clusters with user profile interpretations
#' define_clusters(
#'   models = sessions_clusters$models,
#'   interpretations = c(
#'     "Connections during working hours",
#'     "Connections during all day (high variability)"
#'   ),
#'   profile_names = c("Workers", "Visitors"),
#'   log = TRUE
#' )
#'
define_clusters <- function (models, interpretations = NULL, profile_names = NULL, log = FALSE) {
  centroids <- tibble(
    cluster = models$cluster,
    mean_start_time = map_dbl(models$mu, ~.x[1]),
    mean_conn_time = map_dbl(models$mu, ~.x[2])
  ) %>% arrange(.data$cluster)

  if (log) {
    centroids[['mean_start_time']] <- exp( centroids[['mean_start_time']] )
    centroids[['mean_conn_time']] <- exp( centroids[['mean_conn_time']] )
  }
  if (!is.null(interpretations)) centroids[['interpretations']] <- interpretations
  if (!is.null(profile_names)) centroids[['profile']] <- profile_names

  centroids[['mean_start_time']] <- convert_time_num_to_chr(centroids[['mean_start_time']])

  return( centroids )
}

#' Classify sessions into user profiles
#'
#' Joins all sub-sets from the list, adding a new column `Profile`
#'
#' @param sessions_clustered list of tibbles with sessions clustered
#' (`sessions`object of the output from function `cluser_sessions()`) from each sub-set
#' @param clusters_definition list of tibbles with clusters definitions
#' (direct output from function `define_clusters()`) of each sub-set
#'
#' @returns tibble
#' @export
#'
#' @importFrom dplyr %>% left_join select everything rename
#' @importFrom purrr map2_dfr
#'
#' @examples
#' library(dplyr)
#'
#' # Select working day sessions (`Timecycle == 1`) that
#' # disconnect the same day (`Disconnection == 1`)
#' sessions_day <- california_ev_sessions %>%
#'   divide_by_timecycle(
#'     months_cycles = list(1:12), # Not differentiation between months
#'     wdays_cycles = list(1:5, 6:7) # Differentiation between workdays/weekends
#'   ) %>%
#'   divide_by_disconnection(
#'     division_hour = 10, start = 3
#'   ) %>%
#'   filter(
#'     Disconnection == 1, Timecycle == 1
#'   )
#'
#' # Identify two clusters
#' sessions_clusters <- cluster_sessions(
#'   sessions_day, k=2, seed = 1234, log = TRUE
#' )
#'
#' # Plot the clusters found
#' plot_bivarGMM(
#'   sessions = sessions_clusters$sessions,
#'   models = sessions_clusters$models,
#'   log = TRUE, start = 3
#' )
#'
#' # Define the clusters with user profile interpretations
#' clusters_definitions <- define_clusters(
#'   models = sessions_clusters$models,
#'   interpretations = c(
#'     "Connections during working hours",
#'     "Connections during all day (high variability)"
#'   ),
#'   profile_names = c("Workers", "Visitors"),
#'   log = TRUE
#' )
#'
#' # Classify each session to the corresponding user profile
#' sessions_profiles <- set_profiles(
#'   sessions_clustered = list(sessions_clusters$sessions),
#'   clusters_definition = list(clusters_definitions)
#' )
#'
set_profiles <- function (sessions_clustered = list(), clusters_definition = list()) {
  map2_dfr(
    sessions_clustered, clusters_definition,
    ~left_join(
      .x,
      .y %>% select("cluster", "profile") %>% rename(Cluster = "cluster", Profile = "profile"),
      by = "Cluster"
    )
  ) %>% select("Profile", everything())
}

