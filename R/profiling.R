
# Clusters interpretation -------------------------------------------------

#' Tabulate clusters interpretation
#'
#' @param models clusters bivariate Gaussian mixture models
#' @param interpretations vector with interpretation sentences of each cluster (arranged by cluster number)
#' @param profile_names vector with user profile assigned to each cluster (arranged by cluster number)
#' @param log Logical. Whether ConnectionStartDateTime and ConnectionHours variables are in natural logarithmic scale (base = `exp(1)`).
#'
#' @return tibble object
#' @export
#'
#' @importFrom dplyr %>% tibble arrange
#' @importFrom rlang .data
define_clusters <- function (models, interpretations = NULL, profile_names = NULL, log = TRUE) {
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

#' Obtain total sessions data set with Profile classification
#'
#' @param sessions_clustered list with sessions clustered of each susbset
#' @param clusters_interpretations tibble with clusters interpretations of each subset
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr %>% left_join select everything rename
#' @importFrom purrr map2_dfr
#'
define_profiles <- function (sessions_clustered = list(), clusters_interpretations = list()) {
  map2_dfr(
    sessions_clustered, clusters_interpretations,
    ~left_join(
      .x,
      .y %>% select("cluster", "profile") %>% rename(Cluster = "cluster", Profile = "profile"),
      by = "Cluster"
    )
  ) %>% select("Profile", everything())
}



