
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

#' Classify sessions of sub-sets into profiles
#'
#' Joins all sub-sets from the list, adding a new column `Profile`
#'
#' @param sessions_clustered list of tibbles with sessions clustered (output from function `cluser_sessions`) from each sub-set
#' @param clusters_definition list of tibbles with clusters definitions (output from function `define_clusters`) of each sub-set
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr %>% left_join select everything rename
#' @importFrom purrr map2_dfr
#'
set_profiles <- function (sessions_clustered = list(), clusters_definition = list()) {
  if (!(("Cluster") %in% names(sessions_clustered))) {
    message("Column `Cluster` must be in sessions clustered data frame.")
    return(NULL)
  }
  if (!(("cluster") %in% names(clusters_definition)) | !(("profile") %in% names(clusters_definition))) {
    message("Columns `cluster` and `profile` must be in clusters definition data frame.")
    return(NULL)
  }
  map2_dfr(
    sessions_clustered, clusters_definition,
    ~left_join(
      .x,
      .y %>% select("cluster", "profile") %>% rename(Cluster = "cluster", Profile = "profile"),
      by = "Cluster"
    )
  ) %>% select("Profile", everything())
}

