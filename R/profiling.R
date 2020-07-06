
# Clusters interpretation -------------------------------------------------

#' Tabulate clusters interpretation
#'
#' @param bivarGMM clusters models
#' @param interpretations vector with interpretation sentences of each cluster (arranged by cluster number)
#' @param profile_names vector with user profile assigned to each cluster (arranged by cluster number)
#'
#' @return tibble object
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
tabulate_clusters_interpretation <- function(bivarGMM, interpretations, profile_names) {
  tibble(
    "cluster" = bivarGMM$cluster,
    "mean_start_time" = map_dbl(bivarGMM$mu, ~.x[1]) %>% convert_time_num_to_chr(),
    "mean_conn_time" = map_dbl(bivarGMM$mu, ~.x[2])
  ) %>%
    arrange(.data$cluster) %>%
    mutate(
      "interpretation" = interpretations,
      "profile" = profile_names
    )
}


#' Obtain total sessions data set with Profile classification
#'
#' @param sessions_clustered list with sessions clustered of each susbset
#' @param clusters_interpretations tibble with clusters interpretations of each subset
#'
#' @return tibble
#' @export
#'
#' @importFrom purrr map2_dfr
#' @importFrom dplyr left_join select
#'
get_sessions_profiling <- function(sessions_clustered = list(), clusters_interpretations = list()) {
  purrr::map2_dfr(
    sessions_clustered, clusters_interpretations,
    ~ left_join(
      .x,
      .y %>% select("cluster", "profile") %>% rename("Cluster" = "cluster", "Profile" = "profile"),
      by = "Cluster"
    )
  ) %>%
    select(c("Profile", evprof::sessions_feature_names))
}
