

# Connection models -------------------------------------------------------

#' Aggregate clusters GMM into profiles GMM
#'
#' @param subsets_clustering list with clustering results of each subset to aggregate
#' @param clusters_interpretations list with clusters interpretations of each subset
#'
#' @return tibble
#' @export
#'
#' @importFrom purrr map_dbl pmap_dfr
#' @importFrom dplyr tibble arrange mutate select group_by summarise
#' @importFrom rlang .data
#'
get_connection_models <- function(subsets_clustering = list(), clusters_interpretations = list()) {

  subsets_n_sessions <- map_dbl(subsets_clustering, ~ nrow(.x[["sessions"]]))
  subsets_ratios <- subsets_n_sessions/sum(subsets_n_sessions)

  pmap_dfr(
    tibble(subsets_clustering, clusters_interpretations, subsets_ratios),
    ~ ..1[["models"]] %>%
      arrange(.data$cluster) %>%
      mutate("profile" = ..2[["profile"]]) %>%
      select(- "cluster") %>%
      group_by(.data$profile) %>%
      summarise(
        "profile_ratio" = sum(.data$ratio)* ..3,
        "connection_models" = list(tibble(mu = .data$mu, sigma = .data$sigma, ratio = .data$ratio/sum(.data$ratio)))
      )
  )
}



# Energy models -----------------------------------------------------------

#' Get energy univariate Gaussian Mixture Model
#'
#' @param energy_vct energy numeric vector
#' @param k number of univariate Gaussian Mixture Models (int)
#' @param maxit maximum number of iterations (int)
#' @param log Logical, true to perform logarithmic transformation (base = exp(1))
#'
#' @return tibble
#'
#' @importFrom mixtools normalmixEM
#' @importFrom dplyr tibble
#'
get_energy_model <- function(energy_vct, k, maxit=5000, log = TRUE) {
  if (log) energy_vct <- log(energy_vct)
  mixmdl <- mixtools::normalmixEM(energy_vct, k = k, maxit = maxit)
  tibble(mu = mixmdl$mu, sigma = mixmdl$sigma, lambda = mixmdl$lambda)
}


#' Title
#'
#' @param sessions_profiles sessions data set with user profile attribute
#' @param k named numeric vector with the number of univariate Gaussian Mixture Models for each profile.
#' The names of the vector should correspond exactly with all user profiles in `sessions_profiles` tibble.
#' @param maxit maximum number of iterations (int)
#' @param log Logical, true to perform logarithmic transformation (base = exp(1))
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr %>% group_by arrange summarise mutate select
#' @importFrom purrr map2
#' @importFrom rlang .data
#'
get_energy_models <- function(sessions_profiles, k, maxit=5000, log = TRUE) {
  sessions_profiles %>%
    group_by(profile = .data$Profile) %>%
    summarise(
      energy = list(.data$Energy)
    ) %>%
    arrange(match(.data$profile, names(k))) %>%
    mutate(
      k = k[.data$profile],
      energy_models = map2(.data$energy, .data$k, ~ get_energy_model(.x, .y, maxit, log))
    ) %>%
    select(.data$profile, .data$energy_models)
}


#' Compare density of estimated energy with density of real energy vector
#'
#' @param sessions_profiles sessions data set with user profile attribute
#' @param energy_models energy models returned by function `get_energy_models`
#'
#' @return list of ggplots
#' @export
#'
#' @importFrom ggplot2 ggplot aes_string geom_density labs theme_light
#' @importFrom dplyr tibble rename
#' @importFrom rlang .data
#' @importFrom cowplot plot_grid
#'
plot_energy_models_density <- function(sessions_profiles, energy_models) {
  plot_list <- energy_models %>%
    left_join(
      sessions_profiles %>%
        group_by(.data$Profile) %>%
        summarise(energy = list(.data$Energy)) %>%
        rename(profile = .data$Profile),
      by = 'profile'
    ) %>%
    mutate(
      estimated_energy = map2(.data$energy, .data$energy_models, ~ get_estimated_energy(length(.x), .y))
    ) %>%
    select("profile", "energy", "estimated_energy") %>%
    pmap(
      ~ ggplot(data = tibble(x = ..2), aes_string(x = "x")) +
        geom_density(fill = 'navy', alpha = 0.7, show.legend = T) +
        geom_density(
          data = tibble(x = unlist(..3)),
          aes_string(x = "x"), size = 1.2, color = "navy", show.legend = T
        ) +
        labs(x = "Energy charged", y = "Density", title = ..1) +
        theme_light()
    )
  plot_grid(plotlist = plot_list)
}


#' DEPRECATED: Compare Gaussian Mixture Models of estimated energy with density of real energy vector
#'
#' @param profile profile name
#' @param energy_vct energy numeric vector
#' @param estimated_energy estimated energy numeric vector
#'
#' @return ggplot
#'
#' @importFrom ggplot2 ggplot aes_string geom_density labs theme_light
#' @importFrom dplyr tibble
#'
plot_estimated_energy_models_density <- function(profile, energy_vct, estimated_energy) {
  plot <- ggplot(data = tibble(x = energy_vct), aes_string(x = "x")) +
    geom_density(fill = 'navy', alpha = 0.7) +
    labs(x = "Energy charged", y = "Density", title = profile) +
    theme_light()
  for (i in 1:length(estimated_energy)) {
    plot <- plot +
      geom_density(
        data = tibble(x = estimated_energy[[i]]),
        aes_string(x = "x"), size = 1.2, color = "navy"
      )
  }
  return(plot)
}



# Plot all clusters of the models -----------------------------------------

#' Aggregate clusters GMM into profiles GMM
#'
#' @param subsets_clustering list with clustering results of each subset to aggregate
#' @param clusters_interpretations list with clusters interpretations of each subset
#' @param profiles_ratios tibble with columns `profile` and `profile_ratio`
#'
#' @return ggplot2
#' @export
#'
#' @importFrom purrr map_dbl pmap_dfr
#' @importFrom dplyr tibble arrange mutate select group_by summarise
#' @importFrom rlang .data
#'
plot_model_clusters <- function(subsets_clustering = list(), clusters_interpretations = list(), profiles_ratios) {

  cluster_profiles_names <- unlist(map(clusters_interpretations, ~ .x[["profile"]]))

  plot_bivarGMM(
    map_dfr(subsets_clustering, ~ .x[["sessions"]]),
    map_dfr(subsets_clustering, ~ .x[["models"]]),
    cluster_profiles_names
  ) +
    labs(color = "Profile") +
    scale_color_discrete(labels = paste0(
      unique(cluster_profiles_names),
      " (",
      round(profiles_ratios[["profile_ratio"]][match(unique(cluster_profiles_names), profiles_ratios[["profile"]])]*100),
      "%)"
    ))
}


# Save the models ---------------------------------------------------------

#' Save the models as .RDS file
#'
#' @param connection_GMM list of different connection bivariate GMM
#' @param energy_GMM list of different energy univariate GMM
#' @param models_names character vector with the labels of each model
#' @param months integer vector with the corresponding months of the year for each model
#' @param wdays integer vector with the corresponding days of the week for each model (week start = 1)
#' @param connection_log Logical, true if connection models have logarithmic transformations
#' @param energy_log Logical, true if energy models have logarithmic transformations
#' @param file character string with the path or name of the .RDS file
#'
#' @export
#'
#' @importFrom purrr map2
#' @importFrom dplyr tibble left_join
#'
save_models <- function(connection_GMM = list(), energy_GMM = list(),
                        models_names, months = list(1:12, 1:12), wdays = list(1:5, 6:7),
                        connection_log = TRUE, energy_log = TRUE, file = '.') {

  GMM <- map2(
    connection_GMM, energy_GMM,
    ~ left_join(.x, .y, by = 'profile')
  )

  ev_models <- list(
    metadata = list(
      creation = Sys.Date(),
      connection_log = connection_log,
      energy_log = energy_log
    ),
    models = tibble(
      model_name = models_names,
      months = months,
      wdays = wdays,
      models = GMM
    )
  )

  saveRDS(ev_models, file = file)
}



# Modify the models -------------------------------------------------------

#' Update the ratios of the user profiles
#'
#' @param ev_models tibble with columns: `model_name`, `months`, `wdays`, `models`, `n_sessions`
#' @param new_ratios tibble with columns: `model_name`, `profile`, `profile_ratio.`
#' It must have all profiles from every model, including the ones with `profile_ratio = 0`.
#' The ratios must be between 0 and 1.
#' @param discard If TRUE, profiles with `profile_ratio == 0` will be discarded from the `ev_models` object
#'
#' @return tibble
#' @export
#'
#' @importFrom purrr map_dbl
#'
update_profiles_ratios <- function(ev_models, new_ratios, discard=FALSE) {

  for (m in 1:nrow(ev_models)) {
    model <- ev_models[["models"]][[m]]
    model_name <- ev_models[["model_name"]][[m]]
    model[["profile_ratio"]] <- map_dbl(
      model[["profile"]],
      ~ new_ratios[["profile_ratio"]][ (new_ratios[["model_name"]] == model_name) & (new_ratios[["profile"]] == .x) ]
    )

    if (discard) {
      model <- model[model[["profile_ratio"]] > 0, ]
    }

    ev_models[["models"]][[m]] <- model
  }

  return(ev_models)
}


