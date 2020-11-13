

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
#'
#' @return tibble
#'
#' @importFrom mixtools normalmixEM
#' @importFrom dplyr tibble
#'
get_energy_model <- function(energy_vct, k, maxit=5000) {
  mixmdl <- mixtools::normalmixEM(energy_vct, k = k, maxit = maxit)
  tibble(mu = mixmdl$mu, sigma = mixmdl$sigma, lambda = mixmdl$lambda)
}


#' Title
#'
#' @param sessions_profiles sessions data set with user profile attribute
#' @param k named numeric vector with the number of univariate Gaussian Mixture Models for each profile.
#' The names of the vector should correspond exactly with all user profiles in `sessions_profiles` tibble.
#' @param maxit maximum number of iterations (int)
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr %>% group_by arrange summarise mutate select
#' @importFrom purrr map2
#' @importFrom rlang .data
#'
get_energy_models <- function(sessions_profiles, k, maxit=5000) {

  sessions_profiles %>%
    group_by(profile = .data$Profile) %>%
    summarise(
      energy = list(.data$Energy)
    ) %>%
    arrange(match(.data$profile, names(k))) %>%
    mutate(
      k = k[.data$profile],
      energy_models = map2(.data$energy, .data$k, ~ get_energy_model(.x, .y, maxit))
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
plot_estimated_energy_models <- function(profile, energy_vct, estimated_energy) {
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


# Simulate sessions -------------------------------------------------------

#' Estimate sessions energy values
#'
#' @param n number of sessions
#' @param mu means of univariate GMM
#' @param sigma covariance matrix of univariate GMM
#'
#' @return numeric vector
#' @noRd
#'
#' @importFrom stats rnorm
#'
estimate_energy <- function(n, mu, sigma) {
  # if (n == 0) return(0)
  if (n == 0) n = 1
  energy_estimated <- rnorm(n, mu, sigma)
  # Negative values replaced by 3 kWh
  # Potential improvement to avoid negative variables: Log-Normal conversion
  energy_estimated[energy_estimated <= 1] <- 3
  return(energy_estimated)
}

#' Estimate energy given energy models tibble
#'
#' @param n number of sessions
#' @param energy_models energy models tibble
#'
#' @return list of numeric vectors
#' @noRd
#'
#' @importFrom purrr pmap
#'
get_estimated_energy <- function(n, energy_models) {
  return(pmap(
    energy_models,
    ~ estimate_energy(round(n*..3), ..1, ..2)
  ))
}


#' Estimate sessions connection values
#'
#' @param n number of sessions
#' @param mu means of bivariate GMM
#' @param sigma covariance matrix of bivariate GMM
#'
#' @return vector of numeric values
#' @noRd
#'
#' @importFrom MASS mvrnorm
#'
estimate_connection <- function(n, mu, sigma) {
  # if (n == 0) return(matrix(c(0, 0), ncol = 2))
  if (n == 0) n = 1
  MASS::mvrnorm(n = n, mu = mu, Sigma = sigma)
}


#' Get estimated profiles
#'
#' @param n number of sessions
#' @param profile_models models of the profile
#'
#' @return list with sessions connection values
#' @noRd
#'
#' @importFrom purrr pmap
#'
get_estimated_connections <- function(n, profile_models) {
  return(pmap(
    profile_models,
    ~ estimate_connection(round(n*..3), ..1, ..2)
  ))
}


#' Estimate sessions parameters of a specific profile
#'
#' @param profile_name profile name
#' @param n_sessions total number of sessions per day
#' @param connection_models bivariate GMM of the profile
#' @param energy_models univariate GMM of the profile
#'
#' @return tibble
#' @noRd
#'
#' @importFrom dplyr tibble
#' @importFrom purrr simplify
#'
estimate_sessions <- function(profile_name, n_sessions, connection_models, energy_models) {
  estimated_connections <- do.call(
    rbind,
    get_estimated_connections(n_sessions, connection_models)
  )
  estimated_energy <- simplify(
    get_estimated_energy(n_sessions, energy_models)
  )
  return(tibble(
    start = round(estimated_connections[,1], 2),
    duration = round(estimated_connections[,2], 2),
    energy = round(estimated_energy[1:nrow(estimated_connections)], 2)
  ))
}


#' Get sessions for a specific day and profile
#'
#' @param profile_name profile name
#' @param day day as datetime with hour 00:00
#' @param ev_models tibble with profiles models according to calendar
#'
#' @return tibble
#' @noRd
#'
#' @importFrom purrr map_lgl
#' @importFrom lubridate month wday
#' @importFrom dplyr %>% mutate select
#' @importFrom tidyr drop_na
#'
get_profile_day_sessions <- function(profile_name, day, ev_models) {

  month_day <- month(day)
  wday_day <- wday(day, week_start = 1)

  models_month_idx <- purrr::map_lgl(ev_models[["months"]], ~ month_day %in% .x)
  models_wday_idx <- purrr::map_lgl(ev_models[["wdays"]], ~ wday_day %in% .x)

  day_models <- ev_models[["models"]][models_month_idx & models_wday_idx][[1]]
  day_n_sessions <- ev_models[["n_sessions"]][models_month_idx & models_wday_idx][[1]]

  if (!(profile_name %in% day_models[["profile"]])) {
    return( NULL )
  }

  profile_idx <- which(day_models[["profile"]] == profile_name)
  profile_n_sessions <- round(day_n_sessions*day_models[["profile_ratio"]][[profile_idx]])

  if (profile_n_sessions == 0) {
    return( NULL )
  }

  estimate_sessions(
    profile_name,
    profile_n_sessions,
    connection_models = day_models[["connection_models"]][[profile_idx]],
    energy_models = day_models[["energy_models"]][[profile_idx]]
  ) %>%
    mutate("start_dt" = day + convert_time_num_to_period(.data$start)) %>%
    select(- "start") %>%
    drop_na()
}

#' Get profile sessions
#'
#' @param profile_name profile name
#' @param dates datetime vector with dates to simualte (datetime values with hour set to 00:00)
#' @param ev_models profiles models
#'
#' @return tibble
#' @noRd
#'
#' @importFrom purrr map_dfr
#'
get_profile_sessions <- function(profile_name, dates, ev_models) {
  map_dfr(dates, ~get_profile_day_sessions(profile_name, .x, ev_models))
}


#' Simualte sessions given datetime sequence and models
#'
#' @param ev_models tibble with columns: `model_name`, `months`, `wdays`, `models`, `n_sessions`
#' The column `models` must be a list of tibbles, while each tibble must have the columns
#'  `profile`, `profile_ratio` (between 0 and 1), `connection_models` and `energy_models`
#' @param charging_powers charging powers proportions (tibble) with two columns: `power` and `ratio`.
#' The powers must be in kW and the ratios between 0 and 1.
#' @param dates datetime vector with dates to simualte (datetime values with hour set to 00:00)
#' @param interval_mins interval of minutes (integer) to round the sessions datetime variables
#'
#' @return tibble
#' @export
#'
#' @importFrom purrr map map_dfr set_names
#' @importFrom dplyr mutate any_of
#' @importFrom rlang .data
#' @importFrom xts align.time
#'
simulate_sessions <- function(ev_models, charging_powers, dates, interval_mins) {
  # Obtain sessions from all profiles in models
  profiles <- unique(unlist(map(ev_models[["models"]], ~ .x[["profile"]])))

  sessions_estimated <- map_dfr(
    set_names(profiles, profiles),
    ~get_profile_sessions(.x, dates, ev_models),
    .id = "Profile"
  )

  # Standardize the variables
  sessions_estimated <- sessions_estimated %>%
    mutate(
      ConnectionStartDateTime = xts::align.time(.data$start_dt, n=60*interval_mins),
      ConnectionHours = round_to_interval(.data$duration, interval_mins/60),
      Power = sample(charging_powers[["power"]], size = nrow(sessions_estimated), prob = charging_powers[["ratio"]], replace = T),
      Energy = round_to_interval(.data$energy, .data$Power*interval_mins/60)
    )

  # Limit energy charged according to power
  limit_idx <- sessions_estimated$Energy > sessions_estimated$Power*sessions_estimated$ConnectionHours
  sessions_estimated[limit_idx, "Energy"] <-
    sessions_estimated[limit_idx, "Power"]*sessions_estimated[limit_idx, "ConnectionHours"]

  # Increase energy resulting in 0kWh due to power round
  e0_idx <- sessions_estimated$Energy <= 0
  sessions_estimated[e0_idx, "Energy"] <- sessions_estimated[e0_idx, "Power"]*interval_mins/60

  # Calculate charging time according to power and energy
  sessions_estimated <- sessions_estimated %>%
    mutate(
      ChargingHours = .data$Energy/.data$Power,
      ChargingEndDateTime = .data$ConnectionStartDateTime + convert_time_num_to_period(.data$ChargingHours)
    ) %>%
    select(any_of(c("Profile", evprof::sessions_feature_names)))

  return( sessions_estimated )
}


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


