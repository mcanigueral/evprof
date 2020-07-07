

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
#' @param n_models number of models
#'
#' @return tibble
#' @export
#'
#' @importFrom mixtools normalmixEM
#' @importFrom dplyr tibble
#'
get_energy_model <- function(energy_vct, n_models) {
  mixmdl <- mixtools::normalmixEM(energy_vct, k = n_models, maxit = 5000)
  tibble(mu = mixmdl$mu, sigma = mixmdl$sigma, lambda = mixmdl$lambda)
}


#' Plot estimated energy density
#'
#' @param profile profile name
#' @param energy_vct energy numeric vector
#' @param estimated_energy estimated energy numeric vector
#'
#' @return ggplot
#' @export
#'
#' @importFrom ggplot2 ggplot aes_string geom_density labs theme_light
#' @importFrom dplyr tibble
#'
plot_estimated_energy_density <- function(profile, energy_vct, estimated_energy) {
  return(
    ggplot(data = tibble(x = energy_vct), aes_string(x = "x")) +
      geom_density(fill = 'navy', alpha = 0.7) +
      geom_density(
        data = tibble(x = unlist(estimated_energy)),
        aes_string(x = "x"), size = 1.2, color = "navy"
      ) +
      labs(x = "Energy charged", y = "Density", title = profile) +
      theme_light()
  )
}


#' Plot estimated energy models
#'
#' @param profile profile name
#' @param energy_vct energy numeric vector
#' @param estimated_energy estimated energy numeric vector
#'
#' @return ggplot
#' @export
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



# Simulate sessions -------------------------------------------------------

#' Estimate sessions energy values
#'
#' @param n number of sessions
#' @param mu means of univariate GMM
#' @param sigma covariance matrix of univariate GMM
#'
#' @return numeric vector
#'
#' @importFrom stats rnorm
#'
#' @noRd
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
estimate_profile <- function(n, mu, sigma) {
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
get_estimated_profiles <- function(n, profile_models) {
  return(pmap(
    profile_models,
    ~ estimate_profile(round(n*..3), ..1, ..2)
  ))
}


#' Estimate sessions parameters of a specific profile
#'
#' @param profile_name profile name
#' @param n number of sessions
#' @param models bivariate GMM of the profile
#'
#' @return tibble
#' @noRd
#'
#' @importFrom dplyr tibble
#' @importFrom purrr simplify
#'
estimate_sessions <- function(profile_name, n, models) {
  profile_idx <- which(models[["profile"]] == profile_name)
  n_sessions <- round(n*models[["profile_ratio"]][[profile_idx]])
  if (n_sessions == 0) {
    return(tibble(
      start = 0,
      period = 0,
      energy = 0
    ))
  }
  estimated_profiles <- do.call(
    rbind,
    get_estimated_profiles(n_sessions, models[["connection_models"]][[profile_idx]])
  )
  estimated_energy <- simplify(
    get_estimated_energy(n_sessions, models[["energy_models"]][[profile_idx]])
  )
  return(tibble(
    start = round(estimated_profiles[,1], 2),
    period = round(estimated_profiles[,2], 2),
    energy = round(estimated_energy[1:nrow(estimated_profiles)], 2)
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

  day_models <- ev_models[["models"]][models_month_idx*models_wday_idx][[1]]
  n_sessions <- ev_models[["n_sessions"]][models_month_idx*models_wday_idx][[1]]

  print(paste0("Profile: ", profile_name))
  print(paste0("Day models: ", day_models))
  print(paste0("N sessions: ", n_sessions))

  if (profile_name %in% day_models[["profile"]]) {
    estimate_sessions(profile_name, n_sessions, day_models) %>%
      mutate("start_dt" = day + convert_time_num_to_period(.data$start)) %>%
      select(- "start") %>%
      drop_na()
  } else {
    return( NULL )
  }
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
#' @param dates datetime vector with dates to simualte (datetime values with hour set to 00:00)
#' @param ev_models profiles models
#' @param charging_rates charging rates proportions (tibble)
#' @param interval_mins interval of time to round the sessions datetime parameters
#'
#' @return tibble
#' @export
#'
#' @importFrom purrr map map_dfr set_names
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @importFrom xts align.time
#'
simulate_sessions <- function(dates, ev_models, charging_rates, interval_mins) {
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
      ConnectionStartDateTime = xts::align.time(start_dt, n=60*interval_mins),
      ConnectionHours = round_to_interval(period, interval = interval_mins/60),
      Power = sample(charging_rates[["rate"]], size = nrow(.), prob = charging_rates[["ratio"]], replace = T),
      Energy = round_to_interval(energy, interval = Power*interval_mins/60)
    )

  # Limit energy charged according to power
  limit_idx <- sessions_estimated$Energy > sessions_estimated$Power*sessions_estimated$ConnectionHours
  sessions_estimated[limit_idx, "Energy"] <-
    sessions_estimated[limit_idx, "Power"]*sessions_estimated[limit_idx, "ConnectionHours"]

  # Increase energy resulting in 0kWh due to power rate round
  e0_idx <- sessions_estimated$Energy == 0
  sessions_estimated[e0_idx, "Energy"] <- sessions_estimated[e0_idx, "Power"]*interval_mins/60

  # Calculate charging time according to power and energy
  sessions_estimated <- sessions_estimated %>%
    mutate(
      ChargingHours = .data$Energy/.data$Power,
      ChargingEndDateTime = .data$ConnectionStartDateTime + convert_time_num_to_period(.data$ChargingHours)
    ) %>%
    select(any_of(c("Profile", sessions_feature_names)))

  return( sessions_estimated )
}
