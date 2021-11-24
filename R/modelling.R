

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
#' @importFrom dplyr tibble arrange mutate select group_by summarise rename
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
        "connection_models" = list(tibble(mu = !!sym('mu'), sigma = !!sym('sigma'), ratio = !!sym('ratio')/sum(!!sym('ratio'))))
      ) %>%
      rename(ratio = .data$profile_ratio)
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
  if (log) {
    energy_vct <- log(energy_vct)
    energy_vct <- energy_vct[energy_vct > 0]
  }
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

#' Estimate sessions energy values
#'
#' @param n number of sessions
#' @param mu means of univariate GMM
#' @param sigma covariance matrix of univariate GMM
#' @param log Logical, true if models have logarithmic transformation and exponential transformation will be performed
#'
#' @return numeric vector
#' @noRd
#'
#' @importFrom stats rnorm
#'
estimate_energy <- function(n, mu, sigma, log) {
  if (n == 0) n = 1
  energy <- rnorm(n, mu, sigma)
  if (log) energy <- exp(energy)
  return( energy )
}

#' Estimate energy given energy models tibble
#'
#' @param n number of sessions
#' @param energy_models energy models tibble
#' @param log Logical, true if models have logarithmic transformation and exponential transformation will be performed
#'
#' @return list of numeric vectors
#' @noRd
#'
#' @importFrom purrr pmap
#'
get_estimated_energy <- function(n, energy_models, log) {
  return(unlist(pmap(
    energy_models,
    ~ estimate_energy(round(n*..3), ..1, ..2, log)
  )))
}

#' Compare density of estimated energy with density of real energy vector
#'
#' @param sessions_profiles sessions data set with user profile attribute in column 'Profile'
#' @param energy_models energy models returned by function `get_energy_models`
#' @param log Logical, true to apply a logarithmic transformation
#'
#' @return list of ggplots
#' @export
#'
#' @importFrom ggplot2 ggplot aes_string geom_density labs theme_light
#' @importFrom dplyr tibble rename
#' @importFrom rlang .data
#' @importFrom cowplot plot_grid
#'
plot_energy_models_density <- function(sessions_profiles, energy_models, log = TRUE) {
  set.seed(1234)
  plot_list <- energy_models %>%
    left_join(
      sessions_profiles %>%
        group_by(.data$Profile) %>%
        summarise(energy = list(
          if (log) log(.data$Energy) else .data$Energy
        )) %>%
        rename(profile = .data$Profile),
      by = 'profile'
    ) %>%
    mutate(
      estimated_energy = map2(.data$energy, .data$energy_models, ~ get_estimated_energy(length(.x), .y, log = !log))
    ) %>%
    select("profile", "energy", "estimated_energy") %>%
    pmap(
      ~ ggplot(data = tibble(x = ..2), aes_string(x = "x")) +
        geom_density(fill = 'navy', alpha = 0.7, show.legend = T) +
        geom_density(
          data = tibble(x = ..3),
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
#' @param profiles_ratios tibble with columns `profile` and `ratio`
#' @param log Logical, true to perform logarithmic transformation (base = exp(1))
#'
#' @return ggplot2
#' @export
#'
#' @importFrom purrr map map_dbl pmap_dfr
#' @importFrom dplyr tibble arrange mutate select group_by summarise
#' @importFrom rlang .data
#'
plot_model_clusters <- function(subsets_clustering = list(), clusters_interpretations = list(), profiles_ratios, log = TRUE) {

  cluster_profiles_names <- unlist(map(clusters_interpretations, ~ .x[["profile"]]))

  plot_bivarGMM(
    map_dfr(subsets_clustering, ~ .x[["sessions"]]),
    map_dfr(subsets_clustering, ~ .x[["models"]]),
    cluster_profiles_names,
    log = log
  ) +
    labs(color = "Profile") +
    scale_color_discrete(labels = paste0(
      unique(cluster_profiles_names),
      " (",
      round(profiles_ratios[["ratio"]][match(unique(cluster_profiles_names), profiles_ratios[["profile"]])]*100),
      "%)"
    ))
}


# Save the models ---------------------------------------------------------



#' Get the EV model object of class `evmodel`
#'
#' @param names character vector with the given names of each time-cycle model
#' @param months_lst list of integer vectors with the corresponding months of the year for each time-cycle model
#' @param wdays_lst list of integer vectors with the corresponding days of the week for each model (week start = 1)
#' @param connection_GMM list of different connection bivariate GMM
#' @param energy_GMM list of different energy univariate GMM
#' @param connection_log Logical, true if connection models have logarithmic transformations
#' @param energy_log Logical, true if energy models have logarithmic transformations
#'
#' @return object of class `evmodel`
#' @export
#'
#' @importFrom purrr map2
#' @importFrom dplyr tibble left_join
#'
get_ev_model <- function(names, months_lst = list(1:12, 1:12), wdays_lst = list(1:5, 6:7),
                         connection_GMM, energy_GMM, connection_log, energy_log) {

  GMM <- map2(
    connection_GMM, energy_GMM,
    ~ left_join(.x, .y, by = 'profile')
  )

  ev_model <- list(
    metadata = list(
      creation = Sys.Date(),
      connection_log = connection_log,
      energy_log = energy_log,
      tzone = getOption("evprof.tzone", 'Europe/Amsterdam')
    ),
    models = tibble(
      time_cycle = names,
      months = months_lst,
      wdays = wdays_lst,
      user_profiles = GMM
    )
  )
  class(ev_model) <- "evmodel"
  return( ev_model )
}


#' Save the EV model object of class `evmodel` to an external file
#'
#' @param evmodel object of class `evmodel`
#' @param file character string with the path or name of the file
#' @param fileext character string with the file extension
#'
#' @export
#'
save_ev_model <- function(evmodel, file = 'evmodel', fileext = '.RDS') {
  saveRDS(evmodel, file = paste0(file, fileext))
  # ev_models_json <- toJSON(evmodel)
  # write(ev_models_json, file = paste0(file, fileext))
}


# read_ev_model <- function(file) {
#   obj_json <- fromJSON(file)
# }


#' `print` method for `evmodel` object class
#'
#' @param x  `evmodel` object
#' @param ... further arguments passed to or from other methods.
#'
#' @export
#'
print.evmodel <- function(x, ...) {
  m <- x$models
  cat('EV sessions model of class "evprof", created on', as.character(x$metadata$creation), '\n')
  cat('Timezone of the model:', x$metadata$tzone, '\n')
  cat('The Gaussian Mixture Models of EV user profiles are built in:\n')
  cat('  - Connection Models:', if (x$metadata$connection_log) "logarithmic" else "natural", 'scale\n')
  cat('  - Energy Models:', if (x$metadata$energy_log) "logarithmic" else "natural", 'scale\n')
  cat('\nModel composed by', nrow(m), 'time-cycles:\n')
  for (n in 1:nrow(m)) {
    cat(
      '  ', n, '. ', m[['time_cycle']][n], ':',
      '\n     Months = ', if (length(m[['months']][[n]]) == 1) m[['months']][[n]][1] else
        paste0(m[['months']][[n]][1], '-', m[['months']][[n]][length(m[['months']][[n]])]),
      ', Week days = ', if (length(m[['wdays']][[n]]) == 1) m[['wdays']][[n]][1] else
        paste0(m[['wdays']][[n]][1], '-', m[['wdays']][[n]][length(m[['wdays']])]),
      '\n     User profiles = ', paste(m[['user_profiles']][[n]][['profile']], collapse = ", "),
      '\n', sep = ''
    )
  }
}



# Print model tables ------------------------------------------------------


print_sigma_matrix <- function(sigma) {
  paste(
    "\\begin{array}{cc}",
    sigma[1, 1], "&",
    sigma[1, 2], "\\\\",
    sigma[2, 1], "&",
    sigma[2, 2],
    "\\end{array}"
  )
}

print_mu_matrix <- function(mu) {
  paste(
    "\\begin{array}{cc}",
    mu[1], "\\\\",
    mu[2],
    "\\end{array}"
  )
}

print_cluster_features <- function(cluster) {
  paste(
    sep = "&",
    print_mu_matrix(round(cluster$mu[[1]], 6)),
    print_sigma_matrix(round(cluster$sigma[[1]], 6)),
    round(cluster$ratio*100)
  )
}

print_profile_connection_models <- function(profile_name, connection_models) {
  paste(
    paste0("\\multirow{", nrow(connection_models), "}{*}{", profile_name, "}&"),
    paste(
      collapse = "\\\\ \\cline{2-4} & ",
      purrr::map_chr(
        connection_models %>%
          split(1:nrow(connection_models)),
        print_cluster_features
      )
    ),
    "\\\\ \\hline"
  )
}


#' Get LaTeX code for the GMM features (mu and sigma)
#'
#' @param GMM Gaussian Mixture Models obtained from function `get_connection_models`
#'
#' @return character, LaTeX code
#' @export
#'
#' @importFrom purrr pmap_chr
print_GMM_table <- function(GMM) {
  paste(
    sep = "\n",
    "\\begin{tabular}{l|c|c|c}",
    "\\hline",
    "User profile & Centroid ($\\mu$) & Covariance ($\\Sigma$) & Share (\\%) \\\\",
    "\\hline",
    paste(
      collapse = "\n",
      pmap_chr(
        GMM,
        ~ print_profile_connection_models(..1, ..3)
      )
    ),
    "\\end{tabular}"
  )
}





