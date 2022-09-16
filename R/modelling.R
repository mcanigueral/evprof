

# Connection models -------------------------------------------------------

#' Get a tibble of connection GMM for every user profile
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

#' Get Mclust object of univariate Gaussian Mixture Models
#'
#' @param energy_vct numeric vector, energy from sessions
#' @param log logical, true to perform logarithmic transformation (base = exp(1))
#'
#' @return object of class `dnstyMcl`
#'
#' @importFrom mclust densityMclust cdfMclust
#'
get_energy_model_mclust_object <- function(energy_vct, log = TRUE) {
  if (log) {
    energy_vct <- log(energy_vct)
    energy_vct <- energy_vct[!is.infinite(energy_vct)]
  }
  # Discard the 2% thresholds
  uvGMM <- densityMclust(energy_vct, plot = F)
  cdf_uvGMM <- cdfMclust(uvGMM)
  th_min <- cdf_uvGMM$x[which(cdf_uvGMM$y >= 0.02)[1]]
  th_max <- cdf_uvGMM$x[which(cdf_uvGMM$y <= 0.98)[length(which(cdf_uvGMM$y <= 0.98))]]
  energy_vct <- energy_vct[energy_vct >= th_min & energy_vct <= th_max]
  densityMclust(energy_vct, plot = F)
}

#' Get energy univariate Gaussian Mixture Model
#'
#' @param mclust_obj object of class `dnstyMcl` from function `get_energy_model_mclust_object`
#'
#' @return tibble
#'
#' @importFrom purrr map_dfr
#' @importFrom dplyr tibble
#'
get_energy_model_parameters <- function(mclust_obj) {
  tibble(
    mu = mclust_obj$parameters$mean,
    sigma = sqrt(mclust_obj$parameters$variance$sigmasq),
    ratio = mclust_obj$parameters$pro
  )
}


#' Get a tibble of energy GMM for every user profile
#'
#' @param sessions_profiles sessions data set with user profile attribute
#' @param log Logical, true to perform logarithmic transformation (base = exp(1))
#' @param by_power Logical, true to fit the energy models for every charging rate separately
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr %>% group_by summarise mutate select rename
#' @importFrom tidyr nest
#' @importFrom purrr map
#' @importFrom rlang .data
#'
get_energy_models <- function(sessions_profiles, log = TRUE, by_power = FALSE) {
  if (by_power) {
    sessions_profiles <- sessions_profiles %>%
      mutate(
        ChargingRate = round_to_interval(.data$Power, 3.7)
      )
    sessions_profiles$ChargingRate[sessions_profiles$ChargingRate == 0] <- 3.7
    sessions_profiles$ChargingRate[sessions_profiles$ChargingRate > 11] <- 11
  } else {
    sessions_profiles$ChargingRate <- "Unknown"
  }

  sessions_profiles %>%
    mutate(Energy = round(.data$Energy, 3)) %>%
    filter(.data$Energy > 0) %>%
    group_by(profile = .data$Profile, charging_rate = .data$ChargingRate) %>%
    summarise(
      energy = list(.data$Energy)
    ) %>%
    mutate(
      mclust = map(.data$energy, ~ get_energy_model_mclust_object(.x, log)),
      energy_models = map(.data$mclust, ~ get_energy_model_parameters(.x))
    ) %>%
    select(.data$profile, .data$charging_rate, .data$energy_models, .data$mclust) %>%
    group_by(.data$profile) %>%
    nest() %>%
    rename(energy_models = .data$data) %>%
    ungroup()
}


#' Compare density of estimated energy with density of real energy vector
#'
#' @param energy_models energy models returned by function `get_energy_models`
#'
#' @return ggplot
#' @export
#'
#' @importFrom purrr map set_names
#' @importFrom cowplot plot_grid
#' @importFrom ggplot2 ggplot aes_string aes geom_histogram geom_line theme_light labs theme unit
#' @importFrom dplyr tibble mutate %>%
#' @importFrom mclust predict.densityMclust
#' @importFrom grDevices extendrange
#'
plot_energy_models <- function(energy_models) {

  plot_list <- list()

  for (prof in unique(energy_models$profile)) {

    em_df <- energy_models$energy_models[[which(energy_models$profile == prof)]]

    histogram_data <- unlist(map(em_df$mclust, ~ .x$data))

    profile_plot <- ggplot(data = tibble(x = histogram_data), aes_string(x = "x")) +
      geom_histogram(
        aes_string(y = "..density.."), color = 'darkgrey', fill = 'grey',
        alpha = 0.2, show.legend = T, binwidth = 0.03
      ) +
      labs(x = "Energy charged", y = "Density", title = prof) +
      theme_light()

    lines_data <- map_dfr(
      set_names(em_df$mclust, em_df$charging_rate),
      ~ tibble(
        x = seq(
          from = extendrange(.x$data, f = 0.1)[1],
          to = extendrange(.x$data, f = 0.1)[2],
          length = 1000
        )
      ) %>%
        mutate(
          y = predict.densityMclust(.x, .data$x)
        ),
      .id = "charging_rate"
    ) %>%
      mutate(
        charging_rate = factor(.data$charging_rate, levels = c("3.7", "7.4", "11", "Unknown"))
      )

    profile_plot2 <- profile_plot +
      geom_line(
        data = lines_data,
        aes_string(x = "x", y = "y", color = "charging_rate"),
        size = 1
      ) +
      labs(color = "Charging rate (kW)") +
      theme(
        legend.position = "bottom",
        plot.margin = unit(c(1, 0.5, 0.5, 0.5), "cm")
      )

    if (length(unique(em_df$charging_rate)) == 1) {
      profile_plot2 <- profile_plot2 +
        theme(
          legend.position = "none"
        )
    }

    plot_list[[prof]] <- profile_plot2
  }

  cowplot::plot_grid(plotlist = plot_list, nrow = 2)
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
#' @param data_tz character, time zone of the original data (necessary to properly simulate new sessions)
#'
#' @return object of class `evmodel`
#' @export
#'
#' @importFrom purrr map map2
#' @importFrom dplyr tibble left_join select mutate %>%
#'
get_ev_model <- function(names, months_lst = list(1:12, 1:12), wdays_lst = list(1:5, 6:7),
                         connection_GMM, energy_GMM, connection_log, energy_log, data_tz) {

  # Remove `mclust` component from energy models tibble
  energy_GMM <- map(
    energy_GMM,
    ~ .x %>%
      mutate(
        energy_models = map(
          .data$energy_models,
          ~ select(.x, - "mclust")
        )
      )
    )

  GMM <- map2(
    connection_GMM, energy_GMM,
    ~ left_join(.x, .y, by = 'profile')
  )

  ev_model <- list(
    metadata = list(
      creation = Sys.Date(),
      connection_log = connection_log,
      energy_log = energy_log,
      tzone = data_tz
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



# Print connection model tables ------------------------------------------------------


#' Get LaTeX code for the connection bivariate GMM features (mu and sigma)
#'
#' @param GMM Gaussian Mixture Models obtained from function `get_connection_models`
#'
#' @return character, LaTeX code
#' @export
#'
#' @importFrom purrr pmap_chr
print_connection_models_table <- function(GMM) {
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

print_cluster_features <- function(cluster) {
  paste(
    sep = "&",
    print_biGMM_mu_matrix(round(cluster$mu[[1]], 6)),
    print_biGMM_sigma_matrix(round(cluster$sigma[[1]], 6)),
    round(cluster$ratio*100)
  )
}

print_biGMM_sigma_matrix <- function(sigma) {
  paste(
    "\\begin{array}{cc}",
    sigma[1, 1], "&",
    sigma[1, 2], "\\\\",
    sigma[2, 1], "&",
    sigma[2, 2],
    "\\end{array}"
  )
}

print_biGMM_mu_matrix <- function(mu) {
  paste(
    "\\begin{array}{cc}",
    mu[1], "\\\\",
    mu[2],
    "\\end{array}"
  )
}


# Print energy models table -----------------------------------------------

#' Get LaTeX code for the energy GMM features (mu and sigma)
#'
#' @param GMM Gaussian Mixture Models obtained from function `get_energy_models`
#'
#' @return character, LaTeX code
#' @export
#'
#' @importFrom purrr pmap_chr
print_energy_models_table <- function(GMM) {
  paste(
    sep = "\n",
    "\\begin{tabular}{l|c|c|c}",
    "\\hline",
    "User profile & Mean ($\\mu$) & Std. deviation ($\\sigma$) & Share (\\%) \\\\",
    "\\hline",
    paste(
      collapse = "\n",
      pmap_chr(
        GMM,
        ~ print_profile_energy_models(..1, ..2)
      )
    ),
    "\\end{tabular}"
  )
}

print_profile_energy_models <- function(profile_name, energy_models) {
  paste(
    paste0("\\multirow{", nrow(energy_models), "}{*}{", profile_name, "}&"),
    paste(
      collapse = "\\\\ \\cline{2-4} & ",
      purrr::map_chr(
        energy_models %>%
          split(1:nrow(energy_models)),
        print_gaussian_features
      )
    ),
    "\\\\ \\hline"
  )
}

print_gaussian_features <- function(gaussian) {
  paste(
    sep = "&",
    round(gaussian$mu, 6),
    round(gaussian$sigma, 6),
    round(gaussian$ratio*100)
  )
}
