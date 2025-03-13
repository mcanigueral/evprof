

# Connection models -------------------------------------------------------

#' Get a tibble of connection GMM for every user profile
#'
#' @param subsets_clustering list with clustering results of each subset
#' (direct output from function `cluser_sessions()`)
#' @param clusters_definition list of tibbles with clusters definitions
#' (direct output from function `define_clusters()`) of each sub-set
#'
#' @returns tibble
#' @export
#'
#' @importFrom purrr map_dbl pmap_dfr
#' @importFrom dplyr tibble arrange mutate select group_by summarise rename
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
#'   ) %>%
#'   sample_frac(0.05)
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
#' # Create a table with the connection GMM parameters
#' get_connection_models(
#'   subsets_clustering = list(sessions_clusters),
#'   clusters_definition = list(clusters_definitions)
#' )
#'
#'
get_connection_models <- function(subsets_clustering = list(), clusters_definition = list()) {

  subsets_n_sessions <- map_dbl(subsets_clustering, ~ nrow(.x[["sessions"]]))
  subsets_ratios <- subsets_n_sessions/sum(subsets_n_sessions)

  pmap_dfr(
    tibble(subsets_clustering, clusters_definition, subsets_ratios),
    ~ ..1[["models"]] %>%
      arrange(.data$cluster) %>%
      mutate(profile = ..2[["profile"]]) %>%
      select(- "cluster") %>%
      group_by(.data$profile) %>%
      summarise(
        "profile_ratio" = sum(.data$ratio)* ..3,
        "connection_models" = list(tibble(mu = !!sym('mu'), sigma = !!sym('sigma'), ratio = !!sym('ratio')/sum(!!sym('ratio'))))
      ) %>%
      rename(ratio = "profile_ratio")
  )
}



# Energy models -----------------------------------------------------------

#' Get Mclust object of univariate Gaussian Mixture Models
#'
#' @param energy_vct numeric vector, energy from sessions
#' @param log logical, whether to transform `ConnectionStartDateTime` and
#' `ConnectionHours` variables to natural logarithmic scale (base = `exp(1)`).
#'
#' @returns object of class `dnstyMcl`
#' @keywords internal
#'
#' @importFrom mclust densityMclust cdfMclust
#'
get_energy_model_mclust_object <- function(energy_vct, log = TRUE) {
  if (log) {
    energy_vct <- log(energy_vct)
    energy_vct <- energy_vct[!is.infinite(energy_vct)]
  }
  # Discard the 2% thresholds
  uvGMM <- densityMclust(energy_vct, plot = FALSE)
  cdf_uvGMM <- cdfMclust(uvGMM)
  th_min <- cdf_uvGMM$x[which(cdf_uvGMM$y >= 0.02)[1]]
  th_max <- cdf_uvGMM$x[which(cdf_uvGMM$y <= 0.98)[length(which(cdf_uvGMM$y <= 0.98))]]
  energy_vct <- energy_vct[energy_vct >= th_min & energy_vct <= th_max]
  densityMclust(energy_vct, plot = FALSE)
}

#' Get energy univariate Gaussian Mixture Model
#'
#' This function outputs a similar ellipses plot than function `plot_bivarGMM()`
#' but using a different color for each user profile instead of clusters
#' (the clusters of a same profile have the same color now).
#'
#' @param mclust_obj object of class `dnstyMcl` from function `get_energy_model_mclust_object`
#'
#' @returns tibble
#' @keywords internal
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
#' This function simulates random energy values, makes the density curve and overlaps
#' the simulated density curve with the real density curve of the user profile's energy values.
#' This is useful to appreciate how the modeled values fit the real ones and increase
#' or decrease the number of Gaussian components.
#'
#' @param sessions_profiles tibble, sessions data set in evprof
#' [standard format](https://mcanigueral.github.io/evprof/articles/sessions-format.html)
#' with user profile attribute `Profile`
#' @param log logical, whether to transform `ConnectionStartDateTime` and
#' `ConnectionHours` variables to natural logarithmic scale (base = `exp(1)`).
#' @param by_power Logical, true to fit the energy models for every charging rate separately
#'
#' @returns tibble
#' @export
#'
#' @importFrom dplyr %>% group_by summarise mutate select rename all_of n
#' @importFrom tidyr nest
#' @importFrom purrr map
#' @importFrom rlang .data
#'
#' @examples
#' \donttest{
#' library(dplyr)
#'
#' # Classify each session to the corresponding user profile
#' sessions_profiles <- california_ev_sessions_profiles %>%
#'   dplyr::sample_frac(0.05)
#'
#' # Get a table with the energy GMM parameters
#' get_energy_models(sessions_profiles, log = TRUE)
#'
#' # If there is a `Power` variable in the data set
#' # you can create an energy model per power rate and user profile
#' # First it is convenient to round the `Power` values for more generic models
#' sessions_profiles <- sessions_profiles %>%
#'   mutate(Power = round_to_interval(Power, 3.7)) %>%
#'   filter(Power < 11)
#' sessions_profiles$Power[sessions_profiles$Power == 0] <- 3.7
#' get_energy_models(sessions_profiles, log = TRUE, by_power = TRUE)
#'
#' }
#'
#'
#'
get_energy_models <- function(sessions_profiles, log = TRUE, by_power = FALSE) {
  if (by_power) {
    n_different_power <- unique(sessions_profiles$Power)
    if (length(n_different_power) > 5) {
      message("Warning: more than 5 different charging rates in the data.
              You may have to round the `Power` values for more generic models.")
    }
    sessions_profiles$ChargingRate <- sessions_profiles$Power
  } else {
    sessions_profiles$ChargingRate <- "Unknown"
  }

  sessions_profiles %>%
    mutate(Energy = round(.data$Energy, 3)) %>%
    filter(.data$Energy > 0) %>%
    group_by(profile = .data$Profile, charging_rate = .data$ChargingRate) %>%
    filter(n() > 1) %>% # At least more than one observation per group
    summarise(
      energy = list(.data$Energy),
      n_sessions = n(),
    ) %>%
    mutate(
      ratio = .data$n_sessions/sum(.data$n_sessions),
      mclust = map(.data$energy, ~ get_energy_model_mclust_object(.x, log)),
      energy_models = map(.data$mclust, ~ get_energy_model_parameters(.x))
    ) %>%
    select(all_of(c("profile", "charging_rate", "ratio", "energy_models", "mclust"))) %>%
    group_by(.data$profile) %>%
    nest() %>%
    rename(energy_models = "data") %>%
    ungroup()
}


#' Compare density of estimated energy with density of real energy vector
#'
#' @param energy_models energy models returned by function `get_energy_models`
#' @param nrow integer, number of rows in the plot grid (passed to `cowplot::plot_grid`)
#'
#' @returns ggplot
#' @export
#'
#' @importFrom purrr map set_names
#' @importFrom cowplot plot_grid
#' @importFrom ggplot2 ggplot aes geom_histogram geom_line theme_light labs theme unit after_stat
#' @importFrom dplyr tibble mutate %>%
#' @importFrom mclust predict.densityMclust
#' @importFrom grDevices extendrange
#' @importFrom rlang .data
#'
#' @examples
#' # The package evprof provides example objects of connection and energy
#' # Gaussian Mixture Models obtained from California's open data set
#' # (see California article in package website) created with functions
#' # `get_connection models` and `get_energy models`.
#'
#' # Get the working days energy models
#' energy_models <- evprof::california_GMM$workdays$energy_models
#'
#' # Plot energy models
#' plot_energy_models(energy_models)
#'
#'
plot_energy_models <- function(energy_models, nrow=2) {

  plot_list <- list()

  for (prof in unique(energy_models$profile)) {

    em_df <- energy_models$energy_models[[which(energy_models$profile == prof)]]

    histogram_data <- unlist(purrr::map(em_df$mclust, ~ .x$data))

    profile_plot <- ggplot(data = tibble(x = histogram_data), aes(x = .data[["x"]])) +
      geom_histogram(
        aes(y = after_stat(.data$density)), color = 'darkgrey', fill = 'grey',
        alpha = 0.2, show.legend = TRUE, binwidth = 0.03
      ) +
      labs(x = "Energy charged", y = "Density", title = prof) +
      theme_light()

    lines_data <- map_dfr(
      set_names(em_df$mclust, em_df$charging_rate),
      ~ tibble(
        x = seq(
          from = extendrange(.x$data, f = 0.1)[1],
          to = extendrange(.x$data, f = 0.1)[2],
          length.out = 1000
        )
      ) %>%
        mutate(
          y = predict.densityMclust(.x, .data$x)
        ),
      .id = "charging_rate"
    )

    profile_plot2 <- profile_plot +
      geom_line(
        data = lines_data,
        aes(x = .data[["x"]], y = .data[["y"]], color = .data[["charging_rate"]]),
        linewidth = 1
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

  cowplot::plot_grid(plotlist = plot_list, nrow = nrow)
}


# Plot all clusters of every user profile -----------------------------------------

#' Plot all bi-variable GMM (clusters) with the colors corresponding
#' to the assigned user profile. This shows which clusters correspond to which
#' user profile, and the proportion of every user profile.
#'
#' @param subsets_clustering list with clustering results of each subset
#' (direct output from function `cluser_sessions()`)
#' @param clusters_definition list of tibbles with clusters definitions
#' (direct output from function `define_clusters()`) of each sub-set
#' @param profiles_ratios tibble with columns `profile` and `ratio`
#' @param log logical, whether to transform `ConnectionStartDateTime` and
#' `ConnectionHours` variables to natural logarithmic scale (base = `exp(1)`).
#'
#' @returns ggplot2
#' @export
#'
#' @importFrom purrr map map_dbl pmap_dfr
#' @importFrom dplyr tibble arrange mutate select group_by summarise
#' @importFrom rlang .data
#'
#' @examples
#' library(dplyr)
#'
#' # Select working day sessions (`Timecycle == 1`) that
#' # disconnect the same day (`Disconnection == 1`)
#' sessions_day <- evprof::california_ev_sessions_profiles %>%
#'   filter(Timecycle == "Workday") %>%
#'   sample_frac(0.05)
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
#' clusters_definitions <- define_clusters(
#'   models = sessions_clusters$models,
#'   interpretations = c(
#'     "Connections during all day (high variability)",
#'     "Connections during working hours"#'
#'   ),
#'   profile_names = c("Visitors", "Workers"),
#'   log = TRUE
#' )
#'
#' # Create a table with the connection GMM parameters
#' connection_models <- get_connection_models(
#'   subsets_clustering = list(sessions_clusters),
#'   clusters_definition = list(clusters_definitions)
#' )
#'
#' # Plot all bi-variable GMM (clusters) with the colors corresponding
#' # to their assigned user profile
#' plot_model_clusters(
#'   subsets_clustering = list(sessions_clusters),
#'   clusters_definition = list(clusters_definitions),
#'   profiles_ratios = connection_models[c("profile", "ratio")]
#' )
#'
#'
plot_model_clusters <- function(subsets_clustering = list(), clusters_definition = list(),
                                profiles_ratios, log = TRUE) {

  cluster_profiles_names <- unlist(map(clusters_definition, ~ .x[["profile"]]))

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
#' @param connection_GMM list of different connection bivariate GMM obtained from `get_connection_models`
#' @param energy_GMM list of different energy univariate GMM obtained from `get_energy_models`
#' @param connection_log logical, true if connection models have logarithmic transformations
#' @param energy_log logical, true if energy models have logarithmic transformations
#' @param data_tz character, time zone of the original data (necessary to properly simulate new sessions)
#'
#' @returns object of class `evmodel`
#' @export
#'
#' @importFrom purrr map map2
#' @importFrom dplyr tibble left_join select mutate %>%
#'
#' @examples
#' # The package evprof provides example objects of connection and energy
#' # Gaussian Mixture Models obtained from California's open data set
#' # (see California article in package website) created with functions
#' # `get_connection models` and `get_energy models`.
#'
#' # For workdays sessions
#' workdays_connection_models <- evprof::california_GMM$workdays$connection_models
#' workdays_energy_models <- evprof::california_GMM$workdays$energy_models
#'
#' # For weekends sessions
#' weekends_connection_models <- evprof::california_GMM$weekends$connection_models
#' weekends_energy_models <- evprof::california_GMM$weekends$energy_models
#'
#' # Get the whole model
#' ev_model <- get_ev_model(
#'   names = c("Workdays", "Weekends"),
#'   months_lst = list(1:12, 1:12),
#'   wdays_lst = list(1:5, 6:7),
#'   connection_GMM = list(workdays_connection_models, weekends_connection_models),
#'   energy_GMM = list(workdays_energy_models, weekends_energy_models),
#'   connection_log = TRUE,
#'   energy_log = TRUE,
#'   data_tz = "America/Los_Angeles"
#' )
#'
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


#' Save the EV model object of class `evmodel` to a JSON file
#'
#' @param evmodel object of class `evmodel`
#' (see this [link](https://mcanigueral.github.io/evprof/articles/evmodel.html) for more information)
#' @param file character string with the path or name of the file
#'
#' @returns nothing but saves the `evmodel` object in a JSON file
#' @export
#'
#' @examples
#' ev_model <- california_ev_model # Model of example
#'
#' save_ev_model(ev_model, file = file.path(tempdir(), "evmodel.json"))
#'
save_ev_model <- function(evmodel, file) {
  evmodel_lst <- list(
    metadata = evmodel$metadata,
    models = evmodel$models
  )
  ev_models_json <- jsonlite::toJSON(evmodel_lst)
  if (grepl(".json", file)) {
    write(ev_models_json, file = file)
  } else {
    write(ev_models_json, file = paste0(file, ".json"))
  }
}

#' Read an EV model JSON file and convert it to object of class `evmodel`
#'
#' @param file path to the JSON file
#'
#' @returns object of class `evmodel`
#' @export
#'
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map
#' @importFrom dplyr as_tibble
#'
#' @examples
#' ev_model <- california_ev_model # Model of example
#'
#' save_ev_model(ev_model, file = file.path(tempdir(), "evmodel.json"))
#'
#' read_ev_model(file = file.path(tempdir(), "evmodel.json"))
#'
read_ev_model <- function(file) {
  evmodel <- jsonlite::fromJSON(file)
  class(evmodel) <- "evmodel"
  evmodel$models <- dplyr::as_tibble(evmodel$models)
  evmodel$models$user_profiles <- purrr::map(
    evmodel$models$user_profiles, tidy_models
  )
  return(evmodel)
}

lst_df_to_tbl <- function(df_lst) {
  purrr::map(df_lst, as_tibble)
}

tidy_models <- function(user_models_df) {
  user_models_df <- as_tibble(user_models_df)
  user_models_df$connection_models <- lst_df_to_tbl(user_models_df$connection_models)
  user_models_df$energy_models <- purrr::map(
    user_models_df$energy_models,
    ~ .x %>%
      as_tibble() %>%
      mutate(energy_models = lst_df_to_tbl(energy_models))
  )
  user_models_df
}




#' `print` method for `evmodel` object class
#'
#' @param x  `evmodel` object
#' (see this [link](https://mcanigueral.github.io/evprof/articles/evmodel.html) for more information)
#' @param ... further arguments passed to or from other methods.
#'
#' @returns nothing but prints information about the `evmodel` object
#' @export
#' @keywords internal
#'
#' @examples
#' print(california_ev_model)
#'
#'
print.evmodel <- function(x, ...) {
  m <- x$models
  cat('EV sessions model of class "evmodel", created on', as.character(x$metadata$creation), '\n')
  cat('Timezone of the model:', x$metadata$tzone, '\n')
  cat('The Gaussian Mixture Models of EV user profiles are built in:\n')
  cat('  - Connection Models:', if (x$metadata$connection_log) "logarithmic" else "natural", 'scale\n')
  cat('  - Energy Models:', if (x$metadata$energy_log) "logarithmic" else "natural", 'scale\n')
  cat('\nModel composed by', nrow(m), 'time-cycles:\n')
  for (n in seq_len(nrow(m))) {
    cat(
      '  ', n, '. ', m[['time_cycle']][n], ':',
      '\n     Months = ', if (length(m[['months']][[n]]) == 1) m[['months']][[n]][1] else
        paste0(m[['months']][[n]][1], '-', m[['months']][[n]][length(m[['months']][[n]])]),
      ', Week days = ', if (length(m[['wdays']][[n]]) == 1) m[['wdays']][[n]][1] else
        paste0(m[['wdays']][[n]][1], '-', m[['wdays']][[n]][length(m[['wdays']][[n]])]),
      '\n     User profiles = ', paste(m[['user_profiles']][[n]][['profile']], collapse = ", "),
      '\n', sep = ''
    )
  }
}


