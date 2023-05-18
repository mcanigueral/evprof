
# Bivariate Gaussian Mixture Models clustering --------------------------------------

#' Perform `mclust::Mclust` clustering for multivariate GMM
#'
#' @param sessions tibble, sessions data set in evprof
#' [standard format](https://mcanigueral.github.io/evprof/articles/sessions-format.html).
#' @param k number of clusters
#' @param mclust_tol tolerance parameter for clustering
#' @param mclust_itmax maximum number of iterations
#' @param log logical, whether to transform `ConnectionStartDateTime` and
#' `ConnectionHours` variables to natural logarithmic scale (base = `exp(1)`).
#' @param start integer, start hour in the x axis of the plot.
#' This is only used when `log = FALSE`.
#'
#' @keywords internal
#' @returns mclust object
#'
#' @importFrom mclust Mclust emControl
#'
get_connection_model_mclust_object <- function(sessions, k, mclust_tol = 1e-8, mclust_itmax = 1e4,
                                               log = FALSE, start = getOption("evprof.start.hour")) {
  if (!log) {
    sessions["ConnectionStartDateTime"] <- convert_time_dt_to_plot_num(sessions[["ConnectionStartDateTime"]], start)
  } else {
    sessions <- mutate_to_log(sessions)
  }
  sessions_cluster <- sessions[,c("ConnectionStartDateTime", "ConnectionHours")]
  Mclust(sessions_cluster, G = k, control = emControl(tol = mclust_tol, itmax = mclust_itmax))
}

#' Extract models parameters from mclust object
#'
#' @param mclust_obj `mclust::Mclust` object
#'
#' @keywords internal
#' @importFrom purrr map_dfr
#' @importFrom dplyr tibble
#'
get_connection_model_params <- function(mclust_obj) {
  purrr::map_dfr(
    factor(1:mclust_obj$G),
    ~ tibble(
      mu =  list(as.vector(mclust_obj$parameters$mean[, .x])),
      sigma = list(matrix(mclust_obj$parameters$variance$sigma[,, .x], ncol = 2)),
      ratio = mclust_obj$parameters$pro[.x]
      ),
    .id = "cluster"
  )
}

#' Visualize BIC indicator to choose the number of clusters
#'
#' @param sessions tibble, sessions data set in evprof
#' [standard format](https://mcanigueral.github.io/evprof/articles/sessions-format.html).
#' @param k sequence with the number of clusters, for example 1:10, for 1 to 10 clusters.
#' @param mclust_tol tolerance parameter for clustering
#' @param mclust_itmax maximum number of iterations
#' @param log logical, whether to transform `ConnectionStartDateTime` and
#' `ConnectionHours` variables to natural logarithmic scale (base = `exp(1)`).
#' @param start integer, start hour in the x axis of the plot.
#' This is only used when `log = FALSE`.
#'
#' @returns BIC plot
#' @export
#'
#' @importFrom graphics plot
#' @importFrom mclust mclustBIC
#'
#' @examples
#' choose_k_GMM(california_ev_sessions, k = 1:4, start = 3)
#'
#'
choose_k_GMM <- function(sessions, k, mclust_tol = 1e-8, mclust_itmax = 1e4,
                         log = FALSE, start = getOption("evprof.start.hour")) {
  mod <- get_connection_model_mclust_object(
    sessions, k = k, mclust_tol = mclust_tol,
    mclust_itmax = mclust_itmax, log = log, start = start
  )
  plot(mod, what = "BIC")
}


#' Cluster sessions with `mclust` package
#'
#' @param sessions tibble, sessions data set in evprof
#' [standard format](https://mcanigueral.github.io/evprof/articles/sessions-format.html).
#' @param k number of clusters
#' @param seed random seed
#' @param mclust_tol tolerance parameter for clustering
#' @param mclust_itmax maximum number of iterations
#' @param log logical, whether to transform `ConnectionStartDateTime` and
#' `ConnectionHours` variables to natural logarithmic scale (base = `exp(1)`).
#' @param start integer, start hour in the x axis of the plot.
#' This is only used when `log = FALSE`.
#'
#' @returns list with two attributes: sessions and models
#' @export
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
#' # The column `Cluster` has been added
#' names(sessions_clusters)
#' plot_points(sessions_clusters$sessions) +
#'   ggplot2::aes(color = Cluster)
#'
cluster_sessions <- function(sessions, k, seed, mclust_tol = 1e-8, mclust_itmax = 1e4,
                             log = FALSE, start = getOption("evprof.start.hour")) {
  set.seed(seed)
  mclust_obj <- get_connection_model_mclust_object(
    sessions, k, mclust_tol = mclust_tol, mclust_itmax = mclust_itmax,
    log = log, start = start
  )
  sessions["Cluster"] <- factor(mclust_obj$classification)
  list(
    sessions = sessions,
    models = get_connection_model_params(mclust_obj)
  )
}


#' Save iteration plots in PDF file
#'
#' @param sessions tibble, sessions data set in evprof
#' [standard format](https://mcanigueral.github.io/evprof/articles/sessions-format.html).
#' @param k number of clusters
#' @param it number of iterations
#' @param seeds seed for each iteration
#' @param filename PDF output file (with extension .pdf)
#' @param plot_scale scale of each iteration plot for a good visualization in pdf file
#' @param points_size integer, size of points in the scatter plot
#' @param mclust_tol tolerance parameter for clustering
#' @param mclust_itmax maximum number of iterations
#' @param log logical, whether to transform `ConnectionStartDateTime` and
#' `ConnectionHours` variables to natural logarithmic scale (base = `exp(1)`).
#' @param start integer, start hour in the x axis of the plot.
#' This is only used when `log = FALSE`.
#'
#' @export
#' @returns nothing, but a PDF file is saved in the path specified by parameter `filename`
#'
#' @importFrom ggplot2 ggtitle scale_color_discrete ggsave
#' @importFrom cowplot plot_grid
#' @importFrom stats runif
#'
#' @examples
#' temp_file <- file.path(tempdir(), "iteration.pdf")
#' save_clustering_iterations(california_ev_sessions, k = 2, it = 4, filename = temp_file)
#'
#'
save_clustering_iterations <- function(sessions, k, it=12, seeds = round(runif(it, min=1, max=1000)),
                                    filename = paste0("iteration_", k, "_clusters.pdf"), plot_scale = 2,
                                    points_size = 0.25, mclust_tol = 1e-8, mclust_itmax = 1e4,
                                    log = FALSE, start = getOption("evprof.start.hour")) {
  ellipses_plots <- list()
  IC_values <- tibble(
    seed = seeds,
    BIC = 0
  )

  for (i in seq_len(length(seeds))) {
    set.seed(seeds[i])
    mod <- get_connection_model_mclust_object(
      sessions, k = k, mclust_tol = mclust_tol, mclust_itmax = mclust_itmax,
      log = log, start = start
    )
    mod_params <- get_connection_model_params(mod)
    ellipses_plots[[i]] <- plot_bivarGMM(sessions, mod_params, log = log, points_size = points_size) +
      ggtitle(paste0("Seed: ", seeds[i], ", BIC: ", round(mod$bic))) +
      scale_color_discrete(labels = paste0(
        seq(1, mod$G), " (", round(mod$parameters$pro*100), "%)"
      ))
    IC_values$seed[i] <- seeds[i]
    IC_values$BIC[i] <- round(mod$bic)
  }

  opt_BIC_seed <- IC_values$seed[which(IC_values$BIC == max(IC_values$BIC))]

  message(paste("Optimal seed:", opt_BIC_seed[1], "with BIC =", max(IC_values$BIC)))

  # ggsave(filename, plot = gridExtra::marrangeGrob(ellipses_plots, nrow = round(it/3), ncol = 3), scale = plot_scale)
  ggsave(filename, plot = cowplot::plot_grid(plotlist = ellipses_plots, nrow = round(it/3), ncol = 3),
         paper="a4r", width = 49, height = 40)

}


get_ellipse <- function(mu, sigma, alpha = 0.05, npoints = 200) {
  es <- eigen(sigma)
  e1 <- es$vec %*% diag(sqrt(es$val))
  r1 <- sqrt(stats::qchisq(1 - alpha, 2))
  theta <- seq(0, 2 * pi, len = npoints)
  v1 <- cbind(r1 * cos(theta), r1 * sin(theta))
  pts <- t(mu - (e1 %*% t(v1)))
  dplyr::tibble(
    x = pts[, 1],
    y = pts[, 2]
  )
}

#' Plot Bivariate Gaussian Mixture Models
#'
#' @param sessions tibble, sessions data set in evprof
#' [standard format](https://mcanigueral.github.io/evprof/articles/sessions-format.html).
#' @param models tibble, parameters of the clusters' GMM models obtained with
#' function `cluster_sessions` (object `models` of the returned list)
#' @param profiles_names names of profiles
#' @param points_size size of scatter points in the plot
#' @param lines_size size of lines in the plot
#' @param legend_nrow number of rows in legend
#' @param log logical, whether to transform `ConnectionStartDateTime` and
#' `ConnectionHours` variables to natural logarithmic scale (base = `exp(1)`).
#' @param start integer, start hour in the x axis of the plot.
#' This is only used when `log = FALSE`.
#'
#' @returns ggplot2 plot
#' @export
#'
#' @importFrom purrr map_dfr set_names
#' @importFrom ggplot2 ggplot aes geom_point geom_path labs theme_light theme guides guide_legend scale_x_continuous scale_y_continuous
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
plot_bivarGMM <- function(sessions, models, profiles_names = seq(1, nrow(models)),
                          points_size = 0.25, lines_size = 1, legend_nrow = 2,
                          log = FALSE, start = getOption("evprof.start.hour")) {
  ellipses <- purrr::map_dfr(
    set_names(seq(1, nrow(models)), nm = profiles_names),
    ~ get_ellipse(models$mu[[.x]], models$sigma[[.x]]),
    .id = "profile"
  )
  ellipses$profile <- factor(ellipses$profile, levels = unique(profiles_names))

  if (!log) {
    sessions["ConnectionStartDateTime"] <- convert_time_dt_to_plot_num(sessions[["ConnectionStartDateTime"]], start)
  } else {
    sessions <- mutate_to_log(sessions)
  }

  plot <- ggplot(data = sessions, aes(x = .data$ConnectionStartDateTime, y = .data$ConnectionHours)) +
    geom_point(size = points_size) +
    geom_path(data = ellipses, aes(x = .data$x, y = .data$y, color = .data$profile), linewidth = lines_size) +
    labs(x = 'Connection start time', y = 'Connection hours', color = "") +
    theme_light() +
    theme(legend.position = "bottom") +
    guides(color=guide_legend(nrow=legend_nrow, byrow=TRUE))
  if (!log) {
    plot <- plot +
      scale_x_continuous(breaks = seq(5, 26, 4), labels = sprintf("%d:00", c(seq(5, 23, 4), 2))) +
      scale_y_continuous(breaks = seq(0, 24, 4))
  }
  return( plot )
}

