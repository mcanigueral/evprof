
# Bivariate Gaussian Mixture Models clustering --------------------------------------

#' Perform `mclust::Mclust` clustering for multivariate GMM
#'
#' @param sessions sessions data set in standard format
#' @param k number of clusters
#' @param mclust_tol tolerance parameter for clustering
#' @param mclust_itmax maximum number of iterations
#' @param log Logical. Whether to transform ConnectionStartDateTime and ConnectionHours variables to natural logarithmic scale (base = `exp(1)`).
#'
#' @keywords internal
#' @return mclust object
#'
#' @importFrom mclust Mclust emControl
#'
get_connection_model_mclust_object <- function(sessions, k, mclust_tol = 1e-8, mclust_itmax = 1e4, log = TRUE) {
  if (!log) {
    sessions["ConnectionStartDateTime"] <- convert_time_dt_to_plot_num(sessions[["ConnectionStartDateTime"]])
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

#' Visuallize BIC indicator to choose the number of clusters
#'
#' @param sessions sessions data set in standard format
#' @param k sequence with the number of clusters, for example 1:10, for 1 to 10 clusters.
#' @param mclust_tol tolerance parameter for clustering
#' @param mclust_itmax maximum number of iterations
#' @param log Logical. Whether to transform ConnectionStartDateTime and ConnectionHours variables to natural logarithmic scale (base = `exp(1)`).
#'
#' @return BIC plot
#' @export
#'
#' @importFrom graphics plot
#' @importFrom mclust mclustBIC
#'
choose_k_GMM <- function(sessions, k, mclust_tol = 1e-8, mclust_itmax = 1e4, log = TRUE) {
  mod <- get_connection_model_mclust_object(sessions, k = k, mclust_tol = mclust_tol, mclust_itmax = mclust_itmax, log = log)
  plot(mod, what = "BIC")
}


#' Cluster sessions with `mclust` package
#'
#' @param sessions sessions data set in standard format
#' @param k number of clusters
#' @param seed random seed
#' @param mclust_tol tolerance parameter for clustering
#' @param mclust_itmax maximum number of iterations
#' @param log Logical. Whether to transform ConnectionStartDateTime and ConnectionHours variables to natural logarithmic scale (base = `exp(1)`).
#'
#' @return list with two attributes: sessions and models
#' @export
#'
cluster_sessions <- function(sessions, k, seed, mclust_tol = 1e-8, mclust_itmax = 1e4, log = TRUE) {
  set.seed(seed)
  mclust_obj <- get_connection_model_mclust_object(sessions, k, mclust_tol = mclust_tol, mclust_itmax = mclust_itmax, log = log)
  sessions["Cluster"] <- factor(mclust_obj$classification)
  list(
    sessions = sessions,
    models = get_connection_model_params(mclust_obj)
  )
}


#' Save iteration plots in PDF file
#'
#' @param sessions sessions data set in standard format
#' @param k number of clusters
#' @param it number of iterations
#' @param seeds seed for each iteration
#' @param filename PDF output file (with extension .pdf)
#' @param plot_scale scale of each iteration plot for a good visualization in pdf file
#' @param points_size integer, size of points in the scatter plot
#' @param mclust_tol tolerance parameter for clustering
#' @param mclust_itmax maximum number of iterations
#' @param log Logical. Whether to transform ConnectionStartDateTime and ConnectionHours variables to natural logarithmic scale (base = `exp(1)`).
#'
#' @export
#'
#' @importFrom ggplot2 ggtitle scale_color_discrete ggsave
#' @importFrom cowplot plot_grid
#' @importFrom stats runif
#'
save_clustering_iterations <- function(sessions, k, it=12, seeds = round(runif(it, min=1, max=1000)),
                                    filename = paste0("iteration_", k, "_clusters.pdf"), plot_scale = 2,
                                    points_size = 0.25, mclust_tol = 1e-8, mclust_itmax = 1e4, log = TRUE) {
  ellipses_plots <- list()
  IC_values <- tibble(
    seed = seeds,
    BIC = 0
  )

  for (i in 1:length(seeds)) {
    set.seed(seeds[i])
    mod <- get_connection_model_mclust_object(sessions, k = k, mclust_tol = mclust_tol, mclust_itmax = mclust_itmax, log = log)
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
  pts = t(mu - (e1 %*% t(v1)))
  dplyr::tibble(
    x = pts[, 1],
    y = pts[, 2]
  )
}

#' Plot Bivariate Gaussian Mixture Models
#'
#' @param sessions sessions data set in standard format
#' @param bivarGMM_params parameters of the clusters' GMM models
#' @param profiles_names names of profiles
#' @param points_size size of scatter points in the plot
#' @param lines_size size of lines in the plot
#' @param legend_nrow number of rows in legend
#' @param log Logical. Whether to transform ConnectionStartDateTime and ConnectionHours variables to natural logarithmic scale (base = `exp(1)`).
#'
#' @return ggplot2 plot
#' @export
#'
#' @importFrom purrr map_dfr set_names
#' @importFrom ggplot2 ggplot aes_string geom_point geom_path labs theme_light theme guides guide_legend scale_x_continuous scale_y_continuous
#'
plot_bivarGMM <- function(sessions, bivarGMM_params, profiles_names = seq(1, nrow(bivarGMM_params)), points_size = 0.25, lines_size = 1, legend_nrow = 2, log = TRUE) {
  ellipses <- purrr::map_dfr(
    set_names(seq(1, nrow(bivarGMM_params)), nm = profiles_names),
    ~ get_ellipse(bivarGMM_params$mu[[.x]], bivarGMM_params$sigma[[.x]]),
    .id = "profile"
  )
  ellipses$profile <- factor(ellipses$profile, levels = unique(profiles_names))

  if (!log) {
    sessions["ConnectionStartDateTime"] <- convert_time_dt_to_plot_num(sessions[["ConnectionStartDateTime"]])
  } else {
    sessions <- mutate_to_log(sessions)
  }

  plot <- ggplot(data = sessions, aes_string(x = "ConnectionStartDateTime", y = "ConnectionHours")) +
    geom_point(size = points_size) +
    geom_path(data = ellipses, aes_string(x = "x", y = "y", color = "profile"), size = lines_size) +
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

