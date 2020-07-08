
# Bivariate Gaussian Mixture Models clustering --------------------------------------

#' Visuallize BIC indicator to choose the number of clusters
#'
#' @param sessions sessions data set in standard format
#' @param k sequence with the number of clusters, for example 1:10, for 1 to 10 clusters.
#' @param mclust_tol tolerance parameter for clustering
#' @param mclust_itmax maximum number of iterations
#'
#' @return BIC plot
#' @export
#'
#' @importFrom graphics plot
#' @importFrom mclust mclustBIC
#'
choose_k_GMM <- function(sessions, k, mclust_tol = 1e-8, mclust_itmax = 1e4) {
  mod <- get_mclust_object(sessions, k = 1:10, mclust_tol = mclust_tol, mclust_itmax = mclust_itmax)
  plot(mod, what = "BIC")
}


#' Perform `mclust::Mclust` clustering
#'
#' @param sessions sessions data set in standard format
#' @param k number of clusters
#' @param mclust_tol tolerance parameter for clustering
#' @param mclust_itmax maximum number of iterations
#'
#' @return mclust object
#'
#' @importFrom mclust Mclust emControl
#'
get_mclust_object <- function(sessions, k, mclust_tol = 1e-8, mclust_itmax = 1e4) {
  sessions_cluster <- sessions[,c("ConnectionStartDateTime", "ConnectionHours")]
  sessions_cluster["ConnectionStartDateTime"] <- convert_time_dt_to_plot_num(sessions_cluster[["ConnectionStartDateTime"]])
  Mclust(sessions_cluster, G = k, control = emControl(tol = mclust_tol, itmax = mclust_itmax))
}

#' Extract models parameters from mclust object
#'
#' @param mclust_obj `mclust::Mclust` object
#'
#' @importFrom purrr map_dfr
#' @importFrom dplyr tibble
#'
get_mclust_params <- function(mclust_obj) {
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

#' Cluster sessions with `mclust` package
#'
#' @param sessions sessions data set in standard format
#' @param k number of clusters
#' @param seed random seed
#' @param mclust_tol tolerance parameter for clustering
#' @param mclust_itmax maximum number of iterations
#'
#' @return list with two attributes: sessions and models
#' @export
#'
cluster_sessions <- function(sessions, k, seed, mclust_tol = 1e-8, mclust_itmax = 1e4) {
  set.seed(seed)
  mclust_obj <- get_mclust_object(sessions, k, mclust_tol = mclust_tol, mclust_itmax = mclust_itmax)
  sessions["Cluster"] <- factor(mclust_obj$classification)
  list(
    sessions = sessions,
    models = get_mclust_params(mclust_obj)
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
#' @param mclust_tol tolerance parameter for clustering
#' @param mclust_itmax maximum number of iterations
#'
#' @export
#'
#' @importFrom ggplot2 ggtitle scale_color_discrete ggsave
#' @importFrom cowplot plot_grid
#' @importFrom stats runif
#'
save_clustering_iterations <- function(sessions, k, it=12, seeds = round(runif(it, min=1, max=1000)),
                                    filename = paste0("iteration_", k, "_clusters.pdf"), plot_scale = 2,
                                    mclust_tol = 1e-8, mclust_itmax = 1e4) {
  ellipses_plots <- list()
  IC_values <- tibble(
    seed = seeds,
    BIC = 0
  )

  for (i in 1:length(seeds)) {
    sessions_cluster <- sessions
    set.seed(seeds[i])
    mod <- get_mclust_object(sessions_cluster, k = k)
    mod_params <- get_mclust_params(mod)
    ellipses_plots[[i]] <- plot_bivarGMM(sessions_cluster, mod_params) +
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
  ggsave(filename, plot = cowplot::plot_grid(plotlist = ellipses_plots, nrow = round(it/3), ncol = 3), scale = plot_scale)

}


#' Get ellipse values of a Gaussian model
#'
#' @param mu mean of the g¡Gaussian model
#' @param sigma variance of the Gaussian model
#'
#' @importFrom mixtools ellipse
#'
get_ellipse <- function(mu, sigma) {
  ellips <- mixtools::ellipse(mu = mu, sigma = sigma, npoints = 100, draw = FALSE)
  tibble(
    x = ellips[, 1],
    y = ellips[, 2]
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
#'
#' @return ggplot2 plot
#' @export
#'
#' @importFrom purrr map_dfr set_names
#' @importFrom ggplot2 ggplot aes_string geom_point geom_path labs theme_light theme guides guide_legend scale_x_continuous scale_y_continuous
#'
plot_bivarGMM <- function(sessions, bivarGMM_params, profiles_names = seq(1, nrow(bivarGMM_params)), points_size = 0.5, lines_size = 1, legend_nrow = 2) {
  ellipses <- purrr::map_dfr(
    set_names(seq(1, nrow(bivarGMM_params)), nm = profiles_names),
    ~get_ellipse(bivarGMM_params$mu[[.x]], bivarGMM_params$sigma[[.x]]),
    .id = "profile"
  )
  ellipses$profile <- factor(ellipses$profile, levels = unique(profiles_names))
  sessions_cluster <- sessions[,c("ConnectionStartDateTime", "ConnectionHours")]
  sessions_cluster["ConnectionStartDateTime"] <- convert_time_dt_to_plot_num(sessions_cluster[["ConnectionStartDateTime"]])
  ggplot(data = sessions_cluster, aes_string(x = "ConnectionStartDateTime", y = "ConnectionHours")) +
    geom_point(size = points_size) +
    geom_path(data = ellipses, aes_string(x = "x", y = "y", color = "profile"), size = lines_size) +
    labs(x = 'Connection start time', y = 'Connection hours', color = "") +
    theme_light() +
    theme(legend.position = "bottom") +
    guides(color=guide_legend(nrow=legend_nrow, byrow=TRUE)) +
    scale_x_continuous(breaks = seq(5, 26, 4), labels = sprintf("%d:00", c(seq(5, 23, 4), 2))) +
    scale_y_continuous(breaks = seq(0, 24, 4))
}

