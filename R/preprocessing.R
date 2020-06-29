

# Data division -----------------------------------------------------------

#' {ggplot2} type function to plot a division line
#'
#' @param day_n Number of the day below the line
#' @param start_hour Starting hour
#'
#' @return ggplot2 function
#' @export
#'
#' @importFrom ggplot2 geom_line aes_string
#' @importFrom dplyr tibble
#' @importFrom lubridate force_tz hours days
#' @importFrom rlang .data
#'
get_division_line <- function(day_n, start_hour) {
  geom_line(data = tibble(
    "dt" = seq.POSIXt(from = force_tz(Sys.Date() + hours(start_hour), tzone = getOption("evprof.tzone")), to = force_tz(Sys.Date() + days(1) + hours(start_hour), tzone = getOption("evprof.tzone")), by = "hour"),
    "line" = as.numeric(difftime(force_tz(Sys.Date()+days(day_n) + hours(start_hour), tzone = getOption("evprof.tzone")), .data$dt, units = "hours"))
  ), aes_string("dt", "line"), size = 1, color = "red", linetype = "dashed")
}

#' Iteration over evprof::plot_division_line function to plot multiple lines
#'
#' @param ggplot_points ggplot2 returned by evprof::plot_points function
#' @param n_lines number of lines to plot
#' @param start_hour Starting hour
#'
#' @return ggplot2 function
#' @export
#'
plot_division_lines <- function(ggplot_points, n_lines, start_hour) {
  ggplot_points_lines <- ggplot_points
  for (d in 1:n_lines) {
    ggplot_points_lines <- ggplot_points_lines + get_division_line(d, start_hour)
  }
  return(ggplot_points_lines)
}

#' Divide sessions by disconnection day
#'
#' @param sessions sessions data set in standard format
#' @param days number of disconnection days to select
#' @param start_hour Starting hour
#'
#' @return same sessions data set with extra column "Disconnection"
#' @export
#'
#' @importFrom purrr map_dfr
#' @importFrom dplyr filter
#' @importFrom lubridate days hours
#' @importFrom rlang .data
#'
divide_by_disconnection <- function(sessions, days, start_hour) {
  sessions[["StartTime"]] <- convert_time_dt_to_plot_dt(sessions[["ConnectionStartDateTime"]])
  sessions[["EndTime"]] <- sessions[["StartTime"]] + convert_time_num_to_period(sessions[["ConnectionHours"]])

  purrr::map_dfr(
    days,
    ~ filter(sessions,
             .data$EndTime > (Sys.Date() + days(.x-1) + hours(start_hour)),
             .data$EndTime <= (Sys.Date() + days(.x) + hours(start_hour))),
    .id = "Disconnection"
  )
}


#' Divide sessions by time-cycle
#'
#' @param sessions sessions data set in standard format
#' @param wdays_cycles list containing Weekdays cycles
#' @param months_cycles list containing Monthly cycles
#'
#' @return same sessions data set with extra column "Timecycle"
#' @export
#'
#' @importFrom purrr map2_dfr
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @importFrom lubridate month wday
#'
divide_by_timecycle <- function(sessions, wdays_cycles = list(1:5, 6:7), months_cycles = list(1:12)) {

  wdays_cycles <- rep(wdays_cycles, (length(wdays_cycles) * length(months_cycles))/length(wdays_cycles))
  months_cycles <- rep(months_cycles, (length(wdays_cycles) * length(months_cycles))/length(months_cycles))

  purrr::map2_dfr(
    months_cycles, wdays_cycles,
    ~ filter(sessions,
             month(.data$ConnectionStartDateTime) %in% .x,
             wday(.data$ConnectionStartDateTime, week_start = 1) %in% .y),
    .id = "Timecycle"
  )
}


#' Plot divisions in a wrapped ggplot
#'
#' @param sessions sessions data set in standard format
#' @param by_disconnection boolean
#' @param by_timecycle boolean
#' @param plottype "density" or "points" plot
#' @param nrow number of rows for the ggplot2::facet_wrap
#' @param ncol number of columns for the ggplot2::facet_wrap
#'
#' @return ggplot
#' @export
#'
#' @importFrom purrr when
#' @importFrom dplyr %>% group_by summarise mutate select n left_join
#' @importFrom ggplot2 facet_wrap theme
#' @importFrom rlang .data
#'
plot_divisions <- function(sessions, by_disconnection = TRUE, by_timecycle = TRUE, plottype = c("points", "density"), nrow = NULL, ncol = NULL) {
  groups_pct <- sessions %>%
    purrr::when(
      by_disconnection & by_timecycle ~ group_by(., .data$Disconnection, .data$Timecycle),
      by_disconnection & !by_timecycle ~ group_by(., .data$Disconnection),
      !by_disconnection & by_timecycle ~ group_by(., .data$Timecycle)
    ) %>%
    summarise("n" = n()) %>%
    mutate("SessionsPct" = .data$n/sum(.data$n)*100) %>%
    select(-"n")

  sessions <- sessions %>%
    left_join(groups_pct, by = c(if (by_disconnection) .data$Disconnection, if (by_timecycle) .data$Timecycle)) %>%
    mutate(
      "DivisionLabel" = paste0(
        if (by_disconnection) paste0("Disconnection: ", .data$Disconnection),
        if (by_disconnection & by_timecycle) ", ",
        if (by_timecycle) paste0("Time-cycle: ", .data$Timecycle),
        " (", round(.data$SessionsPct, 1), "%)"
      )
    )

  if (plottype == "points") {
    plot_points(sessions) + facet_wrap(~ .data$DivisionLabel, nrow = nrow, ncol = ncol)
  } else {
    plot_density_2D(sessions) + facet_wrap(~ .data$DivisionLabel, nrow = nrow, ncol = ncol) + theme(legend.position = "none")
  }
}



# DBSCAN Clustering -------------------------------------------------------


#' Get MinPts value according to percentage of sessions data set size (rows)
#'
#' @param sessions sessions data set in standard format
#' @param pct percentage of rows to consider
#'
#' @return integer value
#' @export
#'
get_MinPts <- function(sessions, pct=0.05) {
  round(pct*nrow(sessions))
  # round(log(nrow(sessions)))
}

#' Get optimal value of eps according to MinPts kNNdist
#'
#' @param sessions sessions data set in standard format
#' @param MinPts MinPts value
#'
#' @return numeric value
#' @export
#'
#' @importFrom dbscan kNNdist
#' @importFrom ecp e.divisive
get_eps <- function(sessions, MinPts) {
  sessions[["ConnectionStartDateTime"]] <- convert_time_dt_to_plot_num(sessions[["ConnectionStartDateTime"]])
  kNNdist <- dbscan::kNNdist(sessions[c("ConnectionStartDateTime", "ConnectionHours")], k = MinPts)
  elbows <- ecp::e.divisive(diff(matrix(kNNdist)), k = 1, min.size = 2)$estimates
  elbow <- elbows[length(elbows)]
  kNNdist[elbow]
}

#' Plot kNNdist
#'
#' @param sessions sessions data set in standard format
#' @param MinPts MinPts value
#' @param MinPts_pct MinPts percentage
#'
#' @return plot
#' @export
#'
#' @importFrom dbscan kNNdist
plot_kNNdist <- function(sessions, MinPts = NULL, MinPts_pct = 0.01) {
  if (is.null(MinPts)) {
    MinPts <- get_MinPts(sessions, MinPts_pct)
  }
  sessions[["ConnectionStartDateTime"]] <- convert_time_dt_to_plot_num(sessions[["ConnectionStartDateTime"]])
  ggplot(
    tibble(
      x = 1:nrow(sessions),
      dist = sort(kNNdist(sessions[c("ConnectionStartDateTime", "ConnectionHours")], k = MinPts))
    ), aes_string("x", "dist")
  ) +
    geom_line() +
    labs(x = "Points sorted by distance", y = paste0(MinPts, "-NN distance"))
  # return(dbscan::kNNdistplot(sessions[c("ConnectionStartDateTime", "ConnectionHours")], k = MinPts))
}



#' Get the minPts and eps values for DBSCAN to label only a specific percentage as noise
#'
#' @param sessions sessions data set in standard format
#' @param MinPts DBSCAN MinPts parameter
#' @param eps0 DBSCAN eps parameter corresponding to the elbow of kNN dist plot
#' @param noise_th noise treshold
#' @param eps_offset_pct eps_offset_pct
#' @param eps_inc_pct eps_inc_pct
#'
#' @return tibble with minPts and eps parameters, and the corresponding noise
#' @export
#'
#' @importFrom dbscan dbscan
#' @importFrom dplyr tibble bind_rows arrange sym
#'
get_dbscan_params <- function(sessions, MinPts, eps0, noise_th = 2, eps_offset_pct = 0.9, eps_inc_pct = 0.02) {
  sessions_cluster <- sessions[,c("ConnectionStartDateTime", "ConnectionHours")]
  sessions_cluster["ConnectionStartDateTime"] <- convert_time_dt_to_plot_num(sessions_cluster[["ConnectionStartDateTime"]])
  noise_table <- tibble()

  # Try to achieve noise threshold decreasing eps
  eps_offset <- round(eps0*eps_offset_pct, 1)
  eps_inc <- round(eps_offset*eps_inc_pct, 3)

  noise_table <- tibble()

  for (eps_rest in seq(0, eps_offset, eps_inc)) {
    eps <- eps0 - eps_rest
    if (eps <= 0) break
    sessions_cluster <- sessions[,c("ConnectionStartDateTime", "ConnectionHours")]
    sessions_cluster["ConnectionStartDateTime"] <- convert_time_dt_to_plot_num(sessions_cluster[["ConnectionStartDateTime"]])
    dbscan_clusters <- dbscan::dbscan(sessions_cluster, eps, MinPts)
    noise <- round(sum(dbscan_clusters$cluster == 0)/length(dbscan_clusters$cluster)*100, 2)
    noise_table <- bind_rows(noise_table, c(MinPts = MinPts, eps = eps, noise = noise))
    noise_table <- arrange(noise_table, !!sym("noise"))
    if ((noise_table$noise[1] <= noise_th) & (noise_table$noise[nrow(noise_table)] >= noise_th)) break
  }

  if (noise_table$noise[1] <= noise_th) {
    if (noise_table$noise[nrow(noise_table)] >= noise_th) {
      # Noise threshold achieved
      return( noise_table[noise_table$noise >= noise_th, ][1, ] )
    } else {
      print("Not enought noise. Consider a higher value of MinPts or eps.")
      return(NULL)
    }
  } else {
    print("Too much nosie. Consider a lower value of MinPts.")
    return(NULL)
  }
}

#' Plot outlying sessions
#'
#' @param sessions sessions data set in standard format
#'
#' @return ggplot2 plot
#' @export
#'
#' @importFrom ggplot2 ggplot aes_string geom_point scale_x_datetime theme_light scale_color_manual
#'
plot_outliers <- function(sessions) {
  sessions["ConnectionStartDateTime"] <- convert_time_dt_to_plot_dt(sessions[["ConnectionStartDateTime"]])
  ggplot(sessions, aes_string(x="ConnectionStartDateTime", y="ConnectionHours", color = "Outlier")) +
    geom_point(size = 0.5) +
    scale_x_datetime(date_labels = '%H:%M', date_breaks = '4 hour') +
    labs(x='Connection start time', y='Number of connection hours', color = "") +
    theme_light() +
    scale_color_manual(labels = c("Normal", "Outlier"), values = c("black", "grey"))
}

#' Detect outliers
#'
#' @param sessions sessions data set in standard format
#' @param MinPts MinPts parameter for DBSCAN clustering
#' @param eps eps parameter for DBSCAN clustering
#'
#' @return sessions data set with extra boolean column `Outlier`
#' @export
#'
#' @importFrom dbscan dbscan
#'
detect_outliers <- function(sessions, MinPts, eps) {
  sessions_cluster <- sessions[,c("ConnectionStartDateTime", "ConnectionHours")]
  sessions_cluster["ConnectionStartDateTime"] <- convert_time_dt_to_plot_num(sessions_cluster[["ConnectionStartDateTime"]])
  dbscan_clusters <- dbscan::dbscan(sessions_cluster, eps, MinPts)
  sessions[["Outlier"]] <- dbscan_clusters$cluster == 0
  return( sessions )
}




