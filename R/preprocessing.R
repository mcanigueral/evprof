

# Cut outliers ----------------------------------------------------------

#' Cut outliers based on minimum and maximum limits of ConnectionHours and ConnectionStartDateTime variables
#'
#' @param sessions tibble, sessions data set in evprof standard format
#' @param connection_hours_min numeric, minimum of connection hours (duration). If NA the minimum value is considered.
#' @param connection_hours_max numeric, maximum of connection hours (duration). If NA the maximum value is considered.
#' @param connection_start_min numeric, minimum hour of connection start (hour as numeric). If NA the minimum value is considered.
#' @param connection_start_max numeric, maximum hour of connection start (hour as numeric). If NA the maximum value is considered.
#' @param log logical, whether to transform `ConnectionStartDateTime` and
#' `ConnectionHours` variables to natural logarithmic scale (base = `exp(1)`).
#' @param start integer, start hour in the x axis of the plot.
#'
#' @returns session dataframe
#' @export
#'
#' @importFrom dplyr between
#'
#' @examples
#' library(dplyr)
#' # Localize the outlying sessions above a certain threshold
#' california_ev_sessions %>%
#'   sample_frac(0.05) %>%
#'   plot_points(start = 3)
#'
#' # For example sessions that start before 5 AM or that are
#' # longer than 20 hours are considered outliers
#' sessions_clean <- california_ev_sessions %>%
#'   sample_frac(0.05) %>%
#'   cut_sessions(
#'     start = 3,
#'     connection_hours_max = 20,
#'     connection_start_min = 5
#'   )
#' plot_points(sessions_clean, start = 3)
#'
cut_sessions <- function(sessions,
                         connection_hours_min = NA, connection_hours_max = NA,
                         connection_start_min = NA, connection_start_max = NA,
                         log = FALSE, start = getOption("evprof.start.hour")) {

  if (log) {
    sessions_log <- mutate_to_log(sessions, start)
    connection_hours <- sessions_log[['ConnectionHours']]
    connection_start <- sessions_log[['ConnectionStartDateTime']]
  } else {
    connection_hours <- sessions[['ConnectionHours']]
    connection_start <- convert_time_dt_to_plot_num(sessions[['ConnectionStartDateTime']], start)
  }

  if (is.na(connection_hours_min))
    connection_hours_min <- min(connection_hours)
  if (is.na(connection_hours_max))
    connection_hours_max <- max(connection_hours)
  if (is.na(connection_start_min))
    connection_start_min <- min(connection_start)
  if (is.na(connection_start_max))
    connection_start_max <- max(connection_start)

  connection_hours_idx <- between(connection_hours, connection_hours_min, connection_hours_max)
  connection_start_idx <- between(connection_start, connection_start_min, connection_start_max)

  return(
    sessions[connection_hours_idx & connection_start_idx, ]
  )
}


# DBSCAN Clustering -------------------------------------------------------

#' Plot kNNdist
#'
#' Plot the kNN (k-nearest neighbors) distance plot to visually detect the
#' "elbow" and define an appropriate value for `eps` DBSCAN parameter.
#'
#' @param sessions tibble, sessions data set in evprof standard format
#' @param MinPts integer, DBSCAN MinPts parameter. If null, a value of 200 will be considered.
#' @param log logical, whether to transform `ConnectionStartDateTime` and
#' `ConnectionHours` variables to natural logarithmic scale (base = `exp(1)`).
#' @param start integer, start hour in the x axis of the plot.
#'
#' @returns plot
#' @export
#'
#' @details
#' The kNN (k-nearest neighbors) distance plot can provide insights into
#' setting the `eps` parameter in DBSCAN. The "elbow" in the kNN distance plot
#' is the point where the distances start to increase significantly. At the
#' same time, for DBSCAN, the eps parameter defines the radius within which a
#' specified number of points must exist for a data point to be considered a
#' core point. Therefore, the "elbow" of the kNN distance plot can provide a
#' sense of the scale of the data and help you choose a reasonable range for
#' the `eps` parameter in DBSCAN.
#'
#'
#' @importFrom ggplot2 ggplot aes geom_line labs
#' @importFrom dbscan kNNdist
#'
#' @examples
#' library(dplyr)
#' california_ev_sessions %>%
#'   sample_frac(0.05) %>%
#'   plot_kNNdist(start = 3, log = TRUE)
#'
plot_kNNdist <- function(sessions, MinPts = NULL, log = FALSE,
                         start = getOption("evprof.start.hour")) {
  if (log) {
    sessions <- mutate_to_log(sessions, start)
  } else {
    sessions[["ConnectionStartDateTime"]] <- convert_time_dt_to_plot_num(sessions[["ConnectionStartDateTime"]], start)
  }
  if (is.null(MinPts)) {
    MinPts <- 200
  }
  ggplot(
    tibble(
      x = seq_len(nrow(sessions)),
      dist = sort(kNNdist(sessions[c("ConnectionStartDateTime", "ConnectionHours")], k = MinPts))
    ), aes(.data[["x"]], .data[["dist"]])
  ) +
    geom_line() +
    labs(x = "Points sorted by distance", y = paste0(MinPts, "-NN distance"))
  # return(dbscan::kNNdistplot(sessions[c("ConnectionStartDateTime", "ConnectionHours")], k = MinPts))
}



#' Get the minPts and eps values for DBSCAN to label only a specific percentage as noise
#'
#' @param sessions tibble, sessions data set in evprof standard format
#' @param MinPts DBSCAN MinPts parameter
#' @param eps0 DBSCAN eps parameter corresponding to the elbow of kNN dist plot
#' @param noise_th noise threshold
#' @param eps_offset_pct eps_offset_pct
#' @param eps_inc_pct eps_inc_pct
#' @param log logical, whether to transform `ConnectionStartDateTime` and
#' `ConnectionHours` variables to natural logarithmic scale (base = `exp(1)`).
#' @param start integer, start hour in the x axis of the plot.
#'
#' @returns tibble with minPts and eps parameters, and the corresponding noise
#' @export
#'
#' @importFrom dbscan dbscan
#' @importFrom dplyr tibble bind_rows arrange sym
#'
get_dbscan_params <- function(sessions, MinPts, eps0, noise_th = 2,
                              eps_offset_pct = 0.9, eps_inc_pct = 0.02,
                              log = FALSE, start = getOption("evprof.start.hour")) {
  if (log) {
    sessions <- mutate_to_log(sessions, start)
  } else {
    sessions[["ConnectionStartDateTime"]] <- convert_time_dt_to_plot_num(sessions[["ConnectionStartDateTime"]], start)
  }

  noise_table <- tibble()

  # Try to achieve noise threshold decreasing eps
  eps_offset <- round(eps0*eps_offset_pct, 1)
  eps_inc <- round(eps_offset*eps_inc_pct, 3)

  for (eps_rest in seq(0, eps_offset, eps_inc)) {
    eps <- eps0 - eps_rest
    if (eps <= 0) break
    sessions_cluster <- sessions[,c("ConnectionStartDateTime", "ConnectionHours")]
    dbscan_clusters <- dbscan::dbscan(sessions_cluster, eps, MinPts)
    noise <- round(sum(dbscan_clusters$cluster == 0)/length(dbscan_clusters$cluster)*100, 2)
    noise_table <- bind_rows(noise_table, c(MinPts = MinPts, eps = eps, noise = noise))
    noise_table <- arrange(noise_table, !!sym("noise"))
    if ((noise_table$noise[1] <= noise_th) & (noise_table$noise[nrow(noise_table)] >= noise_th)) break
  }

  if (noise_table$noise[1] <= noise_th) {
    if (noise_table$noise[nrow(noise_table)] >= noise_th) {
      # Noise threshold achieved
      return( as.list(noise_table[noise_table$noise >= noise_th, ][1, ]) )
    } else {
      message(paste0("Not enought noise (", noise_table$noise[nrow(noise_table)] ," %). Consider a lower eps"))
      return( 1 )
    }
  } else {
    message(paste0("Too much nosie (", noise_table$noise[1] ," %). Consider a higher eps."))
    return( 2 )
  }
}

#' Detect outliers
#'
#' @param sessions tibble, sessions data set in evprof standard format
#' @param MinPts MinPts parameter for DBSCAN clustering
#' @param eps eps parameter for DBSCAN clustering
#' @param noise_th noise threshold
#' @param log logical, whether to transform `ConnectionStartDateTime` and
#' `ConnectionHours` variables to natural logarithmic scale (base = `exp(1)`).
#' @param start integer, start hour in the x axis of the plot.
#'
#' @returns sessions tibble with extra boolean column `Outlier`
#' @export
#'
#' @importFrom dbscan dbscan
#'
#' @examples
#' library(dplyr)
#' sessions_outliers <- california_ev_sessions %>%
#'   sample_frac(0.05) %>%
#'   detect_outliers(start = 3, noise_th = 5, eps = 2.5)
#'
detect_outliers <- function(sessions, MinPts=NULL, eps=NULL, noise_th = 2,
                            log = FALSE, start = getOption("evprof.start.hour")) {

  if (is.null(MinPts) | is.null(eps)) {
    if (is.null(MinPts)) MinPts <- 200
    if (is.null(eps)) {
      if (log) eps <- 0.1 else eps <- 1.5
    }
  }
  dbscan_params <- 0
  while (!is.list(dbscan_params)) {
    if (dbscan_params == 1) {
      # message("Solution not found. Decreasing eps and trying again.")
      eps <- eps/2
    } else if (dbscan_params == 2) {
      # message("Solution not found. Increasing eps and trying again.")
      eps <- eps*1.5
    }
    message(paste("Trying with MinPts =", MinPts, "and eps =", eps))
    dbscan_params <- get_dbscan_params(sessions, MinPts = MinPts, eps0 = eps, noise_th = noise_th, log = log, start = start)
  }

  message(paste("Solution found: MinPts=", dbscan_params$MinPts, ", eps =", dbscan_params$eps))

  sessions_cluster <- sessions[,c("ConnectionStartDateTime", "ConnectionHours")]
  if (log) {
    sessions_cluster <- mutate_to_log(sessions_cluster, start)
  } else {
    sessions_cluster[["ConnectionStartDateTime"]] <- convert_time_dt_to_plot_num(sessions_cluster[["ConnectionStartDateTime"]], start)
  }

  dbscan_clusters <- dbscan::dbscan(sessions_cluster, dbscan_params$eps, dbscan_params$MinPts)
  sessions[["Outlier"]] <- dbscan_clusters$cluster == 0
  return( sessions )
}



#' Drop outliers
#'
#' @param sessions tibble, sessions data set in evprof standard format
#'
#' @returns sessions without outliers nor column `Outlier`
#' @export
#'
#' @importFrom dplyr filter select %>%
#' @importFrom rlang .data
#'
#' @examples
#' library(dplyr)
#' sessions_outliers <- california_ev_sessions %>%
#'   sample_frac(0.05) %>%
#'   detect_outliers(start = 3, noise_th = 5, eps = 2.5)
#'
#' plot_outliers(sessions_outliers, start = 3)
#'
#' sessions_clean <- drop_outliers(sessions_outliers)
#'
#' plot_points(sessions_clean, start = 3)
#'
#'
drop_outliers <- function(sessions) {
  sessions %>%
    filter(!.data$Outlier) %>%
    select(- "Outlier")
}


#' Plot outlying sessions
#'
#' @param sessions tibble, sessions data set in evprof standard format
#' @param start integer, start hour in the x axis of the plot.
#' @param log logical, whether to transform `ConnectionStartDateTime` and
#' `ConnectionHours` variables to natural logarithmic scale (base = `exp(1)`).
#' @param ... arguments to pass to function ggplot2::plot_point
#'
#' @returns ggplot2 plot
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_point scale_x_datetime theme_light scale_color_manual guides guide_legend
#'
#' @examples
#' library(dplyr)
#' sessions_outliers <- california_ev_sessions %>%
#'   sample_frac(0.05) %>%
#'   detect_outliers(start = 3, noise_th = 5, eps = 2.5)
#' plot_outliers(sessions_outliers, start = 3)
#' plot_outliers(sessions_outliers, start = 3, log = TRUE)
#'
plot_outliers <- function(sessions, start=getOption("evprof.start.hour"), log = FALSE, ...) {
  outliers_pct <- round(sum(sessions[['Outlier']])/nrow(sessions)*100, 2)
  if (log) {
    sessions <- mutate_to_log(sessions, start)
  } else {
    sessions[["ConnectionStartDateTime"]] <- convert_time_dt_to_plot_dt(sessions[["ConnectionStartDateTime"]], start)
  }
  plot <- ggplot(sessions, aes(x=.data[["ConnectionStartDateTime"]], y=.data[["ConnectionHours"]], color = .data[["Outlier"]])) +
    geom_point(...) +
    labs(x='Connection start time', y='Number of connection hours', color = "",
         subtitle = paste('Outliers level:', outliers_pct, '%')) +
    theme_light() +
    scale_color_manual(labels = c("Normal", "Outlier"), values = c("black", "grey")) +
    guides(color = guide_legend(override.aes = list(size = 2)))
  if (log) {
    plot
  } else {
    plot + scale_x_datetime(date_labels = '%H:%M', date_breaks = '4 hour')
  }
}


# Data division -----------------------------------------------------------

#' {ggplot2} type function to plot a division line
#'
#' @param day_n Number of the day below the line
#' @param division_hour Hour to divide the groups according to disconnection time
#'
#' @returns ggplot2 function
#' @keywords internal
#'
#' @importFrom ggplot2 geom_line aes
#' @importFrom dplyr tibble
#' @importFrom lubridate hours days today
#' @importFrom rlang .data
#'
get_division_line <- function(day_n, division_hour) {
  geom_line(data = tibble(
    "dt" = seq.POSIXt(from = today() + hours(division_hour), to = today() + days(1) + hours(division_hour), by = "hour"),
    "line" = as.numeric(difftime(today()+days(day_n) + hours(division_hour), .data$dt, units = "hours"))
  ), aes(.data[["dt"]], .data[["line"]]), linewidth = 1, color = "red", linetype = "dashed")
}

#' Iteration over evprof::plot_division_line function to plot multiple lines
#'
#' @param ggplot_points ggplot2 returned by evprof::plot_points function
#' @param n_lines number of lines to plot
#' @param division_hour Hour to divide the groups according to disconnection time
#'
#' @returns ggplot2 function
#' @export
#'
#' @examples
#' library(dplyr)
#' california_ev_sessions %>%
#'   sample_frac(0.05) %>%
#'   plot_points(start = 3) %>%
#'   plot_division_lines(n_lines = 1, division_hour = 5)
#'
plot_division_lines <- function(ggplot_points, n_lines, division_hour) {
  ggplot_points_lines <- ggplot_points
  for (d in 1:n_lines) {
    ggplot_points_lines <- ggplot_points_lines + get_division_line(d, division_hour)
  }
  return(ggplot_points_lines)
}

#' Divide sessions by disconnection day
#'
#' @param sessions tibble, sessions data set in evprof standard format
#' @param division_hour Hour to divide the groups according to disconnection time
#' @param start integer, start hour in the x axis of the plot.
#'
#' @returns same sessions data set with extra column "Disconnection"
#' @export
#'
#' @importFrom dplyr select
#' @importFrom lubridate days hours today
#'
#' @examples
#' library(dplyr)
#' sessions_disconnection <- california_ev_sessions %>%
#'   sample_frac(0.05) %>%
#'   divide_by_disconnection(
#'     start = 2, division_hour = 5
#'   )
#'
#' # The column `Disconnection` has been added
#' names(sessions_disconnection)
#'
#' library(ggplot2)
#' sessions_disconnection %>%
#'   tidyr::drop_na() %>%
#'   plot_points() +
#'   facet_wrap(vars(Disconnection))
#'
divide_by_disconnection <- function(sessions, division_hour, start = getOption("evprof.start.hour")) {
  sessions$StartTime <- convert_time_dt_to_plot_dt(sessions$ConnectionStartDateTime, start)
  sessions$EndTime <- sessions$StartTime + convert_time_num_to_period(sessions$ConnectionHours)
  sessions$Disconnection <- NA

  n_max_disconnection_days <- ceiling(max(sessions$ConnectionHours)/24) + 1

  for (day in 1:n_max_disconnection_days) {
    sessions$Disconnection[
      (sessions$EndTime > today() + days(day-1) + hours(division_hour)) &
        (sessions$EndTime <= today() + days(day) + hours(division_hour))
    ] <- day
  }

  return( select(sessions, -c("StartTime", "EndTime")) )
}

#' Divide sessions by time-cycle
#'
#' @param sessions tibble, sessions data set in evprof standard format
#' @param months_cycles list containing Monthly cycles
#' @param wdays_cycles list containing Weekdays cycles
#' @param start integer, start hour in the x axis of the plot.
#'
#' @returns same sessions data set with extra column "Timecycle"
#' @export
#'
#' @importFrom purrr pmap
#' @importFrom dplyr filter select everything
#' @importFrom rlang .data
#' @importFrom lubridate month wday
#'
#' @examples
#' library(dplyr)
#' sessions_timecycles <- california_ev_sessions %>%
#'   sample_frac(0.05) %>%
#'   divide_by_timecycle(
#'     months_cycles = list(1:12),
#'     wdays_cycles = list(1:5, 6:7)
#'   )
#'
#' # The column `Timecycle` has been added
#' names(sessions_timecycles)
#'
#' library(ggplot2)
#' plot_points(sessions_timecycles) +
#'   facet_wrap(vars(Timecycle))
#'
divide_by_timecycle <- function(sessions, months_cycles = list(1:12), wdays_cycles = list(1:5, 6:7), start = getOption("evprof.start.hour")) {

  cycles_tbl <- tibble(
    months = rep(months_cycles, each = length(wdays_cycles)),
    wdays = rep(wdays_cycles, times = length(months_cycles))
  )
  print_timecycles_tbl(cycles_tbl)

  # shift daybreak sessions to the corresponding weekday for convert_time_to_plot_time conversion before clustering
  hours <- hour(sessions$ConnectionStartDateTime)
  wdays <- wday(sessions$ConnectionStartDateTime, week_start = 1)
  sessions_backshift_wday <- hours < start
  wdays_with_start <- wdays
  wdays_with_start[sessions_backshift_wday] <- wdays_with_start[sessions_backshift_wday] - 1
  wdays_with_start[wdays_with_start == 0] <- 7

  purrr::pmap_dfr(
    cycles_tbl,
    ~ filter(sessions,
             month(.data$ConnectionStartDateTime) %in% ..1,
             wdays_with_start %in% ..2),
    .id = "Timecycle"
  ) %>%
    select(- "Timecycle", everything(), "Timecycle")
}

print_timecycles_tbl <- function(cycles_tbl) {
  timecycles_tbl <- purrr::pmap_dfr(
    cycles_tbl,
    ~ dplyr::tibble(
      months = ifelse(length(..1) > 1, paste(..1[1], ..1[length(..1)], sep = "-"), as.character(..1)),
      wdays = ifelse(length(..2) > 1, paste(..2[1], ..2[length(..2)], sep = "-"), as.character(..2))
    ),
    .id = "Timecycle"
  )

  message("The considered time-cycles are:")

  if (requireNamespace("utils", quietly = TRUE) & requireNamespace("knitr", quietly = TRUE)) {
    message(paste0(
      utils::capture.output(knitr::kable(timecycles_tbl)),
      collapse = "\n"
    ))
  } else {
    message("[Warning: install {utils} and {knitr} to see the table.]")
  }
}
