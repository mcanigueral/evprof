

# Cut outliers ----------------------------------------------------------

#' Cut outliers based on minimum and maximum limits of ConnectionHours and ConnectionStartDateTime variables
#'
#' @param sessions sessions data set in standard format
#' @param connection_hours_min numeric, minimum of connection hours (duration). If NA the minimum value is considered.
#' @param connection_hours_max numeric, maximum of connection hours (duration). If NA the maximum value is considered.
#' @param connection_start_min numeric, minimum hour of connection start (hour as numeric). If NA the minimum value is considered.
#' @param connection_start_max numeric, maximum hour of connection start (hour as numeric). If NA the maximum value is considered.
#' @param log Logical. Whether to transform ConnectionStartDateTime and ConnectionHours variables to natural logarithmic scale (base = `exp(1)`).
#'
#' @return session dataframe
#' @export
#'
#' @importFrom dplyr between
#'
cut_sessions <- function(sessions,
                           connection_hours_min = NA, connection_hours_max = NA,
                           connection_start_min = NA, connection_start_max = NA,
                           log = FALSE) {

  if (log) {
    sessions_log <- mutate_to_log(sessions)
    connection_hours <- sessions_log[['ConnectionHours']]
    connection_start <- sessions_log[['ConnectionStartDateTime']]
  } else {
    connection_hours <- sessions[['ConnectionHours']]
    connection_start <- convert_time_dt_to_plot_num(sessions[['ConnectionStartDateTime']])
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


#' Get MinPts value according to percentage of sessions data set size (rows)
#'
#' @param sessions sessions data set in standard format
#' @param pct percentage of rows to consider
#'
#' @return integer value
#' @export
#'
get_MinPts <- function(sessions, pct=0.001) {
  round(pct*nrow(sessions))
}

#' Get optimal value of eps according to MinPts kNNdist
#'
#' @param sessions sessions data set in standard format
#' @param MinPts MinPts value
#'
#' @return numeric value
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
#' @param log Logical. Whether to transform ConnectionStartDateTime and ConnectionHours variables to natural logarithmic scale (base = `exp(1)`).
#'
#' @return plot
#' @export
#'
#' @importFrom dbscan kNNdist
plot_kNNdist <- function(sessions, MinPts = NULL, MinPts_pct = 0.001, log = FALSE) {
  if (log) {
    sessions <- mutate_to_log(sessions)
  } else {
    sessions[["ConnectionStartDateTime"]] <- convert_time_dt_to_plot_num(sessions[["ConnectionStartDateTime"]])
  }
  if (is.null(MinPts)) {
    # MinPts <- get_MinPts(sessions, MinPts_pct)
    MinPts <- 200
  }
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
#' @param log Logical. Whether to transform ConnectionStartDateTime and ConnectionHours variables to natural logarithmic scale (base = `exp(1)`).
#'
#' @return tibble with minPts and eps parameters, and the corresponding noise
#' @export
#'
#' @importFrom dbscan dbscan
#' @importFrom dplyr tibble bind_rows arrange sym
#'
get_dbscan_params <- function(sessions, MinPts, eps0, noise_th = 2, eps_offset_pct = 0.9, eps_inc_pct = 0.02, log = FALSE) {
  if (log) {
    sessions <- mutate_to_log(sessions)
  } else {
    sessions[["ConnectionStartDateTime"]] <- convert_time_dt_to_plot_num(sessions[["ConnectionStartDateTime"]])
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
      message(paste0("Not enought noise (", noise_table$noise[nrow(noise_table)] ," %). Consider a lower value of eps"))
      return( 1 )
    }
  } else {
    message(paste0("Too much nosie (", noise_table$noise[1] ," %). Consider a higher eps."))
    return( 2 )
  }
}

#' Plot outlying sessions
#'
#' @param sessions sessions data set in standard format
#' @param log Logical. Whether to transform ConnectionStartDateTime and ConnectionHours variables to natural logarithmic scale (base = `exp(1)`).
#' @param ... arguments to pass to function ggplot2::plot_point
#'
#' @return ggplot2 plot
#' @export
#'
#' @importFrom ggplot2 ggplot aes_string geom_point scale_x_datetime theme_light scale_color_manual
#'
plot_outliers <- function(sessions, log = FALSE, ...) {
  outliers_pct <- round(sum(sessions[['Outlier']])/nrow(sessions)*100, 2)
  if (log) {
    sessions <- mutate_to_log(sessions)
  } else {
    sessions[["ConnectionStartDateTime"]] <- convert_time_dt_to_plot_dt(sessions[["ConnectionStartDateTime"]])
  }
  plot <- ggplot(sessions, aes_string(x="ConnectionStartDateTime", y="ConnectionHours", color = "Outlier")) +
    geom_point(...) +
    labs(x='Connection start time', y='Number of connection hours', color = "", title = paste('Outliers level:', outliers_pct, '%')) +
    theme_light() +
    scale_color_manual(labels = c("Normal", "Outlier"), values = c("black", "grey"))
  if (log) {
    plot
  } else {
    plot + scale_x_datetime(date_labels = '%H:%M', date_breaks = '4 hour')
  }
}

#' Detect outliers
#'
#' @param sessions sessions data set in standard format
#' @param MinPts MinPts parameter for DBSCAN clustering
#' @param eps eps parameter for DBSCAN clustering
#' @param noise_th noise treshold
#' @param log Logical. Whether to transform ConnectionStartDateTime and ConnectionHours variables to natural logarithmic scale (base = `exp(1)`).
#'
#' @details If MinPts or eps are NULL, no outliers detection is performed.
#'
#' @return sessions tibble with extra boolean column `Outlier`
#' @export
#'
#' @importFrom dbscan dbscan
#'
detect_outliers <- function(sessions, MinPts=NULL, eps=NULL, noise_th = 2, log = FALSE) {

  if (is.null(MinPts) | is.null(eps)) {
    if (is.null(MinPts)) MinPts <- 200 #MinPts <- get_MinPts(sessions, pct = 0.001)
    if (is.null(eps)) {
      if (log) eps <- 0.15 else eps <- 2 # Before it was 0.07 and 1
    }
    dbscan_params <- 0
    while (!is.data.frame(dbscan_params)) {
      if (dbscan_params == 1) {
        message("Solution not found. Decreasing eps and trying again.")
        MinPts <- MinPts/1.5
      } else if (dbscan_params == 2) {
        message("Solution not found. Increasing eps and trying again.")
        eps <- eps*1.5
      }
      message(paste("Trying with MinPts =", MinPts, "and eps =", eps))
      dbscan_params <- get_dbscan_params(sessions, MinPts = MinPts, eps0 = eps, noise_th = noise_th, log = log)
    }
  }

  message(paste("Solution found: MinPts=", dbscan_params$MinPts, ", eps =", dbscan_params$eps))

  sessions_cluster <- sessions[,c("ConnectionStartDateTime", "ConnectionHours")]
  if (log) {
    sessions_cluster <- mutate_to_log(sessions_cluster)
  } else {
    sessions_cluster[["ConnectionStartDateTime"]] <- convert_time_dt_to_plot_num(sessions_cluster[["ConnectionStartDateTime"]])
  }

  dbscan_clusters <- dbscan::dbscan(sessions_cluster, dbscan_params$eps, dbscan_params$MinPts)
  sessions[["Outlier"]] <- dbscan_clusters$cluster == 0
  return( sessions )
}

#' Drop outliers
#'
#' @param sessions sessions data set in standard format
#'
#' @return sessions without outliers nor column `Outlier`
#' @export
#'
#' @importFrom dplyr filter select %>%
#' @importFrom rlang .data
#'
drop_outliers <- function(sessions) {
  sessions %>%
    filter(!.data$Outlier) %>%
    select(-.data$Outlier)
}


# Data division -----------------------------------------------------------

#' {ggplot2} type function to plot a division line
#'
#' @param day_n Number of the day below the line
#' @param division_hour Hour to divide the groups according to disconnection time
#'
#' @return ggplot2 function
#' @export
#'
#' @importFrom ggplot2 geom_line aes_string
#' @importFrom dplyr tibble
#' @importFrom lubridate force_tz hours days
#' @importFrom rlang .data
#'
get_division_line <- function(day_n, division_hour) {
  geom_line(data = tibble(
    "dt" = seq.POSIXt(from = force_tz(Sys.Date() + hours(division_hour), tzone = getOption("evprof.tzone")), to = force_tz(Sys.Date() + days(1) + hours(division_hour), tzone = getOption("evprof.tzone")), by = "hour"),
    "line" = as.numeric(difftime(force_tz(Sys.Date()+days(day_n) + hours(division_hour), tzone = getOption("evprof.tzone")), .data$dt, units = "hours"))
  ), aes_string("dt", "line"), size = 1, color = "red", linetype = "dashed")
}

#' Iteration over evprof::plot_division_line function to plot multiple lines
#'
#' @param ggplot_points ggplot2 returned by evprof::plot_points function
#' @param n_lines number of lines to plot
#' @param division_hour Hour to divide the groups according to disconnection time
#'
#' @return ggplot2 function
#' @export
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
#' @param sessions sessions data set in standard format
#' @param days number of disconnection days to select
#' @param division_hour Hour to divide the groups according to disconnection time
#'
#' @return same sessions data set with extra column "Disconnection"
#' @export
#'
#' @importFrom purrr map_dfr
#' @importFrom dplyr filter %>% select
#' @importFrom lubridate days hours
#' @importFrom rlang .data
#'
divide_by_disconnection <- function(sessions, days, division_hour) {
  sessions[["StartTime"]] <- convert_time_dt_to_plot_dt(sessions[["ConnectionStartDateTime"]])
  sessions[["EndTime"]] <- sessions[["StartTime"]] + convert_time_num_to_period(sessions[["ConnectionHours"]])

  purrr::map_dfr(
    days,
    ~ filter(
      sessions,
      .data$EndTime > force_tz(Sys.Date() + days(.x-1) + hours(division_hour), tzone = getOption("evprof.tzone")),
      .data$EndTime <= force_tz(Sys.Date() + days(.x) + hours(division_hour), tzone = getOption("evprof.tzone"))
    ) %>%
      select(-c("StartTime", "EndTime")),
    .id = "Disconnection"
  )
}


#' Divide sessions by time-cycle
#'
#' @param sessions sessions data set in standard format
#' @param months_cycles list containing Monthly cycles
#' @param wdays_cycles list containing Weekdays cycles
#'
#' @return same sessions data set with extra column "Timecycle"
#' @export
#'
#' @importFrom purrr pmap
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @importFrom lubridate month wday
#'
divide_by_timecycle <- function(sessions, months_cycles = list(1:12), wdays_cycles = list(1:5, 6:7)) {

  cycles_tbl <- tibble(
    months = rep(months_cycles, each = length(wdays_cycles)),
    wdays = rep(wdays_cycles, times = length(months_cycles))
  )

  # shift daybreak sessions to the corresponding weekday for convert_time_to_plot_time conversion before clustering
  hours <- hour(sessions$ConnectionStartDateTime)
  wdays <- wday(sessions$ConnectionStartDateTime, week_start = 1)
  sessions_backshift_wday <- hours < getOption("evprof.start.hour")
  wdays_with_start <- wdays
  wdays_with_start[sessions_backshift_wday] <- wdays_with_start[sessions_backshift_wday] - 1
  wdays_with_start[wdays_with_start == 0] <- 7

  purrr::pmap_dfr(
    cycles_tbl,
    ~ filter(sessions,
             month(.data$ConnectionStartDateTime) %in% ..1,
             wdays_with_start %in% ..2),
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

