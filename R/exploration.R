
# Time conversion functions -----------------------------------------------

#' Convert datetime value to numeric (hour-based)
#'
#' @param time_dt Datetime value
#'
#' @importFrom lubridate hour minute
#'
convert_time_dt_to_num <- function(time_dt) {
  hour(time_dt) + minute(time_dt)/60
}

#' Covnert datetime value to rounded numeric (hour-based)
#'
#' @param time_dt Datetime value
#' @param interval Time interval of the time sequence. It can be 0.5 (30 minutes) or 1 (1 hour).
#'
#' @importFrom lubridate hour<- minute<-
#'
convert_time_dt_to_rounded_dt <- function(time_dt, interval=0.5) {
  if (interval == 0.5) {
    hour_round <- round_to_half(convert_time_dt_to_num(time_dt))
    hour(time_dt) <- hour_round %/% 1
    minute(time_dt) <- (hour_round %% 1)*60
  }
  if (interval == 1) {
    hour(time_dt) <- round(convert_time_dt_to_num(time_dt))
    minute(time_dt) <- 0
  }
  time_dt
}

#' Convert numeric time value (hour-based) to character hour in %H:%M format
#'
#' @param time_num Numeric time value (hour-based)
#'
#' @importFrom lubridate as_datetime minutes
#'
convert_time_num_to_chr <- function(time_num) {
  strftime(
    as_datetime(hours(time_num%/%1) + minutes(round(time_num%%1*60))),
    format = '%H:%M', tz = getOption("evprof.tzone")
  )
}

#' Convert numeric time value to sorted factor considering a start time
#'
#' @param time_num Numeric time value (hour-based)
#' @param interval Time interval of the time sequence. It can be 0.5 (30 minutes) or 1 (1 hour).
#' @param start Start hour (int)
#'
convert_time_num_to_plot_factor <- function(time_num, interval=0.5, start=getOption("evprof.start.hour")) {
  if (interval == 0.5) dt_to_num = convert_time_num_to_chr(round_to_half(time_num))
  if (interval == 1) dt_to_num = convert_time_num_to_chr(round(time_num))
  factor(
    dt_to_num,
    levels = convert_time_num_to_chr(c(seq(start, 23.5, interval), seq(0, start-0.5, interval)))
  )
}

#' Modify datetime values according to time sequence start time
#'
#' @param time_dt Datetime value
#' @param start Start hour (int)
#'
#' @importFrom lubridate date<- date hour days
#'
convert_time_dt_to_plot_dt <- function(time_dt, start=getOption("evprof.start.hour")) {
  date(time_dt) <- Sys.Date()
  next_day_idx <- seq(1, length(time_dt))[(hour(time_dt) < start)]
  date(time_dt)[next_day_idx] <- date(time_dt)[next_day_idx] + days(1)
  time_dt
}

#' Modify numeric time value according to a time sequence start time
#'
#' @param time_num Numeric time value (hour-based)
#' @param start Start hour (int)
#'
convert_time_num_to_plot_num <- function(time_num, start=getOption("evprof.start.hour")) {
  time_num[time_num < start] <- time_num[time_num < start] + 24
  time_num
}

#' Convert numeric time value to a datetime period (hour-based)
#'
#' @param time_num Numeric time value (hour-based)
#'
#' @importFrom lubridate hours minutes
#'
convert_time_num_to_period <- function(time_num) {
  h <- time_num %/% 1
  m <- (time_num - h)*60 %/% 1
  hours(as.integer(h)) + minutes(as.integer(m))
}

#' Convert datetime value to sorted factor considering a start time
#'
#' @param time_dt Datetime value
#' @param interval Time interval of the time sequence. It can be 0.5 (30 minutes) or 1 (1 hour).
#' @param start Start hour (int)
#'
convert_time_dt_to_plot_factor <- function(time_dt, interval=0.5, start=getOption("evprof.start.hour")) {
  convert_time_num_to_plot_factor(convert_time_dt_to_num(time_dt), interval, start)
}

#' Convert datetime values to sorted numeric values considering a start time
#'
#' @param time_dt Datetime value
#' @param start Start hour (int)
#'
convert_time_dt_to_plot_num <- function(time_dt, start=getOption("evprof.start.hour")) {
  convert_time_num_to_plot_num(convert_time_dt_to_num(time_dt), start)
}

#' Round numeric time value to half hour basis.
#'
#' @param time_num Numeric time value (hour-based)
#'
round_to_half <- function(time_num) {
  round(time_num*2)/2
}

# convert_datetime_dt_to_num <- function(time_dt, date_0) {
#   date_offset <- as.numeric(date(time_dt) - date_0, units = "hours")
#   hour(time_dt) + minute(time_dt)/60 + date_offset
# }


# General sessions' plots -------------------------------------------------

#' Scatter plot of sessions
#'
#' @param sessions sessions data set in standard format.
#' @param start start hour (int)
#' @param ... arguments to `ggplot2::geom_point` function
#'
#' @export
#'
#' @importFrom ggplot2 ggplot aes_string geom_point scale_x_datetime labs theme_light
#'
plot_points <- function(sessions, start=getOption("evprof.start.hour"), ...) {
  sessions["ConnectionStartDateTime"] <- convert_time_dt_to_plot_dt(sessions[["ConnectionStartDateTime"]], start)
  ggplot(sessions, aes_string(x="ConnectionStartDateTime", y="ConnectionHours")) +
    geom_point(...) +
    scale_x_datetime(date_labels = '%H:%M', date_breaks = '4 hour') +
    labs(x='Connection start time', y='Number of connection hours', color='Cluster') +
    theme_light()
}

#' Density plot in 2D, considering Start time and Connection duration as variables
#'
#' @param sessions sessions data set in standard format
#' @param bins parameter to pass to `ggplot2::stat_density_2d`
#' @param start start hour (int)
#'
#' @return ggplot2 plot
#' @export
#'
#' @importFrom ggplot2 aes_string stat_density2d scale_fill_viridis_c scale_x_datetime xlab ylab theme_light stat
#' @importFrom rlang .data
#'
plot_density_2D <- function(sessions, bins=15, start=getOption("evprof.start.hour")) {
  sessions["ConnectionStartDateTime"] <- convert_time_dt_to_plot_dt(sessions[["ConnectionStartDateTime"]], start)
  ggplot(sessions, aes_string(x="ConnectionStartDateTime", y="ConnectionHours")) +
    stat_density2d(geom = "polygon", aes(fill = stat(.data$nlevel)), bins = bins) +
    scale_fill_viridis_c(name = 'Density of \nsessions\n') +
    scale_x_datetime(date_labels = '%H:%M', date_breaks = '4 hour') +
    xlab('\nSession start time') + ylab('Number of connection hours\n') +
    theme_light()
}

#' Density plot in 3D, considering Start time and Connection duration as variables
#'
#' @param sessions sessions data set in standard format
#' @param start start hour (int)
#' @param eye list containing x, y and z points of view. Example: `list(x = -0.75, y = -2, z = 0.5)`
#'
#' @return plotly plot (html)
#' @export
#'
#' @importFrom MASS kde2d
#' @importFrom plotly plot_ly add_surface layout hide_colorbar
#' @importFrom dplyr %>%
#'
plot_density_3D <- function(sessions, start=getOption("evprof.start.hour"), eye = list(x = -0.75, y = -2, z = 0.5)) {
  density <- MASS::kde2d(convert_time_dt_to_plot_num(sessions[["ConnectionStartDateTime"]], start), sessions[["ConnectionHours"]])
  plot_ly(x = density$x, y = density$y, z = t(density$z)) %>%
    add_surface() %>%
    layout(
      scene = list(
        xaxis = list(title = "Connection start time", titlefont = list(size = 12), tickfont = list(size = 10)),
        yaxis = list(title = "Connection hours", titlefont = list(size = 12), tickfont = list(size = 10)),
        zaxis = list(title = "Density of sessions", titlefont = list(size = 12), tickfont = list(size = 10)),
        camera = list(eye = eye)
      )) %>%
    hide_colorbar()
}

#' Statistic summary of sessions features
#'
#' @param sessions sessions data set in standard format
#' @param .funs A function `fun`
#' @param vars variables to compute the histogram for
#'
#' @return Summary table
#' @export
#'
#' @importFrom dplyr %>% select summarise_all
#'
summarise_sessions <- function(sessions, .funs, vars = evprof::sessions_summary_feature_names) {
  print(sessions)
  sessions %>%
    select(vars) %>%
    summarise_all(.funs)
}

#' Histogram of a variable from sessions data set
#'
#' @param sessions sessions data set in standard format
#' @param var variable to compute the histogram for
#' @param binwidth with of histogram bins
#'
#' @return ggplot plot
#' @export
#'
#' @importFrom ggplot2 ggplot aes_string aes geom_histogram stat theme_light
#' @importFrom rlang .data
#'
plot_histogram <- function(sessions, var, binwidth=1) {
  ggplot(sessions, aes_string(x=var)) +
    geom_histogram(aes(y=stat(.data$count)/sum(stat(.data$count))*100),
                   binwidth = binwidth, color = 'navy', fill = 'navy', alpha = 0.7) +
    labs(x = "", y = "Count (%)", title = var) +
    theme_light()
}



#' Title
#'
#' @param sessions sessions data set in standard format
#' @param vars variables to plot
#' @param binwidths bindwidths of each variable histogram
#' @param nrow number of rows of the plot grid
#' @param ncol number of columns of the plot grid
#'
#' @return grid plot
#' @export
#'
#' @importFrom purrr map2
#' @importFrom cowplot plot_grid
#'
plot_histogram_grid <- function(sessions, vars=evprof::sessions_summary_feature_names, binwidths=rep(1, length(vars)), nrow = NULL, ncol = NULL) {
  hist_list <- purrr::map2(vars, binwidths, ~ plot_histogram(sessions, .x, .y))
  cowplot::plot_grid(plotlist = hist_list, nrow = nrow, ncol = ncol)
}
