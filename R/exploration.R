

# Numeric operations ------------------------------------------------------

#' Round numeric time value to half hour basis.
#'
#' @param time_num Numeric time value (hour-based)
#'
round_to_half <- function(time_num) {
  round(time_num*2)/2
}

#' Round to nearest interval
#'
#' @param dbl number to round
#' @param interval rounding interval
#'
#' @export
#' @returns numeric value
#'
#' @examples
#' set.seed(1)
#' random_vct <- rnorm(10, 5, 5)
#' round_to_interval(random_vct, 2.5)
#'
round_to_interval <- function(dbl, interval) {
  round(dbl/interval)*interval
}


# Time conversion functions -----------------------------------------------

#' Convert datetime value to numeric (hour-based)
#'
#' @param time_dt Datetime value
#'
#' @importFrom lubridate hour minute second
#'
convert_time_dt_to_num <- function(time_dt) {
  hour(time_dt) + minute(time_dt)/60 + second(time_dt)/3600
}

#' Convert numeric time value (hour-based) to character hour in %H:%M format
#'
#' @param time_num Numeric time value (hour-based)
#'
#' @importFrom lubridate as_datetime hours minutes
#'
convert_time_num_to_chr <- function(time_num) {
  strftime(
    as_datetime(hours(time_num%/%1) + minutes(round(time_num%%1*60))),
    format = '%H:%M', tz = "UTC"
  )
}


#' Modify datetime values according evprof.start.hour
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
  time_num[time_num <= start] <- time_num[time_num <= start] + 24
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


#' Convert datetime values to sorted numeric values considering a start time
#'
#' @param time_dt Datetime value
#' @param start Start hour (int)
#'
convert_time_dt_to_plot_num <- function(time_dt, start=getOption("evprof.start.hour")) {
  convert_time_num_to_plot_num(convert_time_dt_to_num(time_dt), start)
}



# Logarithmic scale -------------------------------------------------------

#' Logarithmic transformation to ConnectionStartDateTime and ConnectionHours variables
#'
#' @param sessions sessions data set in standard format.
#' @param base logarithmic base
#'
mutate_to_log <- function(sessions, base = exp(1)) {
  sessions[["ConnectionStartDateTime"]] <- log(convert_time_dt_to_plot_num(sessions[["ConnectionStartDateTime"]]), base = base)
  sessions[["ConnectionHours"]] <- log(sessions[["ConnectionHours"]], base = base)
  return( sessions )
}


# General sessions' plots -------------------------------------------------

#' Scatter plot of sessions
#'
#' @param sessions tibble, sessions data set in evprof
#' [standard format](https://mcanigueral.github.io/evprof/articles/sessions-format.html).
#' @param start integer, start hour in the x axis of the plot.
#' This is only used when `log = FALSE`.
#' @param log logical, whether to transform `ConnectionStartDateTime` and
#' `ConnectionHours` variables to natural logarithmic scale (base = `exp(1)`).
#' @param ... arguments to `ggplot2::geom_point` function
#'
#' @returns ggplot scatter plot
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_point scale_x_datetime labs theme_light
#'
#' @examples
#' library(dplyr)
#' california_ev_sessions %>% head(3000) %>% plot_points()
#' california_ev_sessions %>% head(3000) %>% plot_points(start = 3)
#' california_ev_sessions %>% head(3000) %>% plot_points(log = TRUE)
#'
plot_points <- function(sessions, start=getOption("evprof.start.hour"), log = FALSE, ...) {
  if (!log) {
    sessions["ConnectionStartDateTime"] <- convert_time_dt_to_plot_dt(sessions[["ConnectionStartDateTime"]], start)
  } else {
    sessions <- mutate_to_log(sessions)
  }

  plot <- ggplot(sessions, aes(x=.data[["ConnectionStartDateTime"]], y=.data[["ConnectionHours"]])) +
    geom_point(...) +
    labs(x='Connection start time', y='Number of connection hours') +
    theme_light()

  if (!log) {
    plot + scale_x_datetime(date_labels = '%H:%M', date_breaks = '4 hour')
  } else {
    plot
  }
}

#' Density plot in 2D, considering Start time and Connection duration as variables
#'
#' @param sessions tibble, sessions data set in evprof
#' [standard format](https://mcanigueral.github.io/evprof/articles/sessions-format.html).
#' @param bins integer, parameter to pass to `ggplot2::stat_density_2d`
#' @param start integer, start hour in the x axis of the plot.
#' This is only used when `log = FALSE`.
#' @param by variable to facet the plot. Character being "wday", "month" or "year", considering the week to start at wday=1.
#' @param log logical, whether to transform `ConnectionStartDateTime` and
#' `ConnectionHours` variables to natural logarithmic scale (base = `exp(1)`).
#'
#' @returns ggplot2 plot
#' @export
#'
#' @importFrom lubridate wday month year
#' @importFrom ggplot2 aes stat_density2d scale_fill_viridis_c scale_x_datetime xlab ylab theme_light after_stat facet_wrap vars
#' @importFrom rlang .data
#'
#' @examples
#' library(dplyr)
#'
#' california_ev_sessions %>%
#'   sample_frac(0.1) %>%
#'   plot_density_2D(by = "wday", start = 3, bins = 15, log = FALSE)
#'
plot_density_2D <- function(sessions, bins=15, by = c("wday", "month", "year"), start=getOption("evprof.start.hour"), log = FALSE) {
  sessions[["wday"]] <- factor(wday(sessions[["ConnectionStartDateTime"]], week_start = 1))
  sessions[["month"]] <- factor(month(sessions[["ConnectionStartDateTime"]]))
  sessions[["year"]] <- factor(year(sessions[["ConnectionStartDateTime"]]))
  if (!log) {
    sessions[["ConnectionStartDateTime"]] <- convert_time_dt_to_plot_dt(sessions[["ConnectionStartDateTime"]], start)
  } else {
    sessions <- mutate_to_log(sessions)
  }
  density_plot <- sessions %>%
    ggplot(aes(x=.data[["ConnectionStartDateTime"]], y=.data[["ConnectionHours"]])) +
    stat_density2d(geom = "polygon", aes(fill = after_stat(.data$nlevel)), bins = bins) +
    scale_fill_viridis_c(name = 'Density of \nsessions\n') +
    # scale_x_datetime(date_labels = '%H:%M', date_breaks = '4 hour') +
    xlab('\nSession start time') + ylab('Number of connection hours\n') +
    theme_light()

  if (by == "wday") {
    hour_breaks <- 6
  } else if (by == "month") {
    hour_breaks <- 8
  } else {
    hour_breaks <- 4
  }

  if (!log) {
    density_plot <- density_plot +
      scale_x_datetime(date_labels = '%H:%M', date_breaks = paste(hour_breaks, 'hour'))
  }

  if (by == "wday") {
    return(
      density_plot + facet_wrap(vars(.data$wday))
    )
  } else if (by == "month") {
    return(
      density_plot + facet_wrap(vars(.data$month))
    )
  } else if (by == "year") {
    return(
      density_plot + facet_wrap(vars(.data$year))
    )
  } else {
    return( density_plot )
  }

}

#' Density plot in 3D, considering Start time and Connection duration as variables
#'
#' @param sessions tibble, sessions data set in evprof
#' [standard format](https://mcanigueral.github.io/evprof/articles/sessions-format.html).
#' @param start integer, start hour in the x axis of the plot.
#' This is only used when `log = FALSE`.
#' @param eye list containing x, y and z points of view. Example: `list(x = -1.5, y = -1.5, z = 1.5)`
#' @param log logical, whether to transform `ConnectionStartDateTime` and
#' `ConnectionHours` variables to natural logarithmic scale (base = `exp(1)`).
#'
#' @returns plotly plot (html)
#' @export
#'
#' @importFrom MASS kde2d
#' @importFrom plotly plot_ly add_surface layout hide_colorbar
#' @importFrom dplyr %>% filter
#'
#' @examples
#' plot_density_3D(california_ev_sessions, start = 3)
#'
plot_density_3D <- function(sessions, start=getOption("evprof.start.hour"), eye = list(x = -1.5, y = -1.5, z = 1.5), log = FALSE) {
  if (!log) {
    sessions["ConnectionStartDateTime"] <- convert_time_dt_to_plot_num(sessions[["ConnectionStartDateTime"]], start)
  } else {
    sessions <- mutate_to_log(sessions)
  }
  sessions <- sessions %>%
    filter(!is.infinite(.data$ConnectionStartDateTime), !is.infinite(.data$ConnectionHours),
           !is.na(.data$ConnectionStartDateTime), !is.na(.data$ConnectionHours))
  density <- MASS::kde2d(sessions[["ConnectionStartDateTime"]], sessions[["ConnectionHours"]])
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
#' @param sessions tibble, sessions data set in evprof
#' [standard format](https://mcanigueral.github.io/evprof/articles/sessions-format.html).
#' [standard format](https://mcanigueral.github.io/evprof/articles/sessions-format.html).
#' @param .funs A function to compute, e.g. `mean`, `max`, etc.
#' @param vars character vector, variables to compute the histogram for
#'
#' @returns Summary table
#' @export
#'
#' @importFrom dplyr %>% select summarise_all any_of
#'
#' @examples
#' summarise_sessions(california_ev_sessions, mean)
#'
#'
summarise_sessions <- function(sessions, .funs, vars = evprof::sessions_summary_feature_names) {
  sessions %>%
    select(any_of(vars)) %>%
    summarise_all(.funs)
}


#' Histogram of a variable from sessions data set
#'
#' @param sessions tibble, sessions data set in evprof
#' [standard format](https://mcanigueral.github.io/evprof/articles/sessions-format.html).
#' @param var character, column name to compute the histogram for
#' @param binwidth integer, with of histogram bins
#'
#' @returns ggplot plot
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_histogram after_stat theme_light
#' @importFrom rlang .data
#' @importFrom tibble tibble
#'
#' @examples
#' plot_histogram(california_ev_sessions, "Power", binwidth = 2)
#' plot_histogram(california_ev_sessions, "Power", binwidth = 0.1)
#'
plot_histogram <- function(sessions, var, binwidth=1) {
  ggplot(sessions, aes(x=.data[[var]])) +
    geom_histogram(aes(y=after_stat(.data$count)/sum(after_stat(.data$count))*100),
                   binwidth = binwidth, color = 'navy', fill = 'navy', alpha = 0.7) +
    labs(x = "", y = "Count (%)", title = var) +
    theme_light()
}



#' Grid of multiple variable histograms
#'
#' @param sessions tibble, sessions data set in evprof
#' [standard format](https://mcanigueral.github.io/evprof/articles/sessions-format.html).
#' @param vars vector of characters, variables to plot
#' @param binwidths vector of integers, binwidths of each variable histogram.
#' The length of the vector must correspond to the length of the `vars` parameter.
#' @param nrow integer, number of rows of the plot grid
#' @param ncol integer, number of columns of the plot grid
#'
#' @returns grid plot
#' @export
#'
#' @importFrom purrr map2
#' @importFrom cowplot plot_grid
#'
#' @examples
#' plot_histogram_grid(california_ev_sessions)
#' plot_histogram_grid(california_ev_sessions, vars = c("Energy", "Power"))
#'
plot_histogram_grid <- function(sessions, vars=evprof::sessions_summary_feature_names, binwidths=rep(1, length(vars)), nrow = NULL, ncol = NULL) {
  hist_list <- purrr::map2(vars, binwidths, ~ plot_histogram(sessions, .x, .y))
  cowplot::plot_grid(plotlist = hist_list, nrow = nrow, ncol = ncol)
}



# Charging rates distribution --------------------------------------------


#' Get charging rates distribution in percentages
#'
#' @param sessions tibble, sessions data set in evprof
#' [standard format](https://mcanigueral.github.io/evprof/articles/sessions-format.html).
#' @param unit character, lubridate `floor_date` unit parameter
#'
#' @returns tibble
#' @export
#'
#' @importFrom dplyr %>% select mutate group_by ungroup summarise n all_of
#' @importFrom lubridate floor_date
#' @importFrom rlang .data
#'
#' @examples
#' get_charging_rates_distribution(california_ev_sessions, unit="month")
#' get_charging_rates_distribution(california_ev_sessions, unit="month")
#'
get_charging_rates_distribution <- function(sessions, unit="year") {
  sessions_power_round <- sessions %>%
    select(all_of(c("ConnectionStartDateTime", "Power"))) %>%
    mutate(
      power = round_to_interval(.data$Power, 3.7)
    )
  sessions_power_round$power[sessions_power_round$power == 0] <- 3.7
  sessions_power_round$power[sessions_power_round$power > 11] <- 11
  sessions_power_round %>%
    group_by(
      datetime = floor_date(.data$ConnectionStartDateTime, unit = unit),
      power = .data$power
    ) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    mutate(
      ratio = .data$n/sum(.data$n)
    )
}



# Sessions features -------------------------------------------------------

#' Get daily number of sessions given a range of years, months and weekdays
#'
#' @param sessions tibble, sessions data set in evprof
#' [standard format](https://mcanigueral.github.io/evprof/articles/sessions-format.html).
#' @param years vector of integers, range of years to consider
#' @param months vector of integers, range of months to consider
#' @param wdays vector of integers, range of weekdays to consider
#'
#' @returns tibble with the number of sessions of each date in the given time period
#' @export
#'
#' @importFrom dplyr %>% filter group_by summarise n
#' @importFrom lubridate year month wday
#' @importFrom rlang .data
#'
#' @examples
#' get_daily_n_sessions(
#'   california_ev_sessions,
#'   year = 2018, months = c(5, 6), wdays = 1
#' )
#'
get_daily_n_sessions <- function(sessions, years, months, wdays) {
  sessions %>%
    filter(
      year(.data$ConnectionStartDateTime) %in% c(years),
      month(.data$ConnectionStartDateTime) %in% c(months),
      wday(.data$ConnectionStartDateTime, week_start = 1) %in% c(wdays)
    ) %>%
    group_by(date = date(.data$ConnectionStartDateTime)) %>%
    summarise(n_sessions = n())
}

#' Get the daily average number of sessions given a range of years, months and weekdays
#'
#' @param sessions tibble, sessions data set in evprof
#' [standard format](https://mcanigueral.github.io/evprof/articles/sessions-format.html).
#' @param years vector of integers, range of years to consider
#' @param months vector of integers, range of months to consider
#' @param wdays vector of integers, range of weekdays to consider
#'
#' @returns tibble with the number of sessions of each date in the given time period
#' @export
#'
#' @importFrom dplyr %>% pull
#'
#' @examples
#' get_daily_avg_n_sessions(
#'   california_ev_sessions,
#'   year = 2018, months = c(5, 6), wdays = 1
#' )
#'
get_daily_avg_n_sessions <- function(sessions, years, months, wdays) {
  get_daily_n_sessions(sessions, years, months, wdays) %>%
    pull("n_sessions") %>%
    mean %>%
    round
}


