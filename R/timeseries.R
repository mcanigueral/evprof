

# Timeseries support ------------------------------------------------------

#' Convert a data.frame or tibble to timeseries data.frame
#'
#' @param df data.frame or tibble
#'
#' @return timeseries data.frame
#' @export
#'
#' @importFrom xts xts
#'
df_to_ts <- function(df) {
  xts::xts(df[-1], order.by = df[[1]])
}



# Demand ------------------------------------------------------------------

#' Obtain demand from a starting dttm value and certain duration interval
#'
#' @param sessions sessions data set in standard format
#' @param dttm_start datetime value starting the demand calculation
#' @param interval numeric, duration (in minutes) of the demand interval
#' @param by character, being 'Profile' or 'Session'. When `by='Profile'` each column corresponds to an EV user profile.
#'
#' @return tibble
#'
#' @importFrom dplyr %>% filter group_by summarise mutate sym
#' @importFrom tidyr spread
#' @importFrom lubridate minutes
#' @importFrom rlang .data
#'
get_interval_demand <- function(sessions, dttm_start, interval, by = c("Profile", "Session")) {
  sessions %>%
    filter(
      .data$ConnectionStartDateTime <= dttm_start & .data$ChargingEndDateTime >= (dttm_start + minutes(interval))
    ) %>%
    group_by(!!sym(by)) %>%
    summarise(Power = sum(.data$Power)) %>%
    mutate(datetime = dttm_start) %>%
    spread(!!sym(by), .data$Power)
}


#' Obtain timeseries demand from sessions dataset
#'
#' @param sessions sessions data set in standard format
#' @param dttm_seq vector with a sequence of datetime values
#' @param by character, being 'Profile' or 'Session'. When `by='Profile'` each column corresponds to an EV user profile.
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr %>% left_join select everything rename
#' @importFrom purrr map2_dfr
#'
get_demand <- function(sessions, dttm_seq, by = "Profile") {
  time_interval <- as.integer(as.numeric(dttm_seq[2] - dttm_seq[1], unit = 'hours')*60)
  demand <- tibble(datetime = dttm_seq) %>%
    left_join(
      map_dfr(dttm_seq, ~get_interval_demand(sessions, .x, time_interval, by)),
      by = "datetime"
    )
  demand <- replace(is.na(demand), 0)
  return( demand )
}



# # DR potential ---------------------------------------------------------------
#
# get_interval_flexible_power <- function(interval, sessions_flex, interval_mins) {
#   sessions_flex %>%
#     group_by(Profile) %>%
#     filter(
#       StartTime <= interval,
#       (StartTime + convert_time_num_to_period(ChargingTime)) >= (interval + minutes(interval_mins)),
#       Flexibility > interval_mins/60
#     ) %>%
#     summarise(Power = sum(ChargingPower)) %>%
#     mutate(datetime = interval) %>%
#     spread(Profile, Power)
# }
#
# get_flexible_power <- function(sessions_flex, seq_dt, interval_mins) {
#   tibble(datetime = seq_dt) %>%
#     left_join(
#       map_dfr(seq_dt, ~get_interval_flexible_power(.x, sessions_flex, interval_mins)),
#       by = "datetime"
#     ) %>%
#     replace(is.na(.), 0)
# }

