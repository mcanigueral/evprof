## code to prepare `sessions` dataset goes here
library(dplyr)
library(lubridate)

sessions <- readRDS("data-raw/sessions.rds") %>%
  filter(year(StartTime) == 2019, month(StartTime) >= 9)

ev_model <- readRDS('data-raw/ev_model.RDS')

usethis::use_data(sessions, ev_model, overwrite = TRUE, internal = TRUE)
