## code to prepare `sessions` dataset goes here
library(dplyr)
library(lubridate)

sessions <- readRDS("data-raw/sessions.rds") %>%
  filter(year(StartTime) == 2019, month(StartTime) >= 9)

usethis::use_data(sessions, overwrite = TRUE, internal = TRUE)
