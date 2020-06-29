#
# get_energy_model <- function(energy_vct, n_models) {
#   mixmdl <- normalmixEM(energy_vct, k = n_models, maxit = 5000)
#   list(mu = mixmdl$mu, sigma = mixmdl$sigma, lambda = mixmdl$lambda)
# }
#
#
# estimate_energy <- function(n, mu, sigma) {
#   # if (n == 0) return(0)
#   if (n == 0) n = 1
#   energy_estimated <- rnorm(n, mu, sigma)
#   energy_estimated[energy_estimated < 0] <- 3
#   return(energy_estimated)
# }
#
#
# get_estimated_energy <- function(n, energy_models) {
#   return(pmap(
#     energy_models,
#     ~ estimate_energy(round(n*..3), ..1, ..2)
#   ))
# }
#
#
# plot_estimated_energy_density <- function(profile, energy_vct, estimated_energy) {
#   return(
#     ggplot(data = tibble(x = energy_vct), aes(x = x)) +
#       geom_density(fill = 'navy', alpha = 0.7) +
#       geom_density(
#         data = tibble(x = unlist(estimated_energy)),
#         aes(x = x), size = 1.2, color = "navy"
#       ) +
#       labs(x = "Energy charged", y = "Density", title = profile) +
#       theme_light()
#   )
# }
#
#
# plot_estimated_energy_models <- function(profile, energy_vct, estimated_energy) {
#   plot <- ggplot(data = tibble(x = energy_vct), aes(x = x)) +
#     geom_density(fill = 'navy', alpha = 0.7) +
#     labs(x = "Energy charged", y = "Density", title = profile) +
#     theme_light()
#   for (i in 1:length(estimated_energy)) {
#     plot <- plot +
#       geom_density(
#         data = tibble(x = estimated_energy[[i]]),
#         aes(x = x), size = 1.2, color = "navy"
#       )
#   }
#   return(plot)
# }
#
#
#
# # Estimated sessions ----------------------------------
#
# estimate_energy <- function(n, mu, sigma) {
#   # if (n == 0) return(0)
#   if (n == 0) n = 1
#   energy_estimated <- rnorm(n, mu, sigma)
#   energy_estimated[energy_estimated < 0] <- 3
#   return(energy_estimated)
# }
#
#
# get_estimated_energy <- function(n, energy_models) {
#   return(pmap(
#     energy_models,
#     ~ estimate_energy(round(n*..3), ..1, ..2)
#   ))
# }
#
#
# estimate_profile <- function(n, mu, sigma) {
#   # if (n == 0) return(matrix(c(0, 0), ncol = 2))
#   if (n == 0) n = 1
#   MASS::mvrnorm(n = n, mu = mu, Sigma = sigma)
# }
#
#
# get_estimated_profiles <- function(n, profile_models) {
#   return(pmap(
#     profile_models,
#     ~ estimate_profile(round(n*..3), ..1, ..2)
#   ))
# }
#
#
# estimate_sessions <- function(profile_name, n, models) {
#   n_sessions <- round(n*models$ratio[models$profile == profile_name])
#   if (n_sessions == 0) {
#     return(tibble(
#       start = 0,
#       period = 0,
#       energy = 0
#     ))
#   }
#   estimated_profiles <- do.call(rbind, get_estimated_profiles(n_sessions,
#                                                               models$profile_models[models$profile == profile_name][[1]])
#   )
#   estimated_energy <- simplify(get_estimated_energy(n_sessions,
#                                                     models$energy_models[models$profile == profile_name][[1]])
#   )
#   # Profiles' flexibility reduced to avoid charging the next morning after 6 a.m.
#   next_morning_idx <- estimated_profiles[,1] + estimated_profiles[,2] >= (24+6)
#   estimated_profiles[next_morning_idx, 2] <- estimated_profiles[next_morning_idx, 2] * 0.5
#   return(tibble(
#     start = round(estimated_profiles[,1]),
#     period = round(estimated_profiles[,2]),
#     energy = estimated_energy[1:nrow(estimated_profiles)]
#   ) %>% filter(period > 1, period <= 24, energy > 0))
# }
#
#
# get_profile_day_sessions <- function(profile_name, day, n_sessions, ev_models) {
#   if (wday(day, week_start = 1) <= 5) { # Weekdays
#     if (profile_name %in% ev_models$Weekdays$profile) {
#       return(
#         estimate_sessions(profile_name, n_sessions$Weekdays, ev_models$Weekdays) %>%
#           mutate(start_dt = force_tz(as_datetime(day) + hours(start), tzone = tzone))
#       )
#     }
#   } else { # Weekend
#     if (profile_name %in% ev_models$Weekends$profile) {
#       return(
#         estimate_sessions(profile_name, n_sessions$Weekends, ev_models$Weekends) %>%
#           mutate(start_dt = force_tz(as_datetime(day) + hours(start), tzone = tzone))
#       )
#     }
#   }
# }
#
# get_profile_sessions <- function(profile_name, dates, n_sessions, ev_models) {
#   map_dfr(dates, ~get_profile_day_sessions(profile_name, .x, n_sessions, ev_models))
# }
#
