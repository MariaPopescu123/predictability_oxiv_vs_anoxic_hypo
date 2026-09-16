
# Function carry out a random walk forecast
generate_baseline_persistenceRW <- function(targets,
                                            site,
                                            var,
                                            forecast_date = Sys.Date(),
                                            model_id = 'persistenceRW',
                                            h,
                                            depth = 'target',
                                            bootstrap = FALSE,
                                            boot_number = 200, ...) {
  
  message('Generating persistenceRW forecast for ',  var, ' at ', site)
  
  if (depth == 'target') {
    # only generates forecasts for target depths
    target_depths <- c(1.5, 1.6, NA)
  } else {
    target_depths <- depth
  }
  
  # if (site == 'bvre' & !(var %in% c('Chla_ugL_mean'))){
  #   target_depths <- c(0.1, 3, NA)
  # }
  
  targets_ts <- targets |>
    mutate(datetime = lubridate::as_date(datetime)) |>
    filter(variable %in% var,
           site_id %in% site,
           depth_m %in% target_depths,
           datetime < forecast_date) |>
    group_by(variable, site_id, depth_m, duration, project_id, datetime) |>
    summarise(observation = mean(observation), .groups = 'drop') |>  # get rid of the repeat observations by finding the mean
    as_tsibble(key = c('variable', 'site_id', 'depth_m', 'duration', 'project_id'), index = 'datetime') |>
    # add NA values up to today (index)
    fill_gaps(.end = forecast_date)
  
  
  # Work out when the forecast should start
  # NOTE: must filter on depth too. Without it the start date comes from the most recent
  # observation at ANY depth, so a depth that was last sampled earlier gets padded with
  # NAs by fill_gaps() and the random walk starts from NA -> an all-NA forecast.
  forecast_starts <- targets %>%
    dplyr::filter(!is.na(observation) & site_id == site & variable == var &
                    depth_m %in% target_depths & datetime < forecast_date) %>%
    # Start the day after the most recent non-NA value
    dplyr::summarise(start_date = as_date(max(datetime)) + lubridate::days(1)) %>% # Date
    dplyr::mutate(h = (forecast_date - start_date) + h + 1) %>% # Horizon value
    dplyr::ungroup()
  
  # filter the targets data set to the site_var pair
  targets_use <- targets_ts |>
    dplyr::filter(datetime < forecast_starts$start_date)
  
  if (nrow(targets_use) == 0) {
    message(paste0('no targets available, no forecast run for ', site, ' ', var, '. Check site_id and variable name'))
    return(NULL)
    
  } else {
    
    RW_model <- targets_use %>%
      fabletools::model(RW = fable::RW(observation))
    
    
    if (bootstrap == T) {
      forecast <- RW_model %>%
        fabletools::generate(h = as.numeric(forecast_starts$h),
                             bootstrap = T,
                             times = boot_number) |>
        rename(parameter = .rep,
               prediction = .sim) |>
        mutate(model_id = model_id,
               family = 'ensemble',
               reference_datetime = forecast_date)  |>
        select(any_of(c("model_id", "datetime", "reference_datetime","site_id", "variable", "family",
                        "parameter", "prediction", "project_id", "duration", "depth_m" )))|>
        select(-any_of('.model'))|>
        filter(datetime > reference_datetime)|>
        ungroup() |>
        as_tibble()
      
      return(forecast)
      
    }  else {
      # don't use bootstrapping
      forecast <- RW_model %>% fabletools::forecast(h = as.numeric(forecast_starts$h))
      
      # extract parameters
      parameters <- distributional::parameters(forecast$observation)
      
      # make right format
      forecast <- bind_cols(forecast, parameters) |>
        pivot_longer(mu:sigma,
                     names_to = 'parameter',
                     values_to = 'prediction') |>
        mutate(model_id = model_id,
               family = 'normal',
               reference_datetime=forecast_date) |>
        select(all_of(c("model_id", "datetime", "reference_datetime","site_id", "variable", "family",
                        "parameter", "prediction", "project_id", "duration", "depth_m" ))) |>
        select(-any_of('.model')) |>
        filter(datetime > reference_datetime) |>
        ungroup() |>
        as_tibble()
      return(forecast)
    }
    
  }
}