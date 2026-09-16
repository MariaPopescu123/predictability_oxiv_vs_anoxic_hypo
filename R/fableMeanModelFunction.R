
generate_baseline_mean <- function(targets, # a dataframe already read in
                                   h = 35,
                                   site,
                                   model_id = 'historic_mean',
                                   var,
                                   depth = 'target',
                                   forecast_date = Sys.Date(),
                                   bootstrap = F,
                                   boot_number = 200) {
  message('Generating historic mean forecast for ',  var, ' at ', site)
  
  if (depth == 'target') {
    # only generates forecasts for target depths
    target_depths <- c(1.5, 1.6, NA)
  } else {
    target_depths <- depth
  }
  
  
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
  
  
  # filter the targets data set to the site_var pair
  targets_use <- targets_ts |>
    dplyr::filter(datetime < forecast_date)
  
  
  if (nrow(targets_use) == 0) {
    message(paste0('no targets available, no forecast run for ', site, ' ', var, '. Check site_id and variable name'))
    return(NULL)
    
  } else {
    # fit model
    mean_model <- targets_use |>
      fabletools::model(mean = fable::MEAN(observation))
    
    # Generate uncertainty using boostrapping??
    if (bootstrap == T) {
      forecast <- mean_model %>%
        fabletools::generate(h = h,
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
        filter(datetime > reference_datetime)
      
      return(as_tibble(forecast))
      
    }  else {
      # don't use bootstrapping
      forecast <- mean_model %>% fabletools::forecast(h = h)
      
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