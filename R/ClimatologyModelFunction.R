generate_baseline_climatology <- function(targets, # a dataframe already read in
                                          h = 35,
                                          site, # vector of site_ids
                                          model_id = 'climatology',
                                          var, # single variable
                                          depth = 'target',
                                          forecast_date = Sys.Date()) {
  message('Generating climatology for ',  var, ' at ', site)
  
  if (depth == 'target') {
    # only generates forecasts for target depths
    target_depths <- c(1.5, 1.6, NA)
  } else {
    target_depths <- depth
  }
  
  
  variable_df <- data.frame(doy = seq(1,366, 1),
                            variable = var,
                            site_id = site)
  
  # calculate the mean and standard deviation for each doy
  target_clim <- targets %>%
    filter(variable %in% var,
           depth_m %in% target_depths,
           site_id %in% site,
           datetime < forecast_date) %>%
    mutate(doy = yday(datetime),
           observation = ifelse(is.nan(observation), NA, observation)) %>%
    drop_na(observation) |>
    group_by(doy, site_id, variable, depth_m) %>%
    summarise(clim_mean = mean(observation, na.rm = TRUE),
              clim_sd = sd(observation, na.rm = TRUE),
              .groups = "drop") %>%
    drop_na(clim_mean) |>
    full_join(variable_df, by = c('doy', 'site_id', 'variable')) |>
    arrange(doy) |>
    mutate(clim_mean = ifelse(is.nan(clim_mean), NA, clim_mean),
           clim_mean = ifelse(variable == 'Secchi_m_sample',
                              imputeTS::na_interpolation(x = clim_mean),
                              # all values "interpolated" irrespective of gap length?
                              clim_mean))
  
  if (nrow(target_clim) == 0) {
    message('No targets available. Check that the dates, depths, and sites exist in the target data frame')
    return(NULL)
  } else {
    # what dates do we want a forecast of?
    curr_month <- month(forecast_date)
    if(curr_month < 10){
      curr_month <- paste0("0", curr_month)
    }
    
    curr_year <- year(forecast_date)
    start_date <- forecast_date + days(1)
    
    forecast_dates <- seq(start_date, as_date(start_date + days(h)), "1 day")
    forecast_doy <- yday(forecast_dates)
    
    # put in a table
    forecast_dates_df <- tibble(datetime = forecast_dates,
                                doy = forecast_doy)
    
    forecast <- target_clim %>%
      mutate(doy = as.integer(doy)) %>%
      filter(doy %in% forecast_doy) %>%
      full_join(forecast_dates_df, by = 'doy') %>%
      arrange(site_id, datetime)
    
    subseted_site_names <- unique(forecast$site_id)
    site_vector <- NULL
    for(i in 1:length(subseted_site_names)){
      site_vector <- c(site_vector, rep(subseted_site_names[i], length(forecast_dates)))
    }
    
    # make sure all are represented
    forecast_tibble <- tibble(datetime = rep(forecast_dates, length(subseted_site_names)),
                              site_id = site_vector,
                              variable = var)
    
    forecast <- right_join(forecast, forecast_tibble, by = join_by("site_id", "variable", "datetime")) |>
      filter(!is.na(clim_mean))
    
    # Check for missing and interpolate, remove if there are less than two dates forecasted
    site_count <- forecast %>%
      select(datetime, site_id, variable, clim_mean, clim_sd) %>%
      filter(!is.na(clim_mean)) |>
      group_by(site_id, variable) %>%
      summarize(count = n(), .groups = "drop") |>
      filter(count > 2) |>
      distinct() |>
      pull(site_id)
    
    if (length(site_count) != 0) {
      combined <- forecast %>%
        filter(site_id %in% site_count) |>
        select(datetime, site_id, depth_m, variable, clim_mean, clim_sd, depth_m) %>%
        rename(mean = clim_mean,
               sd = clim_sd) %>%
        group_by(site_id, variable) %>%
        mutate(mu = imputeTS::na_interpolation(x = mean),
               sigma = median(sd, na.rm = TRUE)) |>
        
        # get in standard format
        pivot_longer(c("mu", "sigma"),names_to = "parameter", values_to = "prediction") |>
        mutate(family = "normal") |>
        mutate(reference_datetime = forecast_date,
               model_id = model_id) |>
        select(model_id, datetime, reference_datetime, site_id, variable, family, parameter, prediction, depth_m) |>
        mutate(project_id = "vera4cast",
               duration = "P1D") |>
        ungroup() |>
        as_tibble()
      
      message('climatology generated')
      return(combined)
    } else {
      message('cannot generate climatology for this period')
      return(NULL)
    }
  }
  
}