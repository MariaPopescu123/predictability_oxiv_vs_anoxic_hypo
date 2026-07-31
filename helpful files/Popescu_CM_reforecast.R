#questions for Quinn
#we said 150 ensemble members but my CI is still wide (can I drop my met variable ensemble members too)
#also for the reforecast currently I have it running for daily (can I just run it for weekly)
#my automation not working
#are there other ways I can reduce the amount of time it takes to run
library(tidyverse)
library(arrow)
library(kableExtra)
library(scoringRules)
library(duckdbfs)

set.seed(123)

target_variable <- "ChlorophyllMaximum_depth_sample"
focal_site <- "fcre"

all_forecast_dates <- seq(from = ymd("2025-04-01"),
                          to   = ymd("2026-04-01"),
                          by   = "3 days")

my_forecasts_full <- NULL
# Load helper functions
helper.functions <- source("./R/helper_functions.R")
#from mp_DCMdepth_Forecast.R
site <- c("fcre") #just FCR
forecast_depths <- 'focal'

forecast_horizon <- 31
n_members <- 150
calibration_start_date <- ymd("2022-11-11")
model_id <- "fCMdepth_mp"
targets_url <- "https://amnh1.osn.mghpcc.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz"

var <- "ChlorophyllMaximum_depth_sample"
project_id <- "vera4cast"

#### Get targets ####
message('Getting targets')
#read once and reuse for all target subsets (avoids 4 separate downloads)
all_targets <- readr::read_csv(targets_url, show_col_types = F) |>
  filter(site_id %in% site,
         datetime <= as_date(Sys.Date()))

targets <- all_targets |>
  filter(variable %in% var)

#this function tells me data availability
source("R/data_availability_function.R")

#### Bathymetry ####
#needed for schmidt stability calculation
bath <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/1254/1/f7fa2a06e1229ee75ea39eb586577184")

bathFCR <- bath|>
  filter(Reservoir == "FCR")

# interpolate bathymetry to 0.1m resolution
bth_depths_interp <- seq(min(bathFCR$Depth_m), max(bathFCR$Depth_m), by = 0.1)
bth_areas_interp <- approx(x = bathFCR$Depth_m, y = bathFCR$SA_m2,
                           xout = bth_depths_interp, rule = 2)$y

bth_depths <- bth_depths_interp
bth_areas <- bth_areas_interp

#### FLARE forecast covariates ####
#pulling FLARE outputs to use as future covariates

fcre_reforecast <- arrow::s3_bucket(file.path("bio230121-bucket01/flare/forecasts/parquet/site_id=fcre/model_id=glm_aed_flare_v3/"),
                                    endpoint_override = 'amnh1.osn.mghpcc.org',
                                    anonymous = TRUE)

#ADDING THIS HERE FOR ADDITIONAL DATA NEED TO CHECK SHOULD GO BACK TO MAY 2024
#NOT INCLUDED YET
fcre_reforecast2 <- arrow::s3_bucket(file.path("bio230121-bucket01/fcre-reforecast/forecasts/parquet/site_id=fcre/model_id=glm_aed_flare_v3_reforecast"),
                                     endpoint_override = 'amnh1.osn.mghpcc.org',
                                     anonymous = TRUE)


flare_ds1 <- arrow::open_dataset(list(
  arrow::open_dataset(fcre_reforecast),
  arrow::open_dataset(fcre_reforecast2))) #using both for an extra couple of data points but still just up to May 2024
#CHANGED HERE TO INCLUDE BOTH VERSIONS OF FLARE
#start anytime after january 2021
flare_ds0 <- flare_ds1|>
  mutate(reference_datetime = as_date(reference_datetime))|>
  filter(reference_datetime >= ymd("2021-01-05"))


for (fdate in all_forecast_dates) {
  
  forecast_date <- as_date(fdate)
  #remove when done testing
  flare_ds <- flare_ds0|>
    filter(reference_datetime <= forecast_date)
  
  #pull FLARE forecast issued on/just before forecast_date so the future
  #covariates actually align with the forecast horizon
  forecast_horizon_start <- as.POSIXct(forecast_date + 1, tz = "UTC")
  forecast_ref_cutoff <- as.POSIXct(forecast_date + 1, tz = "UTC")
  
  #future water temp####
  flare_ref_temp <- flare_ds |>
    filter(variable == "Temp_C_mean",
           reference_datetime <= forecast_ref_cutoff) |>
    summarise(max_ref = max(reference_datetime)) |>
    collect() |>
    pull(max_ref)
  
  df_flare_new <- flare_ds |>
    filter(
      variable == "Temp_C_mean",
      parameter <= n_members, #flare produces enough parameters weee
      reference_datetime == flare_ref_temp,
      datetime >= forecast_horizon_start
    ) |>
    collect()
  
  #not touched
  df_flare_new_forbind <- df_flare_new |>
    rename(datetime_date = datetime) |>
    filter(parameter <= n_members) |>
    select(-reference_date) |>
    mutate(variable = "temperature",
           reference_datetime = as.character(reference_datetime),
           site_id = site,
           model_id = 'test_runs3') |>
    mutate(parameter = as.numeric(parameter)) |>
    select(reference_datetime, datetime_date, site_id, depth, family, parameter, variable, prediction, model_id)
  
  ####future secchi####
  flare_ref_secchi <- flare_ds |>
    filter(variable == "secchi",
           reference_datetime <= forecast_ref_cutoff) |>
    summarise(max_ref = max(reference_datetime)) |>
    collect()|>
    pull(max_ref)
  
  #could also just filter on horizon
  secchi_flare_new <- flare_ds |>
    filter(
      variable == "secchi",
      parameter <= n_members,
      reference_datetime == flare_ref_secchi,
      datetime >= forecast_horizon_start
    ) |>
    collect()
  
  secchi_flare_new_forbind <- secchi_flare_new |>
    rename(datetime_date = datetime) |>
    filter(parameter <= n_members) |>
    select(-reference_date) |>
    mutate(variable = "secchi",
           reference_datetime = as.character(reference_datetime),
           site_id = site,
           model_id = 'test_runs3') |>
    mutate(parameter = as.numeric(parameter)) |>
    select(reference_datetime, datetime_date, site_id, depth, family, parameter, variable, prediction, model_id)
  
  #secchi is calculated from light extinction
  #extc - light extinction outputted from FLARE
  
  # future Schmidt stability per ensemble member from FLARE water temp profiles
  # uses bathymetry and temp-at-depth to compute daily Schmidt stability
  ####future schmidt####
  future_schmidt_ens <- df_flare_new_forbind |>
    group_by(datetime_date, parameter) |>
    arrange(depth) |>
    summarise(
      SchmidtStability_Jm2_mean = rLakeAnalyzer::schmidt.stability(
        wtr = prediction, depths = depth,
        bthA = bth_areas, bthD = bth_depths
      ),
      .groups = "drop"
    ) |>
    mutate(datetime = as.Date(datetime_date)) |>
    select(datetime, parameter, SchmidtStability_Jm2_mean)
  
  # future secchi per ensemble member from FLARE
  future_secchi_ens <- secchi_flare_new_forbind |>
    group_by(datetime_date, parameter) |>
    summarise(Secchi_m_sample = mean(prediction, na.rm = TRUE), .groups = "drop") |>
    mutate(datetime = as.Date(datetime_date)) |>
    select(datetime, parameter, Secchi_m_sample)
  
  ###combined future covariates####
  future_covariates <- future_schmidt_ens |>
    full_join(future_secchi_ens, by = c("datetime", "parameter"))
  
  #### Historic covariates####
  #Schmidt stability
  #secchi
  #average air temperature (from NOAA)
  #average windspeed (from NOAA)
  
  #historic water temp####
  df_flare_new <- flare_ds |>
    filter(
      variable == "Temp_C_mean",
      parameter <= n_members,
      reference_datetime >= ymd("2021-01-05"),
      reference_datetime <= forecast_date, #no data leakage
      reference_datetime == datetime
    ) |>
    collect()
  
  #not touched
  df_flare_new_forbind <- df_flare_new |>
    rename(datetime_date = datetime) |>
    filter(parameter <= n_members) |>
    select(-reference_date) |>
    mutate(variable = "temperature",
           reference_datetime = as.character(reference_datetime),
           site_id = site,
           model_id = 'test_runs3') |>
    mutate(parameter = as.numeric(parameter)) |>
    select(reference_datetime, datetime_date, site_id, depth, family, parameter, variable, prediction, model_id)
  
  ####historic secchi####
  
  secchi_flare_new <- flare_ds |>
    filter(
      variable == "secchi",
      parameter <= n_members,
      reference_datetime >= ymd("2021-01-05"),
      reference_datetime <= forecast_date,
      reference_datetime == datetime
    ) |>
    collect()
  
  secchi_flare_new_forbind <- secchi_flare_new |>
    rename(datetime_date = datetime) |>
    filter(parameter <= n_members) |>
    select(-reference_date) |>
    mutate(variable = "secchi",
           reference_datetime = as.character(reference_datetime),
           site_id = site,
           model_id = 'test_runs3') |>
    mutate(parameter = as.numeric(parameter)) |>
    select(reference_datetime, datetime_date, site_id, depth, family, parameter, variable, prediction, model_id)
  
  ####historic Schmidt stability####
  #per ensemble member from FLARE water temp profiles
  # uses bathymetry and temp-at-depth to compute daily Schmidt stability
  historic_schmidt_ens <- df_flare_new_forbind |>
    group_by(datetime_date, parameter) |>
    arrange(depth) |>
    summarise(
      SchmidtStability_Jm2_mean = rLakeAnalyzer::schmidt.stability(
        wtr = prediction, depths = depth,
        bthA = bth_areas, bthD = bth_depths
      ),
      .groups = "drop"
    ) |>
    mutate(datetime = as.Date(datetime_date)) |>
    select(datetime, parameter, SchmidtStability_Jm2_mean)|>
    group_by(datetime)|>
    summarise(SchmidtStability_Jm2_mean = mean(SchmidtStability_Jm2_mean, na.rm = TRUE), .groups = "drop")
  
  # historic secchi per ensemble member from FLARE
  historic_secchi_ens <- secchi_flare_new_forbind |>
    group_by(datetime_date, parameter) |>
    summarise(Secchi_m_sample = mean(prediction, na.rm = TRUE), .groups = "drop") |>
    mutate(datetime = as.Date(datetime_date)) |>
    select(datetime, parameter, Secchi_m_sample)|>
    group_by(datetime)|>
    summarise(Secchi_m_sample = mean(Secchi_m_sample, na.rm = TRUE), .groups = "drop")
  
  
  ####combined historic secchi and schmidt####
  historic_covariates <- historic_schmidt_ens |>
    full_join(historic_secchi_ens, by = c("datetime"))
  
  covariate_vars <- c("SchmidtStability_Jm2_mean", "Secchi_m_sample")
  
  
  #### Weather drivers ####
  #forecasted weather data
  # Get the forecasted weather data (shortwave, air temperature, wind components)
  message('Getting weather')
  
  met_vars <- c("surface_downwelling_shortwave_flux_in_air", "air_temperature",
                "eastward_wind", "northward_wind")
  
  noaa_date <- forecast_date
  print(paste0('NOAA data from: ', noaa_date))
  
  met_s3_future <- arrow::s3_bucket(file.path("bio230121-bucket01/flare/drivers/met/gefs-v12/stage2",
                                              paste0("reference_datetime=", noaa_date),
                                              paste0("site_id=", site)),
                                    endpoint_override = 'amnh1.osn.mghpcc.org',
                                    anonymous = TRUE)
  
  forecast_weather_raw <- arrow::open_dataset(met_s3_future) |>
    dplyr::filter(variable %in% met_vars) |>
    mutate(datetime_date = as.Date(datetime),
           reference_datetime = forecast_date) |>
    group_by(reference_datetime, datetime_date, variable, parameter) |>
    summarise(prediction = mean(prediction, na.rm = T), .groups = "drop") |>
    mutate(parameter = parameter + 1) |>
    dplyr::collect()
  
  # compute wind speed from eastward and northward components per ensemble member per day
  wind_components <- forecast_weather_raw |>
    filter(variable %in% c("eastward_wind", "northward_wind")) |>
    pivot_wider(names_from = variable, values_from = prediction) |>
    mutate(variable = "wind_speed",
           prediction = sqrt(eastward_wind^2 + northward_wind^2)) |>
    select(reference_datetime, datetime_date, variable, parameter, prediction)
  
  forecast_weather <- forecast_weather_raw |>
    filter(!variable %in% c("eastward_wind", "northward_wind")) |>
    bind_rows(wind_components)
  
  # historic weather
  historic_noaa_s3 <- arrow::s3_bucket(paste0("bio230121-bucket01/flare/drivers/met/gefs-v12/stage3/site_id=", site),
                                       endpoint_override = "amnh1.osn.mghpcc.org",
                                       anonymous = TRUE)
  
  historic_weather_raw <- arrow::open_dataset(historic_noaa_s3) |>
    filter(datetime < forecast_date,
           datetime >= as.Date(calibration_start_date),
           variable %in% met_vars) |>
    group_by(datetime, variable) |>
    summarise(prediction = mean(prediction, na.rm = T), .groups = "drop") |>
    collect()
  
  # compute historic wind speed from components
  historic_wind <- historic_weather_raw |>
    filter(variable %in% c("eastward_wind", "northward_wind")) |>
    pivot_wider(names_from = variable, values_from = prediction) |>
    mutate(variable = "wind_speed",
           prediction = sqrt(eastward_wind^2 + northward_wind^2)) |>
    select(datetime, variable, prediction)
  
  historic_weather <- historic_weather_raw |>
    filter(!variable %in% c("eastward_wind", "northward_wind")) |>
    bind_rows(historic_wind) |>
    pivot_wider(names_from = variable, values_from = prediction) |>
    mutate(datetime = as.Date(datetime))|>
    group_by(datetime)|>
    summarise(air_temperature = mean(air_temperature, na.rm = T),
              wind_speed = mean(wind_speed, na.rm = T), .groups = "drop")
  
  
  #### Fit random forest model for DCM depth ####
  message('Preparing training data')
  
  # predictors
  all_predictor_vars <- c("SchmidtStability_Jm2_mean", "Secchi_m_sample",
                          "air_temperature", "wind_speed",
                          "doy_sin", "doy_cos")
  
  fit_df <- targets |>
    mutate(datetime = as.Date(datetime)) |>
    filter(datetime < forecast_date,
           datetime >= calibration_start_date) |>
    group_by(datetime, variable) |>
    summarise(observation = mean(observation, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = variable, values_from = observation) |>
    full_join(historic_covariates, by = "datetime") |>
    full_join(historic_weather, by = "datetime") |>
    mutate(doy_sin = sin(2 * pi * lubridate::yday(datetime) / 365),
           doy_cos = cos(2 * pi * lubridate::yday(datetime) / 365))|>
    arrange(datetime)|>
    filter(!is.na(ChlorophyllMaximum_depth_sample))
  
  #only keep predictors that actually exist in the data for this site
  predictor_vars <- all_predictor_vars[all_predictor_vars %in% names(fit_df)]
  message(paste0('Using predictors: ', paste(predictor_vars, collapse = ", ")))
  
  #remove NAs for randomforest
  fit_df_noNA <- fit_df |>
    select(all_of(c(var, predictor_vars))) |>
    na.omit()
  
  #I technically only have 37 right now since 2024-10-01 is earliest FLARE output for temp and secchi
  #I have now combined the other FLARE output so I have some starting 2024-05
  message('Fitting random forest model')
  
  # 80/20 train/test split 
  
  trainIndex <- sample(1:nrow(fit_df_noNA), 0.8 * nrow(fit_df_noNA))
  trainData  <- fit_df_noNA[trainIndex, ]
  testData   <- fit_df_noNA[-trainIndex, ]
  
  rf_formula <- reformulate(predictor_vars, response = var)
  
  #taking this straight from my DCM depth project 
  grid_trees <- c(100, 200, 300, 500)
  grid_nodes <- c(2, 4, 6, 8)
  grid_mtry <- c(1,2,3,4,5,6)
  
  results <- list()
  idx <- 1
  
  for (nt in grid_trees) {
    for (ns in grid_nodes) {
      for (mt in grid_mtry) {
        if (mt > length(predictor_vars)) next
        
        fit <- randomForest(
          formula    = rf_formula,
          data       = trainData,
          ntree      = nt,
          mtry       = mt,
          nodesize   = ns,
          importance = TRUE
        )
        rsq <- fit$rsq[length(fit$rsq)]
        mse <- fit$mse[length(fit$mse)]
        
        results[[idx]] <- data.frame(
          Trees = nt,
          NodeSize = ns,
          mtry = mt,
          OOB_R2 = rsq,
          OOB_MSE = mse,
          OOB_RMSE = sqrt(mse)
        )
        idx <- idx + 1
      }
    }
  }
  
  RF_tuning_scores <- dplyr::bind_rows(results) %>%
    arrange(desc(OOB_R2), OOB_MSE)
  print(RF_tuning_scores)
  
  best <- RF_tuning_scores[1, ]
  
  dcm_model <- randomForest(
    formula    = rf_formula,
    data       = trainData,
    ntree      = best$Trees,
    mtry       = best$mtry,
    nodesize   = best$NodeSize,
    importance = TRUE
  )
  
  #look at Adrienne's! https://github.com/LTREB-reservoirs/vera4cast_models/blob/main/R/spcond_rf_abp/RF_model_helper.R
  # predict on test set and compute process uncertainty from residuals (from Adrienne)
  # from Adrienne this is where prediction is actually happening
  testData$forecast_dcm <- predict(dcm_model, testData)
  residuals <- testData$forecast_dcm - testData[[var]]
  sigma <- sd(residuals, na.rm = TRUE) #this is my process uncertainty
  
  message(paste0('Process uncertainty (sigma): ', round(sigma, 3)))  
  
  
  #### Generate forecast ####
  message('Make forecast dataframe')
  
  forecasted_dates <- seq(from = ymd(forecast_date) + 1, to = ymd(forecast_date) + forecast_horizon, by = "day")
  
  # set up table to hold forecast output
  forecast_full_unc <- tibble(date = rep(forecasted_dates, times = n_members),
                              ensemble_member = rep(1:n_members, each = length(forecasted_dates)),
                              reference_datetime = forecast_date,
                              Horizon = date - reference_datetime,
                              forecast_variable = var,
                              value = as.double(NA),
                              uc_type = "total")
  
  #-------------------------------------
  
  message('Generating forecast')
  print(paste0('Running forecast starting on: ', forecast_date))
  
  # FLARE covariates collapsed to daily means (not driver uncertainty here)
  forecast_covariates <- future_covariates |>
    group_by(datetime) |>
    summarise(
      SchmidtStability_Jm2_mean = mean(SchmidtStability_Jm2_mean, na.rm = TRUE),
      Secchi_m_sample           = mean(Secchi_m_sample, na.rm = TRUE),
      .groups = "drop"
    )
  
  # weather drivers per ensemble member (driver uncertainty here)
  #this cycles available NOAA ensembles to fill n_members from prev assignment
  weather_ensemble_names <- unique(forecast_weather$parameter)
  repeat_ens <- rep(weather_ensemble_names, length.out = n_members)
  ens_map <- tibble(ensemble_member = 1:n_members, parameter = repeat_ens)
  
  weather_input <- forecast_weather |>
    filter(variable %in% c("air_temperature", "wind_speed"),
           ymd(reference_datetime) == forecast_date) |>
    select(datetime_date, parameter, variable, prediction) |>
    pivot_wider(names_from = variable, values_from = prediction) |>
    rename(datetime = datetime_date) |>
    mutate(datetime = as.Date(datetime)) |>
    inner_join(ens_map, by = "parameter") |>
    select(ensemble_member, datetime, air_temperature, wind_speed)
  
  # join per-ensemble weather with averaged FLARE covariates and add doy
  forecast_input <- weather_input |>
    left_join(forecast_covariates, by = "datetime") |>
    mutate(doy_sin = sin(2 * pi * lubridate::yday(datetime) / 365),
           doy_cos = cos(2 * pi * lubridate::yday(datetime) / 365)) |>
    drop_na()
  
  # predict per ensemble member, then add process uncertainty noise per row
  forecast_input$mu <- round(predict(dcm_model, forecast_input), digits = 2)
  forecast_input$prediction <- forecast_input$mu + rnorm(nrow(forecast_input), mean = 0, sd = sigma)
  
  forecast_par <- forecast_input |>
    select(datetime, ensemble_member, prediction) |>
    rename(parameter = ensemble_member)
  
  #### Format output to vera standard ####
  forecast_df <- forecast_par |>
    mutate(
      family             = 'ensemble',
      duration           = "P1D",
      depth_m            = NA_real_, #this is ok
      variable           = var,
      project_id         = project_id,
      model_id           = model_id,
      site_id            = site,
      reference_datetime = forecast_date,
      parameter          = as.numeric(parameter)
    ) |>
    select(datetime, reference_datetime, duration, site_id, depth_m,
           family, parameter, variable, prediction, model_id, project_id)
  
  
  my_forecasts_full <- bind_rows(my_forecasts_full, forecast_df)
}
library(readr)
write_csv(my_forecasts_full, "R/my_forecasts_full.csv")
