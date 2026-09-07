#MODEL_WORKFLOW####

# Run DCM depth model and submit to VERA forecasting challenge
# Author: MP
# Date: February 2025

# Purpose: forecast Deep Chlorophyll Maximum (DCM) depth using
# thermocline depth, Schmidt stability, and Secchi depth (photic zone proxy) as covariates

library(tidyverse)
library(lubridate)
library(vera4castHelpers)
library(zoo)

if(exists("curr_reference_datetime") == FALSE){
  
  curr_reference_datetime <- Sys.Date()
  
}else{
  
  print('Running Reforecast')
  
}

# Load helper functions
helper.functions <- source("./R/helper_functions.R")

#### set function inputs
forecast_date <- Sys.Date() - lubridate::days(1)
sites <- c("fcre") #just FCR
forecast_depths <- 'focal'

forecast_horizon <- 31
n_members <- 150
calibration_start_date <- ymd("2022-11-11")
model_id <- "fCMdepth_mp"
targets_url <- "https://amnh1.osn.mghpcc.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz"

var <- "ChlorophyllMaximum_depth_sample"
project_id <- "vera4cast"


for (i in sites){
  
  site <- i
  print(site)
  
  output_folder <- paste0("./model_output/", model_id, "_", site, "_", forecast_date, ".csv")
  
  ## run function
  forecast_output <- generate_DCMdepth_forecast(forecast_date = forecast_date,
                                                forecast_horizon = forecast_horizon,
                                                n_members = n_members,
                                                output_folder = output_folder,
                                                model_id = model_id,
                                                targets_url = targets_url,
                                                var = var,
                                                site = site,
                                                forecast_depths = forecast_depths,
                                                project_id = project_id,
                                                calibration_start_date = calibration_start_date)
  
  # Write the file locally
  forecast_file_abs_path <- paste0("./model_output/fCMdepth_mp/", model_id, "_", site, "_", forecast_date, ".csv")
  
  print('Writing File...')
  
  if (!file.exists("./model_output/fCMdepth_mp/")){
    dir.create("./model_output/fCMdepth_mp/", recursive = TRUE, showWarnings = FALSE)
  }
  
  write.csv(forecast_output, forecast_file_abs_path, row.names = FALSE)
  
  
  ## validate and submit forecast (uncomment when ready to submit)
  # print('Validating File...')
  ## validator only reads the local CSV - safe, left live
  vera4castHelpers::forecast_output_validator(forecast_file_abs_path)

  ## WRITES TO BUCKET - deliberately disabled. submit() uploads this forecast to
  ## the VERA submissions bucket. Uncomment ONLY when you actually intend to
  ## submit a forecast to the challenge.
  # vera4castHelpers::submit(forecast_file_abs_path, s3_region = "submit", s3_endpoint = "ltreb-reservoirs.org", first_submission = FALSE)
  
} # end loop

#forecast_output_validator()
#submit()
#Vera4castHelpers functions
#need to register model