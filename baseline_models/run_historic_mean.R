print(paste0("Running persistence at ", Sys.time()))

library(tidyverse)
library(lubridate)
library(aws.s3)
library(imputeTS)
library(tsibble)
library(fable)

#config <- yaml::read_yaml("challenge_configuration.yaml") Maria hardcoded in from yaml (see climatology)
team_name <- 'historic_mean'

source('R/fableMeanModelFunction.R')
source('R/convert2binary.R')

# Read in targets
targets_insitu <- readr::read_csv(paste0("https://", "amnh1.osn.mghpcc.org", "/", "bio230121-bucket01/vera4cast/targets", "/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz"), guess_max = 10000)
targets_met <- readr::read_csv(paste0("https://", "amnh1.osn.mghpcc.org", "/", "bio230121-bucket01/vera4cast/targets", "/project_id=vera4cast/duration=P1D/daily-met-targets.csv.gz"), guess_max = 10000, show_col_types = FALSE)
targets_tubr <- readr::read_csv(paste0("https://", "amnh1.osn.mghpcc.org", "/", "bio230121-bucket01/vera4cast/targets", "/project_id=vera4cast/duration=P1D/daily-inflow-targets.csv.gz"), guess_max = 10000, show_col_types = FALSE)

# Keep a pristine copy of the insitu targets. Later in the script targets_insitu gets
# mutated (BVR chem depths are all recoded to 1.5 m), which destroys the real depth
# structure -- the depth-resolved forecasts below must be built from this untouched copy.
targets_insitu_raw <- targets_insitu

# Get site information
sites <- readr::read_csv('https://raw.githubusercontent.com/LTREB-reservoirs/vera4cast/main/vera4cast_field_site_metadata.csv',
                         show_col_types = FALSE)
site_names <- sites$site_id

# Runs the RW forecast for inflow variables
historic_mean_inflow <- purrr::map_dfr(.x = c('Flow_cms_mean', 'Temp_C_mean'),
                                       .f = ~ generate_baseline_mean(targets = targets_tubr,
                                                                     h = 35,
                                                                     model_id = team_name,
                                                                     forecast_date = Sys.Date(),
                                                                     site = 'tubr',
                                                                     depth = 'target',
                                                                     var = .x,
                                                                     ...))

# met variables
historic_mean_met <- generate_baseline_mean(targets = targets_met,
                                            h = 35,
                                            model_id = team_name,
                                            forecast_date = Sys.Date(),
                                            site = 'fcre',
                                            depth = 'target',
                                            var = "AirTemp_C_mean")


# Insitu variables
# get all combinations
site_var_combinations <- expand.grid(var = c('DO_mgL_mean',
                                             'DOsat_percent_mean',
                                             'Chla_ugL_mean',
                                             'Secchi_m_sample',
                                             'Temp_C_mean',
                                             'fDOM_QSU_mean',
                                             'SpCond_uScm_mean',
                                             'Turbidity_FNU_mean'),#,
                                     #'CH4_umolL_sample',
                                     #'CO2_umolL_sample'),
                                     site = c('fcre',
                                              'bvre'))

historic_mean_insitu <- purrr::pmap_dfr(site_var_combinations,
                                        .f = ~ generate_baseline_mean(targets = targets_insitu,
                                                                      h = 35,
                                                                      model_id = team_name,
                                                                      forecast_date = Sys.Date(),
                                                                      depth = 'target',
                                                                      ...))

### INSITU VARIABLES AT DEEPER DEPTH ##
print('Insitu model deeper...')
site_var_combinations_deeper_depth_fcr <- expand.grid(var = c('DO_mgL_mean',
                                                              'Temp_C_mean',
                                                              'CH4_umolL_sample'),
                                                      site = 'fcre',
                                                      depth = 9)

historic_mean_insitu_deeper_fcr <- purrr::pmap_dfr(site_var_combinations_deeper_depth_fcr,
                                                   .f = ~ generate_baseline_mean(targets = targets_insitu,
                                                                                 h = 35,
                                                                                 model_id = team_name,
                                                                                 forecast_date = Sys.Date(),
                                                                                 #depth = 'target',
                                                                                 ...))

site_var_combinations_deeper_depth_bvr <- expand.grid(var = c('DO_mgL_mean',
                                                              'Temp_C_mean',
                                                              'CH4_umolL_sample'),
                                                      site = 'bvre',
                                                      depth = 8)

historic_mean_insitu_deeper_bvr <- purrr::pmap_dfr(site_var_combinations_deeper_depth_bvr,
                                                   .f = ~ generate_baseline_mean(targets = targets_insitu,
                                                                                 h = 35,
                                                                                 model_id = team_name,
                                                                                 forecast_date = Sys.Date(),
                                                                                 #depth = 'target',
                                                                                 ...))

## GHG VARIABLES
site_var_combinations_ghg_insitu <- expand.grid(var = c('CH4_umolL_sample',
                                                        'CO2_umolL_sample'),
                                                site = c('fcre',
                                                         'bvre'))

historic_mean_ghg_insitu <- purrr::pmap_dfr(site_var_combinations_ghg_insitu,
                                            .f = ~ generate_baseline_mean(targets = targets_insitu,
                                                                          h = 35,
                                                                          model_id = team_name,
                                                                          forecast_date = Sys.Date(),
                                                                          depth = c(0.1),
                                                                          ...))

## Productivity variables
site_var_combinations_productivity <- expand.grid(var = c(#'DeepChlorophyllMaximum_binary',
  'TotalConc_ugL_sample',
  'GreenAlgae_ugL_sample',
  'Bluegreens_ugL_sample',
  'BrownAlgae_ugL_sample',
  'MixedAlgae_ugL_sample'),
  # 'TotalConcCM_ugL_sample',
  # 'GreenAlgaeCM_ugL_sample',
  # 'BluegreensCM_ugL_sample',
  # 'BrownAlgaeCM_ugL_sample',
  # 'MixedAlgaeCM_ugL_sample',
  # 'ChlorophyllMaximum_depth_sample',
  # 'MOM_binary_sample',
  # 'MOM_min_sample',
  # 'MOM_max_sample'),
  site = c('fcre',
           'bvre'))

historic_insitu_productivity <- purrr::pmap_dfr(site_var_combinations_productivity,
                                                .f = ~ generate_baseline_mean(targets = targets_insitu,
                                                                              h = 35,
                                                                              forecast_date = Sys.Date(),
                                                                              depth = 'target',
                                                                              ...))

## CHLA maxiumum variables
cmax_vars <- c('DeepChlorophyllMaximum_binary_sample',
               'TotalConcCM_ugL_sample',
               'GreenAlgaeCM_ugL_sample',
               'BluegreensCM_ugL_sample',
               'BrownAlgaeCM_ugL_sample',
               'MixedAlgaeCM_ugL_sample',
               'ChlorophyllMaximum_depth_sample',
               'MOM_binary_sample',
               'MOM_min_sample',
               'MOM_max_sample')

targets_cmax <- targets_insitu |> dplyr::filter(variable %in% cmax_vars) |>
  mutate(depth_m = NA)

site_var_combinations_chla_max <- expand.grid(var = cmax_vars,
                                              site = c('fcre',
                                                       'bvre'))

climatology_insitu_chla_max <- purrr::pmap_dfr(site_var_combinations_chla_max,
                                               .f = ~ generate_baseline_mean(targets = targets_cmax,
                                                                             h = 35,
                                                                             forecast_date = Sys.Date(),
                                                                             depth = 'target',
                                                                             ...))

## CHEM variables
site_var_combinations_chem <- expand.grid(var = c('TN_ugL_sample',
                                                  'TP_ugL_sample',
                                                  'SRP_ugL_sample',
                                                  'NO3NO2_ugL_sample',
                                                  'NH4_ugL_sample',
                                                  'DOC_mgL_sample',
                                                  'DRSI_mgL_sample',
                                                  #'DIC_mgL_samlpe',
                                                  'DC_mgL_sample',
                                                  'DN_mgL_sample'),
                                          site = c('fcre',
                                                   'bvre'))

targets_insitu <- targets_insitu |>
  mutate(depth_m = ifelse(variable %in% c('TN_ugL_sample',
                                          'TP_ugL_sample',
                                          'SRP_ugL_sample',
                                          'NO3NO2_ugL_sample',
                                          'NH4_ugL_sample',
                                          'DOC_mgL_sample') & site_id == 'bvre',
                          1.5,
                          depth_m))

targets_insitu <- targets_insitu |>
  mutate(depth_m = ifelse(variable == 'DRSI_mgL_sample' & depth_m %in% c(0.1, 4, 5),
                          1.5,
                          depth_m))

historic_insitu_chem <- purrr::pmap_dfr(site_var_combinations_chem,
                                        .f = ~ generate_baseline_mean(targets = targets_insitu,
                                                                      h = 35,
                                                                      forecast_date = Sys.Date(),
                                                                      depth = 'target',
                                                                      ...))

## Physical variables
site_var_combinations_physical <- expand.grid(var = c('ThermoclineDepth_m_mean',
                                                      'SchmidtStability_Jm2_mean'),
                                              site = c('fcre',
                                                       'bvre'))

historic_insitu_physical <- purrr::pmap_dfr(site_var_combinations_physical,
                                            .f = ~ generate_baseline_mean(targets = targets_insitu,
                                                                          h = 35,
                                                                          forecast_date = Sys.Date(),
                                                                          depth = 'target',
                                                                          ...))

# ## Generate Metals
print('Metals model')

site_var_combinations_metals <- expand.grid(var = c('TFe_mgL_sample',
                                                    'SFe_mgL_sample',
                                                    'TMn_mgL_sample',
                                                    'SMn_mgL_sample',
                                                    ''),
                                            site = c('fcre',
                                                     'bvre'))

historic_insitu_metals <- purrr::pmap_dfr(site_var_combinations_metals,
                                          .f = ~ generate_baseline_mean(targets = targets_insitu,
                                                                        h = 35,
                                                                        forecast_date = Sys.Date(),
                                                                        depth = 'target',
                                                                        ...))

historic_insitu_metals$duration <- 'P1D'

# Flux variables
# get all combinations
print('Flux model')

historic_flux <- purrr::map_dfr(.x = c('CO2flux_umolm2s_mean', 'CH4flux_umolm2s_mean'),
                                .f = ~ generate_baseline_mean(targets = targets_insitu,
                                                              h = 35,
                                                              forecast_date = Sys.Date(),
                                                              site = 'fcre', depth = 'target', var = .x))

# Generate binary forecasts from continuous
binary_site_var_comb <- data.frame(site = c('fcre', 'bvre'),
                                   depth = c(1.6, 1.5))

historic_mean_insitu_binary <- purrr::pmap_dfr(binary_site_var_comb,
                                               .f = ~convert_continuous_binary(continuous_var = 'Chla_ugL_mean',
                                                                               binary_var = 'Bloom_binary_mean',
                                                                               forecast = historic_mean_insitu,
                                                                               targets = targets_insitu,
                                                                               threshold = 20,
                                                                               ...))

# combine and submit
combined_historic_mean <- bind_rows(historic_mean_inflow, historic_mean_insitu, historic_mean_met, historic_mean_insitu_binary, historic_flux,
                                    historic_insitu_productivity, historic_mean_ghg_insitu, historic_insitu_chem, historic_insitu_physical, historic_insitu_metals,
                                    climatology_insitu_chla_max, historic_mean_insitu_deeper_fcr, historic_mean_insitu_deeper_bvr)

# write forecast file
file_date <- combined_historic_mean$reference_datetime[1]

forecast_file <- paste0(paste("daily", file_date, team_name, sep = "-"), ".csv.gz")

#write_csv(combined_historic_mean, forecast_file)

### VARIABLES OF INTEREST, FORECAST AT SPECIFIC DEPTHS ####
# Same variables and depths as the climatology and persistence models (see
# baseline_models/climatology.R and baseline_models/run_persistence.R), so the three
# baselines can be compared like for like: a surface and a hypolimnetic depth at each
# site, BVR 0.1 / 6 m and FCR 0.1 / 9 m.
print('Variables of interest, by depth')

interested_vars <- c('SRP_ugL_sample',
                     'NO3NO2_ugL_sample',
                     'NH4_ugL_sample',
                     'DOC_mgL_sample',
                     'CH4_umolL_sample',
                     'CO2_umolL_sample')

# NOTE: BVR grab samples are taken at 0.1 / 3 / 6 / 9 m -- there is no chem/GHG data at
# 1.5 m (that is the sensor depth), so 3 m is the option if a mid-depth is ever wanted.
interested_site_depths <- dplyr::bind_rows(
  tidyr::expand_grid(site = 'bvre', depth = c(0.1, 6)),
  tidyr::expand_grid(site = 'fcre', depth = c(0.1, 9)))

# one row per site/depth/variable -> one call per combination, so each depth gets its own
# historic mean
site_var_depth_interested <- tidyr::expand_grid(var = interested_vars,
                                                interested_site_depths)

interested_historic_mean <- purrr::pmap_dfr(site_var_depth_interested,
                                            .f = ~ generate_baseline_mean(targets = targets_insitu_raw,
                                                                          h = 35,
                                                                          model_id = team_name,
                                                                          forecast_date = Sys.Date(),
                                                                          ...))

# which site/depth/variable combinations actually produced a forecast?
interested_historic_mean |>
  dplyr::distinct(site_id, variable, depth_m) |>
  dplyr::arrange(site_id, variable, depth_m) |>
  print(n = Inf)

###plot####
interested_historic_mean %>%
  filter(family == 'normal') |>
  pivot_wider(names_from = parameter, values_from = prediction) |>
  mutate(depth_m = as_factor(depth_m)) |>
  ggplot(aes(x = datetime, y = mu, colour = depth_m, fill = depth_m, group = depth_m)) +
  geom_ribbon(aes(ymax = mu+sigma, ymin = mu-sigma), alpha = 0.2, colour = NA) +
  geom_line() +
  facet_grid(variable~site_id, scales = 'free') +
  labs(colour = 'Depth (m)', fill = 'Depth (m)') +
  # shrink the variable strip labels on the right so the long names stay readable
  theme(strip.text.y = element_text(size = 6),
        strip.text.x = element_text(size = 9))

combined_historic_mean %>%
  filter(family == 'normal') |>
  pivot_wider(names_from = parameter, values_from = prediction) |>
  ggplot(aes(x = datetime, y = mu)) +
  geom_line() +
  geom_ribbon(aes(ymax = mu+sigma, ymin = mu-sigma), alpha = 0.3, fill = 'blue') +
  facet_grid(variable~site_id, scales = 'free')

combined_historic_mean %>%
  filter(family == 'bernoulli') |>
  ggplot(aes(x = datetime, y = prediction, colour = as_factor(depth_m))) +
  geom_line() +
  facet_grid(variable~site_id, scales = 'free')


# vera4castHelpers::submit(forecast_file = forecast_file,
#                          ask = FALSE,
#                          first_submission = FALSE)

unlink(forecast_file)