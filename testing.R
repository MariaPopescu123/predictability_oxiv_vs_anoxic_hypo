#### what chem variables can I actually get out of FLARE? ####
## MP: August 2026
## pulls the variable list out of the FLARE glm_aed output for fcre (Falling
## Creek Reservoir) and bvre (Beaverdam Reservoir) and summarizes what is
## available for the chem variables.
##
## EVERYTHING here is site 50 (the deep hole / catwalk station) in both
## reservoirs - see the "#### 0. site 50" block for why.
##
## NOTES FROM WHERE CODE IS COMING FROM (cited again inline):
##  - s3_bucket + endpoint + the fcre parquet paths:
##      "helpful files/exploring_chem.R" lines 83-91 and 104-111
##  - AED name -> VERA name/unit conversions:
##      "helpful files/forecasting_conversion_notes.R" lines 134-161
##      (Austin's combined_run_aed.R, URL on that file's line 3)
##  - the VERA target list to check the conversions against:
##      "helpful files/forecasting_conversion_notes.R" lines 74-77
##  - daily-insitu targets URL:
##      "helpful files/exploring_chem.R" lines 6 and 40
##  - n_members = 150:
##      "helpful files/exploring_chem.R" line 37
##  - horizon == 0 "nowcast" trick (reference_datetime == datetime):
##      "helpful files/exploring_chem.R" lines 219-221
##
## note: don't aggregate over the whole dataset, it is enormous and all over the
## network. the buckets are partitioned by reference_date, so the date list comes
## from $ls() and everything else from one reference_date folder.


library(tidyverse)
library(arrow)

out_dir <- "./model_output"
osn <- "amnh1.osn.mghpcc.org"   # OSN endpoint, same as "helpful files/exploring_chem.R" line 84

sites <- c("fcre", "bvre")      # used to filter the targets file later


#### 0. site 50 ####
# FORECAST SIDE: FLARE/GLM-AED is a 1-D model of the deep hole. The bucket is
# partitioned by site_id (fcre / bvre) only - there is no Site column, because a
# FLARE run IS the site-50 water column. The depth check in section 6b confirms
# the profile actually reaches the deep-hole depth.
#
# OBSERVATION SIDE: the VERA targets file has site_id but no Site column,
# because Site==50 is filtered upstream. Lines in LTREB-reservoirs/vera4cast,
# targets/target_functions/:
#   target_generation_chemistry_daily.R  line 34  filter(Site==50)   -> NH4, NO3NO2, SRP, DIC
#   target_generation_ghg_daily.R        line 25  filter(Site==50)   -> CH4
#   target_generation_daily_secchi_m.R   line 26  subset(Site=="50") -> Secchi
#   target_generation_FluoroProbe.R      line 69  Site == 50
#   target_generation_exo_daily.R        lines 24-38  no Site filter needed, the inputs
#                                        ARE the site-50 platforms (FCR catwalk EDI 271,
#                                        BVR platform EDI 725) -> DO, Chla, fDOM
#   EXCEPTION: generate_EddyFlux_ghg_targets_function.R line 208 sets Reservoir='fcre'
#   with NO Site filter. CO2flux / CH4flux are eddy-covariance tower fluxes over a
#   wind footprint = whole reservoir, not site 50. Dropped below.
site50_only <- TRUE

# max depth at site 50, from the VERA site table
# https://raw.githubusercontent.com/LTREB-reservoirs/vera4cast/main/vera4cast_field_site_metadata.csv
fcre_max_depth <- 9.3    # Falling Creek
bvre_max_depth <- 13.4   # Beaverdam


#### 1. the buckets ####
# only fcre and bvre run glm_aed. everything else in the flare bucket (ccre,
# sunp, TOOK, SUGG) is glm_flare_v1/v3 - physics only, no chemistry.
# paths and arguments copied from "helpful files/exploring_chem.R" lines 83-91.

fcre_path <- "bio230121-bucket01/flare/forecasts/parquet/site_id=fcre/model_id=glm_aed_flare_v3"
bvre_path <- "bio230121-bucket01/flare/forecasts/parquet/site_id=bvre/model_id=glm_aed_flare_v3"

# the reforecast is a retrospective rerun of the SAME model (the glm3.nml /
# aed2.nml / states_config files are identical to the operational run) - it just
# backfills May-Oct 2024. fcre only, there is no bvre reforecast.
fcre_reforecast_path <- "bio230121-bucket01/fcre-reforecast/forecasts/parquet/site_id=fcre/model_id=glm_aed_flare_v3_reforecast"

fcre_bucket <- arrow::s3_bucket(fcre_path, endpoint_override = osn, anonymous = TRUE)
bvre_bucket <- arrow::s3_bucket(bvre_path, endpoint_override = osn, anonymous = TRUE)
fcre_reforecast_bucket <- arrow::s3_bucket(fcre_reforecast_path, endpoint_override = osn, anonymous = TRUE)


#### 2. what reference dates exist ####
# $ls() lists the "reference_date=YYYY-MM-DD" folders. str_remove strips the
# prefix, ymd() makes them real dates, sort() puts them in order.
# the strip-the-prefix idiom is Austin's, "helpful files/forecasting_conversion_notes.R" line 35.

fcre_dates <- fcre_bucket$ls() |> str_remove("reference_date=") |> ymd() |> sort()
bvre_dates <- bvre_bucket$ls() |> str_remove("reference_date=") |> ymd() |> sort()
fcre_reforecast_dates <- fcre_reforecast_bucket$ls() |> str_remove("reference_date=") |> ymd() |> sort()

# one row per run: how many forecasts, over what window, and how many days inside
# that window have no forecast at all (setdiff of the full daily sequence vs what is there)
coverage <- bind_rows(
  tibble(site = "fcre", run = "glm_aed_flare_v3 (operational)",
         n_dates = length(fcre_dates),
         first_ref = min(fcre_dates), last_ref = max(fcre_dates),
         missing_days = length(setdiff(seq(min(fcre_dates), max(fcre_dates), by = "day"), fcre_dates))),
  tibble(site = "bvre", run = "glm_aed_flare_v3 (operational)",
         n_dates = length(bvre_dates),
         first_ref = min(bvre_dates), last_ref = max(bvre_dates),
         missing_days = length(setdiff(seq(min(bvre_dates), max(bvre_dates), by = "day"), bvre_dates))),
  tibble(site = "fcre", run = "glm_aed_flare_v3_reforecast",
         n_dates = length(fcre_reforecast_dates),
         first_ref = min(fcre_reforecast_dates), last_ref = max(fcre_reforecast_dates),
         missing_days = length(setdiff(seq(min(fcre_reforecast_dates), max(fcre_reforecast_dates), by = "day"),
                                       fcre_reforecast_dates))))

message("reference date coverage:")
print(coverage)

# the fcre operational and reforecast records overlap on 23 reference dates
# (2024-09-25 to 2024-10-17). the operational run is the one to trust, so the
# reforecast is only ever allowed to contribute the dates the operational never
# covered. pull_chem() uses this vector so those 23 days can't come back twice.
# setdiff() strips the Date class, hence as_date().
fcre_reforecast_only_dates <- as_date(setdiff(fcre_reforecast_dates, fcre_dates))

message("fcre operational / reforecast overlap: ",
        length(intersect(fcre_dates, fcre_reforecast_dates)), " reference dates (reforecast dropped for these)")
message("reforecast contributes ", length(fcre_reforecast_only_dates),
        " extra dates: ", min(fcre_reforecast_only_dates), " to ", max(fcre_reforecast_only_dates))


#### 3. what variables come out at each reservoir ####
# one reference_date folder is enough, the variable set doesn't change day to day.
# open_dataset on a single partition = small, fast pull.

fcre_latest <- max(fcre_dates)
bvre_latest <- max(bvre_dates)

fcre_ds <- arrow::s3_bucket(paste0(fcre_path, "/reference_date=", fcre_latest),
                            endpoint_override = osn, anonymous = TRUE) |> arrow::open_dataset()
bvre_ds <- arrow::s3_bucket(paste0(bvre_path, "/reference_date=", bvre_latest),
                            endpoint_override = osn, anonymous = TRUE) |> arrow::open_dataset()

# distinct() runs on the server, collect() brings back only the short result
fcre_vars <- fcre_ds |> distinct(variable) |> collect() |> pull(variable) |> sort()
bvre_vars <- bvre_ds |> distinct(variable) |> collect() |> pull(variable) |> sort()

message("fcre (Falling Creek): ", length(fcre_vars), " variables")
print(fcre_vars)
message("bvre (Beaverdam): ", length(bvre_vars), " variables")
print(bvre_vars)

message("in both reservoirs:")
print(intersect(fcre_vars, bvre_vars))
message("Falling Creek only (the OGM organic matter pool, CAR_ch4, oxy_mean, surface fluxes):")
print(setdiff(fcre_vars, bvre_vars))
message("Beaverdam only (the ZOO groups and the PHY internal N/P stores):")
print(setdiff(bvre_vars, fcre_vars))


#### 4. which of those are chem ####
# AED module prefixes: OXY, NIT (N), PHS (P), CAR (C/DIC/CH4), OGM (organic
# matter), SIL (Si), PHY (phyto), ZOO (zooplankton), plus the chem variables
# FLARE already names itself. those already-named ones are exactly the names
# Austin converts in "helpful files/forecasting_conversion_notes.R" lines 137-159.
chem_pattern <- paste0("^(OXY|NIT|PHS|CAR|OGM|SIL|PHY|ZOO|TRC|NCS)_",
                       "|^(oxy|chla|fdom|secchi|extc)|_flux_|^DO_|^Chla|^fDOM|Rdom")

fcre_chem <- fcre_vars[str_detect(fcre_vars, regex(chem_pattern, ignore_case = TRUE))]
bvre_chem <- bvre_vars[str_detect(bvre_vars, regex(chem_pattern, ignore_case = TRUE))]

message("fcre (Falling Creek): ", length(fcre_chem), " chem variables")
print(fcre_chem)
message("bvre (Beaverdam): ", length(bvre_chem), " chem variables")
print(bvre_chem)

# what the regex did NOT catch at fcre - should be temp/ice/mixing state and
# calibrated parameters. a check that no chem is being dropped.
message("non-chem (physics / state / calibrated parameters), fcre:")
print(setdiff(fcre_vars, fcre_chem))


#### 5. AED -> VERA lookup ####
# every row is a transcription of a line in "helpful files/forecasting_conversion_notes.R":
#   oxy_mean      -> DO_mgL_mean          lines 137-140
#   NIT_amm       -> NH4_ugL_sample       lines 146-147
#   NIT_nit       -> NO3NO2_ugL_sample    line 148 (+ line 149, the buggy rename, see NOTE)
#   PHS_frp       -> SRP_ugL_sample       lines 150-151
#   CAR_dic       -> DIC_mgL_sample       lines 152-153
#   CAR_ch4       -> CH4_umolL_sample     line 154
#   fDOM_QSU_mean                         line 145
#   Chla_ugL_mean -> Bloom_binary_mean    lines 80-83 (>20 ugL at 1.6 m)
#   secchi        -> Secchi_m_sample      line 155
#   co2_flux_mean -> CO2flux_umolm2s_mean lines 156-157
#   ch4_flux_mean -> CH4flux_umolm2s_mean lines 158-159
# vera_variable spellings checked against that file's lines 74-77.

# obs_site50 = is the matching VERA observation a site-50 sample (see section 0)
chem_lookup <- tribble(
  ~flare_variable,  ~vera_variable,          ~conversion,                                        ~obs_source,                              ~obs_site50,
  "oxy_mean",       "DO_mgL_mean",           "/1000*32, forced to depth 1.6, datetime - 1 day",  "exo_daily (catwalk EDI 271/725)",        TRUE,
  "DO_mgL_mean",    "DO_mgL_mean",           "already converted by FLARE",                       "exo_daily (catwalk EDI 271/725)",        TRUE,
  "NIT_amm",        "NH4_ugL_sample",        "/1000/0.001/(1/18.04)",                            "chemistry_daily (Site==50)",             TRUE,
  "NIT_nit",        "NO3NO2_ugL_sample",     "/1000/0.001/(1/62.00)",                            "chemistry_daily (Site==50)",             TRUE,
  "PHS_frp",        "SRP_ugL_sample",        "/1000/0.001/(1/94.9714)",                          "chemistry_daily (Site==50)",             TRUE,
  "CAR_dic",        "DIC_mgL_sample",        "/1000/(1/52.515)",                                 "chemistry_daily (Site==50)",             TRUE,
  "CAR_ch4",        "CH4_umolL_sample",      "none",                                             "ghg_daily (Site==50)",                   TRUE,
  "fDOM_QSU_mean",  "fDOM_QSU_mean",         "(151.3407 + prediction)/29.62654",                 "exo_daily (catwalk EDI 271/725)",        TRUE,
  "Chla_ugL_mean",  "Chla_ugL_mean",         "none (>20 ugL at 1.6m = Bloom_binary_mean)",       "exo_daily (catwalk EDI 271/725)",        TRUE,
  "secchi",         "Secchi_m_sample",       "none (derived from extc)",                         "secchi_daily (Site==\"50\")",            TRUE,
  "co2_flux_mean",  "CO2flux_umolm2s_mean",  "/0.001/86400",                                     "EddyFlux tower footprint - NOT site 50", FALSE,
  "ch4_flux_mean",  "CH4flux_umolm2s_mean",  "/0.001/86400",                                     "EddyFlux tower footprint - NOT site 50", FALSE)

# does FLARE actually produce each variable at each reservoir
chem_lookup <- chem_lookup |>
  mutate(in_fcre = flare_variable %in% fcre_vars,
         in_bvre = flare_variable %in% bvre_vars)

# drop the eddy-flux rows so every forecast/observation pair is a site-50 comparison
if(site50_only){
  message("site50_only = TRUE, dropping: ",
          paste(chem_lookup$vera_variable[!chem_lookup$obs_site50], collapse = ", "))
  chem_lookup <- chem_lookup |> filter(obs_site50)
}

print(chem_lookup, n = Inf)

# chem output with no VERA target to score against (model state only)
message("no VERA target, fcre:")
print(setdiff(fcre_chem, chem_lookup$flare_variable))
message("no VERA target, bvre:")
print(setdiff(bvre_chem, chem_lookup$flare_variable))


#### 6. structure of each chem variable ####
# pull the chem variables from the most recent forecast at each reservoir, tag
# each with its site, and stack them. one collected dataframe, reused in
# section 6b and section 10 so the network only gets hit once.

fcre_chem_data <- fcre_ds |>
  filter(variable %in% fcre_chem) |>
  select(datetime, depth, parameter, variable, prediction, variable_type) |>
  collect() |>
  mutate(site = "fcre")

bvre_chem_data <- bvre_ds |>
  filter(variable %in% bvre_chem) |>
  select(datetime, depth, parameter, variable, prediction, variable_type) |>
  collect() |>
  mutate(site = "bvre")

chem_data <- bind_rows(fcre_chem_data, bvre_chem_data)

chem_structure <- chem_data |>
  group_by(site, variable) |>
  summarise(variable_type = paste(unique(variable_type), collapse = ", "),   # state / diagnostic / parameter
            n_depths      = n_distinct(depth[!is.na(depth)]),                # depth layers in the profile
            depth_range   = if(all(is.na(depth))) "no depth (whole-lake / surface)" else
                              paste0(min(depth, na.rm = TRUE), " - ", max(depth, na.rm = TRUE)),
            n_members     = n_distinct(parameter),                           # ensemble size
            horizon_days  = as.numeric(difftime(max(datetime), min(datetime), units = "days")),
            mean_pred     = mean(prediction, na.rm = TRUE),                  # magnitude check, raw AED units
            prop_na       = mean(is.na(prediction)),
            .groups = "drop") |>
  arrange(variable, site)

print(chem_structure, n = Inf)

# depth resolution differs by reservoir - this is what matters for hypo work.
# Falling Creek 0-9 m (11 layers), Beaverdam 0-13 m (24 layers).
message("depths available per reservoir:")
print(chem_structure |> distinct(site, n_depths, depth_range) |> arrange(site, n_depths))


#### 6b. site 50 depth check ####
# the bottom of the FLARE profile should land near the deep-hole max depth.
# if a run stopped well short of that it would not be site 50.
depth_check <- chem_data |>
  filter(!is.na(depth)) |>
  group_by(site) |>
  summarise(deepest_layer = max(depth), .groups = "drop") |>
  mutate(site50_max_depth = if_else(site == "fcre", fcre_max_depth, bvre_max_depth),
         is_deep_hole     = deepest_layer >= site50_max_depth - 1.5)  # allow for drawdown

message("site 50 depth check:")
print(depth_check)


#### 7. how does that line up with the targets ####
# same URL as "helpful files/exploring_chem.R" lines 6 and 40
targets_url <- "https://amnh1.osn.mghpcc.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz"

# site_id is the only site key in this file - fcre = Falling Creek site 50,
# bvre = Beaverdam site 50, because Site==50 was filtered upstream (section 0).
targets <- readr::read_csv(targets_url, show_col_types = FALSE) |>
  filter(site_id %in% sites)

# if the targets file ever gains a Site column, filter it rather than silently
# mixing in the stream / upstream stations
if("Site" %in% names(targets)) targets <- targets |> filter(Site == 50)

# per reservoir x variable: how many observations, over what window, at what depths
target_avail <- targets |>
  filter(variable %in% chem_lookup$vera_variable, !is.na(observation)) |>
  mutate(datetime = as_date(datetime)) |>
  group_by(site = site_id, variable) |>
  summarise(n_obs        = n(),
            first_obs    = min(datetime),
            last_obs     = max(datetime),
            n_obs_depths = n_distinct(depth_m),
            obs_depths   = paste(sort(unique(depth_m)), collapse = ", "),
            .groups = "drop")

print(target_avail, n = Inf)

# one row per reservoir x variable: forecast from FLARE + observed in targets =
# something I can score. two copies of the lookup, one tagged fcre and one bvre,
# each keeping only its own availability flag.
chem_summary <- bind_rows(
  chem_lookup |> mutate(site = "fcre", in_flare = in_fcre),
  chem_lookup |> mutate(site = "bvre", in_flare = in_bvre)) |>
  select(-in_fcre, -in_bvre) |>
  left_join(chem_structure, by = c("site", "flare_variable" = "variable")) |>  # forecast side
  left_join(target_avail,   by = c("site", "vera_variable"  = "variable")) |>  # observation side
  mutate(has_targets = !is.na(n_obs),
         usable      = in_flare & has_targets) |>
  arrange(site, desc(usable), vera_variable)

print(chem_summary |> select(site, flare_variable, vera_variable, in_flare,
                             n_depths, n_obs, first_obs, last_obs, usable), n = Inf)

# the punchline, per reservoir
fcre_usable <- chem_summary |> filter(site == "fcre", usable) |> pull(vera_variable) |> unique()
bvre_usable <- chem_summary |> filter(site == "bvre", usable) |> pull(vera_variable) |> unique()

message("usable chem at fcre (Falling Creek, site 50): ", paste(fcre_usable, collapse = ", "))
message("usable chem at bvre (Beaverdam, site 50): ",     paste(bvre_usable, collapse = ", "))
message("Falling Creek only: ", paste(setdiff(fcre_usable, bvre_usable), collapse = ", "))
message("Beaverdam only: ",     paste(setdiff(bvre_usable, fcre_usable), collapse = ", "))


#### 8. save so I don't have to re-pull ####
dir.create(out_dir, showWarnings = FALSE)
write_csv(coverage,       file.path(out_dir, "flare_date_coverage.csv"))
write_csv(chem_structure, file.path(out_dir, "flare_chem_structure.csv"))
write_csv(chem_summary,   file.path(out_dir, "flare_chem_vs_targets.csv"))


#### 9. helper for when I want the actual time series ####
# pulls one chem variable at one reservoir across a range of reference dates.
# horizon = 0 gives the FLARE "nowcast" (reference_datetime == datetime), which is
# what I used for temp/secchi in the DCM work - good historic covariate. that trick
# is from "helpful files/exploring_chem.R" lines 219-221.
# n_members = 150 matches "helpful files/exploring_chem.R" line 37.
# no Site filter needed - a FLARE run is the site-50 water column by construction.
pull_chem <- function(variable_name, site = "fcre", start_date, end_date, horizon = 0,
                      depths = NULL, n_members = 150, use_reforecast = TRUE){

  # the operational run first
  if(site == "fcre"){
    main_bucket <- fcre_bucket
  } else {
    main_bucket <- bvre_bucket
  }

  forecasts <- arrow::open_dataset(main_bucket) |>
    mutate(reference_datetime = as_date(reference_datetime)) |>
    filter(variable == variable_name,
           reference_datetime >= start_date,
           reference_datetime <= end_date,
           parameter <= n_members) |>
    collect()

  # fcre only: top up with the reforecast, but ONLY for reference dates the
  # operational never had (fcre_reforecast_only_dates, section 2). that is what
  # stops the 23 overlapping dates being pulled twice - on any date both runs
  # issued, the operational forecast is the one that is kept.
  if(site == "fcre" & use_reforecast){
    reforecasts <- arrow::open_dataset(fcre_reforecast_bucket) |>
      mutate(reference_datetime = as_date(reference_datetime)) |>
      filter(variable == variable_name,
             reference_datetime >= start_date,
             reference_datetime <= end_date,
             reference_datetime %in% fcre_reforecast_only_dates,
             parameter <= n_members) |>
      collect()

    forecasts <- bind_rows(forecasts, reforecasts)
  }

  out <- forecasts |>
    mutate(datetime = as_date(datetime),
           horizon_days = as.numeric(datetime - reference_datetime)) |>
    filter(horizon_days == horizon)

  if(!is.null(depths)) out <- out |> filter(depth %in% depths)

  out |>
    group_by(datetime, depth) |>
    summarise(mean_pred = mean(prediction, na.rm = TRUE),   # ensemble mean
              sd_pred   = sd(prediction, na.rm = TRUE),     # ensemble spread
              .groups   = "drop") |>
    mutate(site = site, variable = variable_name)
}

# hypolimnetic oxygen at Falling Creek site 50 (9 m):
# oxy_9m_fcre <- pull_chem("DO_mgL_mean", "fcre", min(fcre_dates), max(fcre_dates), depths = 9)
# and at Beaverdam - deeper hypolimnion, 13 m not 9 m:
# oxy_13m_bvre <- pull_chem("DO_mgL_mean", "bvre", min(bvre_dates), max(bvre_dates), depths = 13)


#### 10. quick look ####
# reusing chem_data from section 6 so this doesn't hit the network again.
# rows = variable, columns = reservoir.
chem_data |>
  filter(variable %in% chem_lookup$flare_variable) |>
  group_by(site, variable, datetime, depth) |>
  summarise(prediction = mean(prediction, na.rm = TRUE), .groups = "drop") |>   # collapse the ensemble
  ggplot(aes(x = datetime, y = prediction, group = interaction(depth, site), color = depth)) +
  geom_line() +
  facet_grid(variable ~ site, scales = "free_y") +
  theme_bw() +
  labs(title = "most recent FLARE chem forecast, site 50: fcre (Falling Creek) vs bvre (Beaverdam)",
       x = NULL, y = NULL, color = "depth (m)")
