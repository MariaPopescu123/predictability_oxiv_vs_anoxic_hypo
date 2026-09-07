#### what chem variables can I actually get out of FLARE? ####
## MP: August 2026
## Pulls the chem variables out of the FLARE glm_aed output for fcre (Falling
## Creek Reservoir) and bvre (Beaverdam Reservoir), summarizes the forecast
## structure (depths, ensemble size, horizon), and lines that up against the VERA
## targets to see what is actually scoreable.
##
## EVERYTHING here is site 50 (the deep hole / catwalk station) in both
## reservoirs - see the "#### 0. site 50" block for why.
##
## NOTES FROM WHERE CODE IS COMING FROM (cited again inline):
##   s3 paths + OSN endpoint      "helpful files/exploring_chem.R" lines 83-91, 104-111
##   AED -> VERA conversions      "helpful files/forecasting_conversion_notes.R" lines 134-161
##   VERA target name spellings   "helpful files/forecasting_conversion_notes.R" lines 74-77
##   daily-insitu targets URL     "helpful files/exploring_chem.R" lines 6, 40
##   n_members = 150              "helpful files/exploring_chem.R" line 37
##   horizon == 0 nowcast trick   "helpful files/exploring_chem.R" lines 219-221
##
## note: don't aggregate over the whole dataset, it is enormous and all over the
## network. the buckets are partitioned by reference_date, so the date list comes
## from $ls() and everything else from one reference_date folder.


library(tidyverse)
library(arrow)

out_dir <- "./model_output"
osn     <- "amnh1.osn.mghpcc.org"   # OSN endpoint, "helpful files/exploring_chem.R" line 84
sites   <- c("fcre", "bvre")


#### 0. site 50 ####
# FORECAST SIDE: FLARE/GLM-AED is a 1-D model of the deep hole. The bucket is
# partitioned by site_id (fcre / bvre) only
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
# The one FLARE output whose observations are NOT site 50 is the eddy-flux pair;
# it is left out of the lookup table in section 5, see the comment there.

# max depth at site 50, from the VERA site table
# https://raw.githubusercontent.com/LTREB-reservoirs/vera4cast/main/vera4cast_field_site_metadata.csv
site50_max_depth <- c(fcre = 9.3, bvre = 13.4)


#### 1. the buckets ####
# only fcre and bvre run glm_aed. everything else in the flare bucket (ccre,
# sunp, TOOK, SUGG) is glm_flare_v1/v3 - physics only, no chemistry.
# paths and arguments copied from "helpful files/exploring_chem.R" lines 83-91.

open_osn <- function(path) arrow::s3_bucket(path, endpoint_override = osn, anonymous = TRUE)

flare_path <- c(
  fcre = "bio230121-bucket01/flare/forecasts/parquet/site_id=fcre/model_id=glm_aed_flare_v3",
  bvre = "bio230121-bucket01/flare/forecasts/parquet/site_id=bvre/model_id=glm_aed_flare_v3")

# the reforecast is a retrospective rerun of the SAME model (the glm3.nml /
# aed2.nml / states_config files are identical to the operational run) - it just
# backfills May-Oct 2024. fcre only, there is no bvre reforecast.
reforecast_path <- "bio230121-bucket01/fcre-reforecast/forecasts/parquet/site_id=fcre/model_id=glm_aed_flare_v3_reforecast"

flare_bucket      <- map(flare_path, open_osn)   # named list: $fcre, $bvre
reforecast_bucket <- open_osn(reforecast_path)


#### 2. what reference dates exist ####
# $ls() lists the "reference_date=YYYY-MM-DD" folders. str_remove strips the
# prefix, ymd() makes them real dates, sort() puts them in order.
# the strip-the-prefix idiom is Austin's, "helpful files/forecasting_conversion_notes.R" line 35.

list_ref_dates <- function(bucket) bucket$ls() |> str_remove("reference_date=") |> ymd() |> sort()

ref_dates        <- map(flare_bucket, list_ref_dates)
reforecast_dates <- list_ref_dates(reforecast_bucket)

# one row per run: how many forecasts, over what window, and how many days inside
# that window have no forecast at all (setdiff of the full daily sequence vs what is there)
describe_dates <- function(dates, site, run){
  tibble(site = site, run = run,
         n_dates      = length(dates),
         first_ref    = min(dates),
         last_ref     = max(dates),
         missing_days = length(setdiff(seq(min(dates), max(dates), by = "day"), dates)))
}

coverage <- bind_rows(
  describe_dates(ref_dates$fcre,   "fcre", "glm_aed_flare_v3 (operational)"),
  describe_dates(ref_dates$bvre,   "bvre", "glm_aed_flare_v3 (operational)"),
  describe_dates(reforecast_dates, "fcre", "glm_aed_flare_v3_reforecast"))

message("reference date coverage:")
print(coverage)

# the fcre operational and reforecast records overlap on 23 reference dates
# (2024-09-25 to 2024-10-17). the operational run is the one to trust, so the
# reforecast is only ever allowed to contribute the dates the operational never
# covered. pull_chem() uses this vector so those 23 days can't come back twice.
# setdiff() strips the Date class, hence as_date().
reforecast_only_dates <- as_date(setdiff(reforecast_dates, ref_dates$fcre))

message("fcre operational / reforecast overlap: ",
        length(intersect(ref_dates$fcre, reforecast_dates)), " reference dates (reforecast dropped for these)")
message("reforecast contributes ", length(reforecast_only_dates),
        " extra dates: ", min(reforecast_only_dates), " to ", max(reforecast_only_dates))


#### 3. what variables come out at each reservoir ####
# one reference_date folder is enough, the variable set doesn't change day to day.
# open_dataset on a single partition = small, fast pull.
# distinct() runs on the server, collect() brings back only the short result.

latest_ds <- imap(flare_path, \(path, site)
  open_dataset(open_osn(paste0(path, "/reference_date=", max(ref_dates[[site]])))))

flare_vars <- map(latest_ds, \(ds) ds |> distinct(variable) |> collect() |> pull(variable) |> sort())

message("fcre (Falling Creek): ", length(flare_vars$fcre), " variables")
print(flare_vars$fcre)
message("bvre (Beaverdam): ", length(flare_vars$bvre), " variables")
print(flare_vars$bvre)

message("in both reservoirs:")
print(intersect(flare_vars$fcre, flare_vars$bvre))
message("Falling Creek only (the OGM organic matter pool, CAR_ch4, oxy_mean, surface fluxes):")
print(setdiff(flare_vars$fcre, flare_vars$bvre))
message("Beaverdam only (the ZOO groups and the PHY internal N/P stores):")
print(setdiff(flare_vars$bvre, flare_vars$fcre))


#### 4. which of those are chem ####
# two rules: an AED module prefix, or one of the chem variables FLARE already
# names itself. that second list is exactly the set Austin converts in
# "helpful files/forecasting_conversion_notes.R" lines 137-159, plus the two
# FLARE reports without a VERA name (extc, Rdom_minerl).
aed_modules <- c("OXY", "NIT", "PHS", "CAR", "OGM", "SIL", "PHY", "ZOO", "TRC", "NCS")

flare_named_chem <- c("oxy_mean", "DO_mgL_mean", "Chla_ugL_mean", "fDOM_QSU_mean",
                      "secchi", "extc", "Rdom_minerl", "co2_flux_mean", "ch4_flux_mean")

is_chem <- function(v){
  str_detect(v, paste0("^(", paste(aed_modules, collapse = "|"), ")_")) | v %in% flare_named_chem
}

chem_vars <- map(flare_vars, \(v) v[is_chem(v)])

message("fcre (Falling Creek): ", length(chem_vars$fcre), " chem variables")
print(chem_vars$fcre)
message("bvre (Beaverdam): ", length(chem_vars$bvre), " chem variables")
print(chem_vars$bvre)

# what the rules did NOT catch at fcre - should be temp/ice/mixing state and
# calibrated parameters. a check that no chem is being dropped.
message("non-chem (physics / state / calibrated parameters), fcre:")
print(setdiff(flare_vars$fcre, chem_vars$fcre))


#### 5. AED -> VERA lookup ####
# every row is a transcription of a line in "helpful files/forecasting_conversion_notes.R":
#   oxy_mean      -> DO_mgL_mean          lines 137-140
#   NIT_amm       -> NH4_ugL_sample       lines 146-147
#   ... (see that file for the rest)
# vera_variable spellings checked against that file's lines 74-77.
#
# DELIBERATELY NOT IN THIS TABLE: co2_flux_mean / ch4_flux_mean. FLARE produces
# them at fcre and VERA does have targets (CO2flux_umolm2s_mean and
# CH4flux_umolm2s_mean, both /0.001/86400, lines 156-159), but the observations
# are eddy-covariance tower fluxes over a wind footprint - the whole reservoir,
# not site 50. generate_EddyFlux_ghg_targets_function.R line 208 sets
# Reservoir='fcre' with no Site filter. Scoring a whole-reservoir flux against a
# 1-D site-50 water column would not be a like-for-like comparison, and surface
# GHG flux is not part of the hypolimnion question anyway. Everything left in
# this table pairs a site-50 forecast with a site-50 observation.
chem_lookup <- tribble(
  ~flare_variable,  ~vera_variable,          ~conversion,                                        ~obs_source,
  "oxy_mean",       "DO_mgL_mean",           "/1000*32, forced to depth 1.6, datetime - 1 day",  "exo_daily (catwalk EDI 271/725)",
  "DO_mgL_mean",    "DO_mgL_mean",           "already converted by FLARE",                       "exo_daily (catwalk EDI 271/725)",
  "NIT_amm",        "NH4_ugL_sample",        "/1000/0.001/(1/18.04)",                            "chemistry_daily (Site==50)",
  "NIT_nit",        "NO3NO2_ugL_sample",     "/1000/0.001/(1/62.00)",                            "chemistry_daily (Site==50)",
  "PHS_frp",        "SRP_ugL_sample",        "/1000/0.001/(1/94.9714)",                          "chemistry_daily (Site==50)",
  "CAR_dic",        "DIC_mgL_sample",        "/1000/(1/52.515)",                                 "chemistry_daily (Site==50)",
  "CAR_ch4",        "CH4_umolL_sample",      "none",                                             "ghg_daily (Site==50)",
  "fDOM_QSU_mean",  "fDOM_QSU_mean",         "(151.3407 + prediction)/29.62654",                 "exo_daily (catwalk EDI 271/725)",
  "Chla_ugL_mean",  "Chla_ugL_mean",         "none (>20 ugL at 1.6m = Bloom_binary_mean)",       "exo_daily (catwalk EDI 271/725)",
  "secchi",         "Secchi_m_sample",       "none (derived from extc)",                         "secchi_daily (Site==\"50\")")

# does FLARE actually produce each variable at each reservoir
chem_lookup <- chem_lookup |>
  mutate(in_fcre = flare_variable %in% flare_vars$fcre,
         in_bvre = flare_variable %in% flare_vars$bvre)

print(chem_lookup, n = Inf)

# chem output this script will not score: model state with no VERA target at all
# (the OGM / PHY / ZOO / SIL pools, extc, Rdom_minerl) plus the two eddy-flux
# variables excluded above.
message("not scored here, fcre:")
print(setdiff(chem_vars$fcre, chem_lookup$flare_variable))
message("not scored here, bvre:")
print(setdiff(chem_vars$bvre, chem_lookup$flare_variable))


#### 6. structure of each chem variable ####
# pull the chem variables from the most recent forecast at each reservoir and
# stack them. one collected dataframe, reused in section 6b and section 10 so
# the network only gets hit once.

chem_data <- imap(latest_ds, \(ds, site)
  ds |>
    filter(variable %in% chem_vars[[site]]) |>
    select(datetime, depth, parameter, variable, prediction, variable_type) |>
    collect()) |>
  bind_rows(.id = "site")

chem_structure <- chem_data |>
  group_by(site, variable) |>
  summarise(variable_type = paste(unique(variable_type), collapse = ", "),   # state / diagnostic / parameter
            n_depths      = n_distinct(depth[!is.na(depth)]),                # depth layers in the profile
            depth_range   = if(all(is.na(depth))) "no depth (whole-lake / surface)" else
                              paste0(min(depth, na.rm = TRUE), " - ", max(depth, na.rm = TRUE)),
            depths        = paste(sort(unique(depth)), collapse = ", "),     # the actual layers, not just the range
            n_members     = n_distinct(parameter),                           # ensemble size
            horizon_days  = as.numeric(difftime(max(datetime), min(datetime), units = "days")),
            mean_pred     = mean(prediction, na.rm = TRUE),                  # magnitude check, raw AED units
            prop_na       = mean(is.na(prediction)),
            .groups = "drop") |>
  arrange(variable, site)

# depth resolution differs by reservoir - this is what matters for hypo work.
message("depths available per reservoir:")
print(chem_structure |> distinct(site, n_depths, depths) |> arrange(site, n_depths))


#### 6b. site 50 depth check ####
# the bottom of the FLARE profile should land near the deep-hole max depth.
# if a run stopped well short of that it would not be site 50.
depth_check <- chem_data |>
  filter(!is.na(depth)) |>
  group_by(site) |>
  summarise(deepest_layer = max(depth), .groups = "drop") |>
  mutate(max_depth    = site50_max_depth[site],
         is_deep_hole = deepest_layer >= max_depth - 1.5)  # allow for drawdown

message("site 50 depth check:")
print(depth_check)


#### 7. how does that line up with the targets ####
# same URL as "helpful files/exploring_chem.R" lines 6 and 40.
# site_id is the only site key in this file - fcre = Falling Creek site 50,
# bvre = Beaverdam site 50, because Site==50 was filtered upstream (section 0).
targets_url <- "https://amnh1.osn.mghpcc.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz"

targets <- readr::read_csv(targets_url, show_col_types = FALSE) |>
  filter(site_id %in% sites)

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
usable_at <- function(s) chem_summary |> filter(site == s, usable) |> pull(vera_variable) |> unique()
fcre_usable <- usable_at("fcre")
bvre_usable <- usable_at("bvre")

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
#
# use_reforecast only ever does anything at fcre. that means it makes the fcre
# record longer than the bvre one - for a fcre vs bvre comparison either set it
# FALSE or trim both to the common window afterwards.
pull_chem <- function(variable_name, site = "fcre", start_date, end_date, horizon = 0,
                      depths = NULL, n_members = 150, use_reforecast = TRUE){

  grab <- function(bucket, keep_ref_dates = NULL){
    d <- open_dataset(bucket) |>
      mutate(reference_datetime = as_date(reference_datetime)) |>
      filter(variable == variable_name,
             reference_datetime >= start_date,
             reference_datetime <= end_date,
             parameter <= n_members)
    if(!is.null(keep_ref_dates)) d <- d |> filter(reference_datetime %in% keep_ref_dates)
    collect(d)
  }

  forecasts <- grab(flare_bucket[[site]])

  # fcre only: top up with the reforecast, but ONLY for reference dates the
  # operational never had (reforecast_only_dates, section 2). that is what stops
  # the 23 overlapping dates being pulled twice - on any date both runs issued,
  # the operational forecast is the one that is kept.
  if(site == "fcre" && use_reforecast){
    forecasts <- bind_rows(forecasts, grab(reforecast_bucket, reforecast_only_dates))
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
# oxy_9m_fcre <- pull_chem("DO_mgL_mean", "fcre", min(ref_dates$fcre), max(ref_dates$fcre), depths = 9)
# and at Beaverdam - deeper hypolimnion, 13 m not 9 m:
# oxy_13m_bvre <- pull_chem("DO_mgL_mean", "bvre", min(ref_dates$bvre), max(ref_dates$bvre), depths = 13)


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
