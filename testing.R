#### what chem variables can I actually get out of FLARE? ####
## MP: August 2026
## pulls the variable list out of the FLARE glm_aed output for every site that
## has one (fcre and bvre) and summarizes what is available for the chem variables
## AED name -> VERA name/unit conversions follow Austin's combined_run_aed.R
## (see "helpful files/forecasting_conversion_notes.R")

## note: don't aggregate over the whole dataset, it is enormous and all over the
## network. the buckets are partitioned by reference_date, so date coverage
## comes from ls() and the variable/depth/member structure from one partition

library(tidyverse)
library(arrow)

out_dir <- "./model_output"
osn     <- "amnh1.osn.mghpcc.org"

#only fcre and bvre run glm_aed - everything else in the flare bucket (ccre,
#sunp, TOOK, SUGG, etc.) is glm_flare_v1/v3, physics only, no chemistry
sites <- c("fcre", "bvre")

flare_path <- function(site) paste0("bio230121-bucket01/flare/forecasts/parquet/site_id=",
                                    site, "/model_id=glm_aed_flare_v3")

#reforecast run only exists for fcre
reforecast_path <- paste0("bio230121-bucket01/fcre-reforecast/forecasts/parquet/site_id=fcre",
                          "/model_id=glm_aed_flare_v3_reforecast")

bkt <- function(path) arrow::s3_bucket(path, endpoint_override = osn, anonymous = TRUE)


#### 1. what reference dates exist ####
ref_dates <- function(path){
  bkt(path)$ls() |> str_remove("reference_date=") |> ymd() |> sort()
}

site_dates       <- map(set_names(sites), \(s) ref_dates(flare_path(s)))
reforecast_dates <- ref_dates(reforecast_path)

coverage <- imap_dfr(site_dates, \(d, s)
                     tibble(site = s, run = "glm_aed_flare_v3 (operational)",
                            n_dates = length(d), first_ref = min(d), last_ref = max(d),
                            missing_days = length(setdiff(seq(min(d), max(d), by = "day"), d)))) |>
  bind_rows(tibble(site = "fcre", run = "glm_aed_flare_v3_reforecast",
                   n_dates = length(reforecast_dates),
                   first_ref = min(reforecast_dates), last_ref = max(reforecast_dates),
                   missing_days = length(setdiff(seq(min(reforecast_dates),
                                                     max(reforecast_dates), by = "day"),
                                                 reforecast_dates))))

message("reference date coverage:")
print(coverage)


#### 2. what variables come out at each site ####
#one partition is enough, the variable set doesn't change day to day
latest_ds <- function(site){
  d <- max(site_dates[[site]])
  bkt(file.path(flare_path(site), paste0("reference_date=", d))) |> arrow::open_dataset()
}

site_vars <- map(set_names(sites), \(s) latest_ds(s) |>
                   distinct(variable) |> collect() |> pull(variable) |> sort())

iwalk(site_vars, \(v, s) {message(paste0(s, ": ", length(v), " variables")); print(v)})

message("both sites:")
print(reduce(site_vars, intersect))
message("fcre only:")
print(setdiff(site_vars$fcre, site_vars$bvre))
message("bvre only:")
print(setdiff(site_vars$bvre, site_vars$fcre))


#### 3. which of those are chem ####
#AED module prefixes: OXY, NIT (N), PHS (P), CAR (C/DIC/CH4), OGM (organic
#matter), SIL (Si), PHY (phyto groups), ZOO (zooplankton), plus FLARE's
#already-named chem outputs
chem_pattern <- paste0("^(OXY|NIT|PHS|CAR|OGM|SIL|PHY|ZOO|TRC|NCS)_",
                       "|^(oxy|chla|fdom|secchi|extc)|_flux_|^DO_|^Chla|^fDOM|Rdom")

is_chem   <- \(v) str_detect(v, regex(chem_pattern, ignore_case = TRUE))
chem_vars <- map(site_vars, \(v) v[is_chem(v)])

iwalk(chem_vars, \(v, s) {message(paste0(s, ": ", length(v), " chem variables")); print(v)})
message("non-chem (physics / state / calibrated parameters), fcre:")
print(setdiff(site_vars$fcre, chem_vars$fcre))


#### 4. AED -> VERA lookup ####
#from Austin's conversion script, so I know what each raw variable is once it is
#on the VERA scale and can line it up with the targets
#NOTE: Austin's script renames NIT_amm twice (NH4, then NO3NO2) so NIT_nit never
#gets renamed - that looks like a bug, corrected here
chem_lookup <- tribble(
  ~flare_variable,  ~vera_variable,          ~conversion,
  "oxy_mean",       "DO_mgL_mean",           "/1000*32, forced to depth 1.6, datetime - 1 day",
  "DO_mgL_mean",    "DO_mgL_mean",           "already converted by FLARE",
  "NIT_amm",        "NH4_ugL_sample",        "/1000/0.001/(1/18.04)",
  "NIT_nit",        "NO3NO2_ugL_sample",     "/1000/0.001/(1/62.00)",
  "PHS_frp",        "SRP_ugL_sample",        "/1000/0.001/(1/94.9714)",
  "CAR_dic",        "DIC_mgL_sample",        "/1000/(1/52.515)",
  "CAR_ch4",        "CH4_umolL_sample",      "none",
  "fDOM_QSU_mean",  "fDOM_QSU_mean",         "(151.3407 + prediction)/29.62654",
  "Chla_ugL_mean",  "Chla_ugL_mean",         "none (>20 ugL at 1.6m = Bloom_binary_mean)",
  "secchi",         "Secchi_m_sample",       "none (derived from extc)",
  "co2_flux_mean",  "CO2flux_umolm2s_mean",  "/0.001/86400",
  "ch4_flux_mean",  "CH4flux_umolm2s_mean",  "/0.001/86400") |>
  mutate(in_fcre = flare_variable %in% site_vars$fcre,
         in_bvre = flare_variable %in% site_vars$bvre)

print(chem_lookup, n = Inf)

#chem output with no VERA target to score against (model state only)
iwalk(chem_vars, \(v, s) {message(paste0("no VERA target, ", s, ":"))
  print(setdiff(v, chem_lookup$flare_variable))})


#### 5. structure of each chem variable ####
#depths, ensemble members, horizon - from the most recent forecast at each site
chem_structure <- map_dfr(sites, \(s){
  latest_ds(s) |>
    filter(variable %in% chem_vars[[s]]) |>
    select(datetime, depth, parameter, variable, prediction, variable_type) |>
    collect() |>
    group_by(variable) |>
    summarise(site          = s,
              variable_type = paste(unique(variable_type), collapse = ", "),
              n_depths      = n_distinct(depth[!is.na(depth)]),
              depth_range   = if(all(is.na(depth))) "no depth (whole-lake / surface)" else
                                paste0(min(depth, na.rm = TRUE), " - ", max(depth, na.rm = TRUE)),
              n_members     = n_distinct(parameter),
              horizon_days  = as.numeric(difftime(max(datetime), min(datetime), units = "days")),
              mean_pred     = mean(prediction, na.rm = TRUE),
              prop_na       = mean(is.na(prediction)),
              .groups = "drop") |>
    relocate(site)
}) |>
  arrange(variable, site)

print(chem_structure, n = Inf)

#the depth resolution differs by site - this is what matters for hypo work
message("depths available per site:")
print(chem_structure |> distinct(site, n_depths, depth_range) |> arrange(site, n_depths))


#### 6. how does that line up with the targets ####
targets_url <- "https://amnh1.osn.mghpcc.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz"

targets <- readr::read_csv(targets_url, show_col_types = FALSE) |>
  filter(site_id %in% sites)

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

#one table per site: forecast from FLARE + observed in targets = something I can score
chem_summary <- expand_grid(site = sites, chem_lookup) |>
  mutate(in_flare = if_else(site == "fcre", in_fcre, in_bvre)) |>
  select(-in_fcre, -in_bvre) |>
  left_join(chem_structure, by = c("site", "flare_variable" = "variable")) |>
  left_join(target_avail,   by = c("site", "vera_variable"  = "variable")) |>
  mutate(has_targets = !is.na(n_obs),
         usable      = in_flare & has_targets) |>
  arrange(site, desc(usable), vera_variable)

print(chem_summary |> select(site, flare_variable, vera_variable, in_flare,
                             n_depths, n_obs, first_obs, last_obs, usable), n = Inf)

walk(sites, \(s) message(paste0("usable chem pairs at ", s, ": ",
                                paste(unique(chem_summary$vera_variable[chem_summary$site == s &
                                                                          chem_summary$usable]),
                                      collapse = ", "))))


#### 7. save so I don't have to re-pull ####
dir.create(out_dir, showWarnings = FALSE)
write_csv(coverage,       file.path(out_dir, "flare_date_coverage.csv"))
write_csv(chem_structure, file.path(out_dir, "flare_chem_structure.csv"))
write_csv(chem_summary,   file.path(out_dir, "flare_chem_vs_targets.csv"))


#### 8. helper for when I want the actual time series ####
#pulls one chem variable at one site across a range of reference dates
#horizon = 0 gives the FLARE "nowcast" (reference_datetime == datetime), which is
#what I used for temp/secchi in the DCM work - good historic covariate
#use_reforecast only does anything for fcre
pull_chem <- function(variable_name, site = "fcre", start_date, end_date, horizon = 0,
                      depths = NULL, n_members = 150, use_reforecast = TRUE){

  paths <- flare_path(site)
  if(site == "fcre" & use_reforecast) paths <- c(paths, reforecast_path)

  ds <- arrow::open_dataset(map(paths, \(p) arrow::open_dataset(bkt(p))))

  out <- ds |>
    mutate(reference_datetime = as_date(reference_datetime)) |>
    filter(variable == variable_name,
           reference_datetime >= start_date,
           reference_datetime <= end_date,
           parameter <= n_members) |>
    collect() |>
    mutate(datetime = as_date(datetime),
           horizon_days = as.numeric(datetime - reference_datetime)) |>
    filter(horizon_days == horizon)

  if(!is.null(depths)) out <- out |> filter(depth %in% depths)

  out |>
    group_by(datetime, depth) |>
    summarise(mean_pred = mean(prediction, na.rm = TRUE),
              sd_pred   = sd(prediction, na.rm = TRUE),
              .groups   = "drop") |>
    mutate(site = site, variable = variable_name)
}

#e.g. hypolimnetic oxygen over the operational record:
# oxy_9m <- pull_chem("DO_mgL_mean", "fcre", min(site_dates$fcre), max(site_dates$fcre), depths = 9)


#### 9. quick look ####
map_dfr(sites, \(s) latest_ds(s) |>
          filter(variable %in% chem_lookup$flare_variable) |>
          select(datetime, depth, variable, prediction) |>
          collect() |>
          mutate(site = s)) |>
  group_by(site, variable, datetime, depth) |>
  summarise(prediction = mean(prediction, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = datetime, y = prediction, group = interaction(depth, site), color = depth)) +
  geom_line() +
  facet_grid(variable ~ site, scales = "free_y") +
  theme_bw() +
  labs(title = "most recent FLARE chem forecast, fcre vs bvre", x = NULL, y = NULL)
