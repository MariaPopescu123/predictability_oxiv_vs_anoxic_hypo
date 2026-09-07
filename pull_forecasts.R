#### pull VERA forecasts for the oxic vs anoxic hypolimnion comparison ####
## MP: September 2026
## Builds one tidy table of forecasts from the requested models, at the requested
## variables / sites / depths, out of the VERA forecast bucket.
##
## Companion to testing.R, which is about what FLARE produces. This one is about
## what has actually been SUBMITTED TO VERA, which is a much smaller set - see
## the coverage report in section 5 and the notes on each model below.
##
## Bucket layout (same OSN endpoint as testing.R):
##   .../vera4cast/forecasts/archive-parquet/project_id=vera4cast/duration=P1D/
##       variable=<VERA name>/model_id=<model>/reference_date=<YYYY-MM-DD>/
## archive-parquet is the one to use: bundled-parquet holds the same rows in a
## single 140M-row file per variable/model, which cannot be filtered cheaply.

library(tidyverse)
library(arrow)

osn       <- "amnh1.osn.mghpcc.org"
vera_root <- "bio230121-bucket01/vera4cast/forecasts/archive-parquet/project_id=vera4cast/duration=P1D"
out_dir   <- "./model_output"

open_osn <- function(path) arrow::s3_bucket(path, endpoint_override = osn, anonymous = TRUE)


#### 1. what to pull ####

# label is what I call the model; model_id is the string in the bucket.
# NOTE on the ones whose name did not map straight across:
#   persistence     -> persistenceRW, the random-walk baseline. There is also a
#                      persistence_constantSD; it only starts 2026-07-09, so RW is
#                      the one with a usable record.
#   DOY climatology -> climatology, which IS the day-of-year climatology. The
#                      monthly version is a separate model_id, monthly_mean.
#   chronos         -> chronos2, the Amazon Chronos foundation-model submission.
#   FLARE           -> glm_aed_flare_v3, the same run testing.R reads raw. The
#                      VERA copy is already mapped onto observation depths, so
#                      its shallowest layer is 0.1 m, not the raw 0 m.
models <- tribble(
  ~label,             ~model_id,
  "historical mean",  "historic_mean",
  "persistence",      "persistenceRW",
  "DOY climatology",  "climatology",
  "FLARE",            "glm_aed_flare_v3",
  "ARIMA",            "arima_no_covariate",
  "prophet",          "vera_prophet",
  "chronos",          "chronos2",
  "ETS",              "ETS")

# VERA target names. CO2 here is DISSOLVED CO2 from the GHG samples
# (CO2_umolL_sample), not the eddy-covariance flux CO2flux_umolm2s_mean - the
# flux is a whole-reservoir footprint, not site 50 (see testing.R section 5).
variables <- c("NH4_ugL_sample", "NO3NO2_ugL_sample", "SRP_ugL_sample",
               "CO2_umolL_sample", "CH4_umolL_sample", "DOC_mgL_sample")

# surface + hypolimnion at each reservoir.
# bvre 8.0 is not one of the depths I asked for, but it is the depth the VERA
# baselines actually submit CH4/CO2 at in Beaverdam (they use 0.1 and 8, not 6),
# so it is pulled too and labelled - without it there is no baseline GHG
# forecast at depth in bvre at all.
focal_depths <- tribble(
  ~site_id, ~depth_m, ~layer,
  "fcre",   0.1,      "surface",
  "fcre",   9.0,      "hypolimnion",
  "bvre",   0.1,      "surface",
  "bvre",   6.0,      "hypolimnion",
  "bvre",   8.0,      "hypolimnion (VERA GHG depth)")

# the window to pull. the baselines have very short archives (ETS, chronos2 and
# arima start late July 2026, prophet mid-August), so a like-for-like skill
# comparison across all eight models can only happen where they all overlap.
# widen this to go back to 2024 for FLARE / persistence / climatology alone.
start_date <- as_date("2026-07-27")
end_date   <- as_date("2026-09-02")


#### 2. pull one model x variable ####
# returns member-level rows - the ensemble is needed for CRPS later, so it is
# not collapsed here.
pull_one <- function(model_id, variable, start_date, end_date, depths = focal_depths){

  path <- paste0(vera_root, "/variable=", variable, "/model_id=", model_id)
  listing <- try(open_osn(path)$ls(), silent = TRUE)
  if(inherits(listing, "try-error") || !length(listing)) return(NULL)   # model never submitted this variable

  ref_dates <- ymd(str_remove(listing, "reference_date=")) |> sort()
  ref_dates <- ref_dates[ref_dates >= start_date & ref_dates <= end_date]
  if(!length(ref_dates)) return(NULL)

  map(ref_dates, function(rd){
    ds <- try(open_dataset(open_osn(paste0(path, "/reference_date=", rd))), silent = TRUE)
    if(inherits(ds, "try-error")) return(NULL)
    out <- try(ds |>
      filter(site_id %in% depths$site_id, depth_m %in% depths$depth_m) |>
      select(site_id, reference_datetime, datetime, depth_m, family, parameter, prediction) |>
      collect(), silent = TRUE)
    if(inherits(out, "try-error") || !nrow(out)) return(NULL)
    # round depth_m the moment it lands in R. Some models store depth_m as
    # float32, so 0.1 comes back as 0.100000001490116 once arrow widens it to a
    # double. The arrow filter above is unaffected (arrow casts the literal down
    # to the column type), but the inner_join on depth_m below would silently
    # drop those rows. glm_aed_v1 is the one that does this today.
    out |> mutate(depth_m = round(depth_m, 2), reference_date = rd)
  }) |>
    list_rbind() |>
    (\(d) if(is.null(d) || !nrow(d)) NULL else
      d |> mutate(model_id = model_id, variable = variable))()
}


#### 3. pull everything ####
grid <- expand_grid(model_id = models$model_id, variable = variables)

message("pulling ", nrow(grid), " model x variable combinations, ",
        start_date, " to ", end_date, " ...")

forecasts <- pmap(grid, \(model_id, variable){
  d <- pull_one(model_id, variable, start_date, end_date)
  message("  ", str_pad(model_id, 22), str_pad(variable, 20),
          if(is.null(d)) "-" else paste(format(nrow(d), big.mark = ","), "rows"))
  d
}) |>
  list_rbind()

# tidy up: real dates, horizon, the site/depth labels, and my model names
forecasts <- forecasts |>
  mutate(reference_datetime = as_date(reference_datetime),
         datetime           = as_date(datetime),
         horizon_days       = as.numeric(datetime - reference_datetime)) |>
  # inner_join, not left_join: the arrow filter above can only test the union of
  # the depths (0.1, 6, 8, 9), so it also returns fcre at 6 m and bvre at 9 m.
  # joining on the site/depth PAIR is what drops those.
  inner_join(focal_depths, by = c("site_id", "depth_m")) |>
  left_join(models,        by = "model_id") |>
  select(label, model_id, variable, site_id, depth_m, layer,
         reference_date, reference_datetime, datetime, horizon_days,
         family, parameter, prediction)

message("\ntotal: ", format(nrow(forecasts), big.mark = ","), " member-level rows")


#### 4. ensemble summary ####
# one row per model x variable x site x depth x reference date x horizon.
forecast_summary <- forecasts |>
  group_by(label, model_id, variable, site_id, depth_m, layer,
           reference_date, datetime, horizon_days) |>
  summarise(n_members = n(),
            mean_pred = mean(prediction, na.rm = TRUE),
            sd_pred   = sd(prediction, na.rm = TRUE),
            q10       = quantile(prediction, 0.10, na.rm = TRUE),
            q90       = quantile(prediction, 0.90, na.rm = TRUE),
            .groups   = "drop")


#### 5. coverage: what actually came back ####
# the point of this table is the gaps, not the counts. a missing row means that
# model never submitted that variable at that site and depth in this window.
coverage <- forecasts |>
  group_by(label, variable, site_id, depth_m, layer) |>
  summarise(n_ref_dates  = n_distinct(reference_date),
            first_ref    = min(reference_date),
            last_ref     = max(reference_date),
            max_horizon  = max(horizon_days),
            n_members    = n_distinct(parameter),
            .groups = "drop")

message("\n=== coverage: model x variable, at the requested depths ===")
print(coverage |>
        mutate(cell = paste0(site_id, " ", depth_m, "m")) |>
        select(label, variable, cell, n_ref_dates) |>
        pivot_wider(names_from = cell, values_from = n_ref_dates) |>
        arrange(variable, label), n = Inf, width = Inf)

# spelled out: which of the requested model x variable x depth cells are empty
requested <- expand_grid(models |> select(label), variable = variables,
                         focal_depths |> filter(layer != "hypolimnion (VERA GHG depth)"))
missing <- anti_join(requested, coverage, by = c("label","variable","site_id","depth_m"))

message("\n=== requested but NOT available (", nrow(missing), " of ", nrow(requested), " cells) ===")
print(missing |> mutate(cell = paste0(site_id, " ", depth_m, "m")) |>
        count(variable, label, wt = NULL) |> arrange(variable, label), n = Inf)


#### 6. save ####
dir.create(out_dir, showWarnings = FALSE)
write_parquet(forecasts,   file.path(out_dir, "vera_forecasts_members.parquet"))
write_csv(forecast_summary, file.path(out_dir, "vera_forecasts_summary.csv"))
write_csv(coverage,         file.path(out_dir, "vera_forecast_coverage.csv"))
