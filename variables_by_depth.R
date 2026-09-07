#### which VERA variables are forecast at a given depth? ####
## MP: September 2026
## Companion to pull_forecasts.R. That script pulls a chosen list of variables;
## this one answers the prior question - which variables is ANYONE forecasting at
## a given depth, and which models.
##
## Why this needs a sweep rather than a filter: the bucket is partitioned by
## variable / model_id / reference_date only. depth_m is a COLUMN inside the
## parquet files, so the only way to know which depths a model submits is to
## open a partition and look. There is no listing that answers it.
##
## Cost: ~680 variable x model pairs across the whole bucket, ~0.5 s per
## partition read, so ~6 min at n_dates = 1 and ~17 min at n_dates = 3.

library(tidyverse)
library(arrow)

osn       <- "amnh1.osn.mghpcc.org"
vera_root <- "bio230121-bucket01/vera4cast/forecasts/archive-parquet/project_id=vera4cast/duration=P1D"
out_dir   <- "./model_output"

open_osn <- function(path) arrow::s3_bucket(path, endpoint_override = osn, anonymous = TRUE)

ls_partition <- function(path, prefix){
  r <- try(open_osn(path)$ls(), silent = TRUE)
  if(inherits(r, "try-error") || !length(r)) character(0) else sort(str_remove(r, prefix))
}


#### the sweep ####
# max_dates: how many reference dates to try per variable x model, spread evenly
# across that model's archive. Reading ONE date is not enough, for two separate
# reasons:
#   1. these baselines only forecast where they have a recent observation, so a
#      model that does submit at 0.1 m can show nothing at 0.1 m on a given date
#   2. a model can be submitting at one reservoir and not the other on any given
#      date - glm_aed_flare_v3 at bvre is intermittent, and a fixed 3-date sample
#      missed it completely
# So this reads sampled dates one at a time and stops once every requested site
# has been seen AND at least min_dates dates have been read. Single-site models
# (glm_aed_flare_v3 CH4 is fcre-only) pay the full max_dates.
#
# THE RESULT IS ALWAYS A LOWER BOUND. It is a union over the dates actually read,
# and two runs with different sampling found different things: a 3-date run missed
# glm_aed_flare_v3 at bvre entirely, and a stop-on-first-hit run missed persistenceRW
# submitting SRP at bvre 0.1 m. Raise min_dates/max_dates to tighten it; the only
# exhaustive answer reads every reference date, which is hours.
depths_by_variable <- function(sites = c("fcre", "bvre"), min_dates = 5, max_dates = 12, variables = NULL){

  if(is.null(variables)) variables <- ls_partition(vera_root, "variable=")
  message("sweeping ", length(variables), " variables ...")

  map(variables, function(v){
    var_path <- paste0(vera_root, "/variable=", v)
    models   <- ls_partition(var_path, "model_id=")

    res <- map(models, function(m){
      mod_path  <- paste0(var_path, "/model_id=", m)
      ref_dates <- ymd(ls_partition(mod_path, "reference_date="))
      if(!length(ref_dates)) return(NULL)

      samp <- ref_dates[round(seq(1, length(ref_dates), length.out = min(max_dates, length(ref_dates))))]
      # newest first - most likely to have every site. as.character() matters:
      # `for (x in <Date vector>)` iterates the underlying numeric and drops the
      # class, which would build "reference_date=20301" instead of a real date.
      samp <- rev(as.character(samp))
      found <- list()

      for(rd in samp){
        ds <- try(open_dataset(open_osn(paste0(mod_path, "/reference_date=", rd))), silent = TRUE)
        if(inherits(ds, "try-error")) next
        r <- try(ds |> distinct(site_id, depth_m) |> collect(), silent = TRUE)
        if(inherits(r, "try-error") || !nrow(r)) next
        # round on arrival - some models store depth_m as float32, which widens
        # to 0.100000001490116 in R and then fails every equality test
        found[[length(found) + 1]] <- r |> mutate(depth_m = round(depth_m, 2))
        # stop once BOTH conditions hold: every requested site has been seen, and
        # at least min_dates have been read. Stopping on the site test alone is
        # not enough - a model present at both sites on the newest date would be
        # read exactly once, and a depth it only submits occasionally (persistenceRW
        # SRP at bvre 0.1 m) would never show up.
        seen <- unique(list_rbind(found)$site_id)
        if(all(sites %in% seen) && length(found) >= min_dates) break
      }

      if(!length(found)) return(NULL)
      list_rbind(found) |> mutate(model_id = m)
    }) |> list_rbind()

    if(is.null(res) || !nrow(res)) return(NULL)
    message("  ", str_pad(v, 38), length(models), " models")
    res |> filter(site_id %in% sites) |> distinct(site_id, depth_m, model_id) |> mutate(variable = v)
  }) |> list_rbind()
}

depth_map <- depths_by_variable(min_dates = 5, max_dates = 12)

dir.create(out_dir, showWarnings = FALSE)
write_csv(depth_map, file.path(out_dir, "vera_depth_map.csv"))


#### the answer: variables forecast at 0.1 m ####
# depth_m is already rounded to 2 dp, and whole-lake variables carry depth_m = NA
at_0.1 <- depth_map |>
  filter(!is.na(depth_m), depth_m == 0.1) |>
  group_by(variable, site_id) |>
  summarise(n_models = n_distinct(model_id),
            models   = paste(sort(unique(model_id)), collapse = ", "),
            .groups  = "drop")

message("\n=== variables with a 0.1 m forecast ===")
print(at_0.1 |> select(variable, site_id, n_models) |>
        pivot_wider(names_from = site_id, values_from = n_models) |>
        arrange(variable), n = Inf)

message("\n=== and the models behind them ===")
for(i in seq_len(nrow(at_0.1)))
  cat(sprintf("%-38s %-5s %s\n", at_0.1$variable[i], at_0.1$site_id[i], at_0.1$models[i]))

write_csv(at_0.1, file.path(out_dir, "vera_variables_at_0.1m.csv"))
