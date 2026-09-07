

all_results <- arrow::open_dataset("s3://anonymous@bio230121-bucket01/vera4cast/forecasts/bundled-summaries?endpoint_override=amnh1.osn.mghpcc.org")
df <- all_results |>
  dplyr::filter(variable %in% c("DO_mgL_mean")) |>
  dplyr::collect()

# list the variable partitions instead of naming six
variables <- ls_partition(vera_root, "variable=")            # all 51

# for each variable x model, open one reference_date and read the depths present
open_dataset(open_osn(paste0(mod_path, "/reference_date=", rd))) |>
  distinct(site_id, depth_m) |>
  collect()