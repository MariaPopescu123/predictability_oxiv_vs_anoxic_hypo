#### most recent FLARE chem forecast: NH4, NO3NO2, SRP, DIC at all depths ####
## MP: August 2026
## fcre = Falling Creek Reservoir, bvre = Beaverdam Reservoir, both at site 50.
## bucket paths / endpoint / conversions all follow testing.R - see that script
## for why this is site 50 and where the conversions come from
## ("helpful files/forecasting_conversion_notes.R" lines 146-153).

library(tidyverse)
library(arrow)

osn <- "amnh1.osn.mghpcc.org"

fcre_path <- "bio230121-bucket01/flare/forecasts/parquet/site_id=fcre/model_id=glm_aed_flare_v3"
bvre_path <- "bio230121-bucket01/flare/forecasts/parquet/site_id=bvre/model_id=glm_aed_flare_v3"

# the four discrete chemistry variables, in AED names.
# AED name -> VERA name, from "helpful files/forecasting_conversion_notes.R":
#   NIT_amm -> NH4_ugL_sample     line 146
#   NIT_nit -> NO3NO2_ugL_sample  line 148
#   PHS_frp -> SRP_ugL_sample     line 150
#   CAR_dic -> DIC_mgL_sample     line 152
chem_vars <- c("NIT_amm", "NIT_nit", "PHS_frp", "CAR_dic")


#### 1. find the most recent forecast at each reservoir ####
fcre_bucket <- arrow::s3_bucket(fcre_path, endpoint_override = osn, anonymous = TRUE)
bvre_bucket <- arrow::s3_bucket(bvre_path, endpoint_override = osn, anonymous = TRUE)

fcre_latest <- fcre_bucket$ls() |> str_remove("reference_date=") |> ymd() |> max()
bvre_latest <- bvre_bucket$ls() |> str_remove("reference_date=") |> ymd() |> max()

message("most recent fcre (Falling Creek) forecast: ", fcre_latest)
message("most recent bvre (Beaverdam) forecast: ",     bvre_latest)


#### 2. pull just that one reference_date at each reservoir ####
# one partition each, only the four variables, only the columns needed
fcre_forecast <- arrow::s3_bucket(paste0(fcre_path, "/reference_date=", fcre_latest),
                                  endpoint_override = osn, anonymous = TRUE) |>
  arrow::open_dataset() |>
  filter(variable %in% chem_vars) |>
  select(datetime, depth, parameter, variable, prediction) |>
  collect() |>
  mutate(site = "fcre (Falling Creek)", reference_date = fcre_latest)

bvre_forecast <- arrow::s3_bucket(paste0(bvre_path, "/reference_date=", bvre_latest),
                                  endpoint_override = osn, anonymous = TRUE) |>
  arrow::open_dataset() |>
  filter(variable %in% chem_vars) |>
  select(datetime, depth, parameter, variable, prediction) |>
  collect() |>
  mutate(site = "bvre (Beaverdam)", reference_date = bvre_latest)

forecast <- bind_rows(fcre_forecast, bvre_forecast)


#### 3. convert AED units -> VERA units ####
# so the axes are in the units the targets are in, not raw AED mmol/m3
forecast <- forecast |>
  mutate(observation_scale = case_when(
           variable == "NIT_amm" ~ prediction/1000/0.001/(1/18.04),      # -> NH4 ugL
           variable == "NIT_nit" ~ prediction/1000/0.001/(1/62.00),      # -> NO3NO2 ugL
           variable == "PHS_frp" ~ prediction/1000/0.001/(1/94.9714),    # -> SRP ugL
           variable == "CAR_dic" ~ prediction/1000/(1/52.515)),          # -> DIC mgL
         # nicer facet labels, ordered N -> P -> C
         panel = factor(variable,
                        levels = chem_vars,
                        labels = c("NH4 (ug/L)", "NO3NO2 (ug/L)", "SRP (ug/L)", "DIC (mg/L)")),
         # fcre on the left, bvre on the right
         site  = factor(site, levels = c("fcre (Falling Creek)", "bvre (Beaverdam)")),
         datetime = as_date(datetime))


#### 4. collapse the 221-member ensemble to a mean per depth per day ####
forecast_mean <- forecast |>
  group_by(site, panel, datetime, depth) |>
  summarise(prediction = mean(observation_scale, na.rm = TRUE), .groups = "drop")

message("depths plotted - fcre: ",
        n_distinct(forecast_mean$depth[forecast_mean$site == "fcre (Falling Creek)"]),
        ", bvre: ",
        n_distinct(forecast_mean$depth[forecast_mean$site == "bvre (Beaverdam)"]))


#### 5. plot ####
# rows = variable (free y, so each analyte gets its own scale but the two
# reservoirs share it and stay directly comparable), columns = reservoir,
# one line per depth coloured surface (light) -> bottom (dark)
chem_plot <- ggplot(forecast_mean,
                    aes(x = datetime, y = prediction, group = depth, color = depth)) +
  geom_line(linewidth = 0.4) +
  facet_grid(panel ~ site, scales = "free_y", switch = "y") +
  scale_color_viridis_c(option = "mako", direction = -1, name = "depth (m)") +
  scale_x_date(date_labels = "%b %d") +
  theme_bw() +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        strip.text.y.left = element_text(angle = 90),
        axis.title.y = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Most recent FLARE glm_aed forecast, site 50",
       subtitle = paste0("ensemble mean by depth  |  fcre issued ", fcre_latest,
                         ", bvre issued ", bvre_latest),
       x = NULL)

print(chem_plot)

ggsave("./model_output/flare_chem_forecast_recent.png", chem_plot,
       width = 9, height = 9, dpi = 200, bg = "white")


#### 6. same thing with independent y axes ####
# the shared scale above is honest about magnitude (FCR hypo NH4 runs ~8x
# Beaverdam's, Beaverdam SRP ~20x FCR's) but it flattens the smaller panel to a
# line. facet_wrap frees every panel separately so the shape is readable too.
chem_plot_free <- ggplot(forecast_mean,
                         aes(x = datetime, y = prediction, group = depth, color = depth)) +
  geom_line(linewidth = 0.4) +
  facet_wrap(~ panel + site, scales = "free_y", ncol = 2) +
  scale_color_viridis_c(option = "mako", direction = -1, name = "depth (m)") +
  scale_x_date(date_labels = "%b %d") +
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Most recent FLARE glm_aed forecast, site 50 - independent y axes",
       subtitle = paste0("ensemble mean by depth  |  fcre issued ", fcre_latest,
                         ", bvre issued ", bvre_latest),
       x = NULL, y = NULL)

print(chem_plot_free)

ggsave("./model_output/flare_chem_forecast_recent_freey.png", chem_plot_free,
       width = 9, height = 10, dpi = 200, bg = "white")
