# build_new_location.R ------------------------------------------------------
#
# Example: build game inputs for a new place from Natural Earth admin-1 units.
# Here: mainland Portugal (18 districts). Change `country` to try elsewhere.
#
# Run from the project root:
#   source("examples/build_new_location.R")
#
# Outputs (never written to data/):
#   locations/<country>/*.rds        same files load_inputs() reads
#   locations/<country>/kernel_row.png   map of one unit's kernel row
#
# Population: Natural Earth admin-1 polygons have NO population column.
# Supply your own table as locations/<country>/population.csv with columns
#   name, pop
# where `name` matches the Natural Earth `name` column. Good sources:
#   - WorldPop (https://www.worldpop.org): gridded population; sum the raster
#     within each polygon, e.g. exactextractr::exact_extract(r, units, "sum")
#   - GHS-POP (https://ghsl.jrc.ec.europa.eu): same idea, global 100 m grids
#   - National statistics office census tables by region
# If no CSV is found, this script falls back to a PLACEHOLDER: the national
# total (Natural Earth admin-0 POP_EST) shared out by land area. That is only
# good enough to check the plumbing, not for teaching.

# ---- packages ----
library(sf)
library(rnaturalearth)   # ne_states() also needs rnaturalearthhires
library(ggplot2)

source("R/build_location_inputs.R")

# ---- settings ----
country <- "Portugal"
gravity <- list(alpha = 1, beta = 2)
focus_unit <- "Lisboa"   # unit whose kernel row we map

out_dir <- file.path("locations", tolower(country))
pop_csv <- file.path(out_dir, "population.csv")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- admin-1 polygons ----
units_raw <- rnaturalearth::ne_states(country = country, returnclass = "sf")

# Drop far-off island groups so distances stay sensible for a mainland game
units <- units_raw[!units_raw$name %in% c("Azores", "Madeira"), ]
message("Units: ", nrow(units))

# ---- population ----
# Preference order: user CSV > WorldPop raster summed per unit > area placeholder
raster_path <- file.path("data_raw", "global", "worldpop", "prt_ppp_2020_1km_Aggregated_UNadj.tif")

if (file.exists(pop_csv)) {
  pop_tbl <- read.csv(pop_csv, stringsAsFactors = FALSE)
  message("Using population from ", pop_csv)
} else if (file.exists(raster_path)) {
  # WorldPop 2020, 1 km, UN-adjusted: people per cell, summed within each district
  unit_pop <- raster_population(units, raster_path)
  pop_tbl <- data.frame(name = units$name, pop = round(unit_pop))
  write.csv(pop_tbl, pop_csv, row.names = FALSE)
  message("Using WorldPop raster; wrote ", pop_csv)
} else {
  warning("No ", pop_csv, " or raster found; using an area-based PLACEHOLDER population")

  # National total from Natural Earth admin-0
  country_sf <- rnaturalearth::ne_countries(country = country, returnclass = "sf")
  national_pop <- country_sf$pop_est[1]

  # Share it out by land area (uniform density)
  unit_area <- as.numeric(sf::st_area(units))
  area_share <- unit_area / sum(unit_area)
  pop_tbl <- data.frame(
    name = units$name,
    pop = round(national_pop * area_share)
  )
}

# ---- sense-check figure: the admin units and their population ----
admin_df <- merge(units[, "name"], pop_tbl, by = "name")
admin_df$label <- paste0(admin_df$name, "
", round(admin_df$pop / 1e3), "k")

admin_map <- ggplot(admin_df) +
  geom_sf(aes(fill = pop / 1e6), colour = "white", linewidth = 0.3) +
  geom_sf_text(aes(label = label), size = 2.4) +
  scale_fill_distiller(name = "Population
(millions)", palette = "Blues", direction = 1) +
  labs(title = paste0(country, ": units in play (", nrow(admin_df), ")"),
       subtitle = "Population = WorldPop 2020 1 km raster summed within each unit",
       x = NULL, y = NULL) +
  theme_minimal()

ggsave(file.path(out_dir, "admin_units.png"), admin_map, width = 5, height = 7, dpi = 120)

# ---- build the bundle ----
inputs <- build_location_inputs(
  units_sf = units,
  pop = pop_tbl,
  name_col = "name",
  gravity = gravity,
  code_col = "iso_3166_2"
)

save_location_inputs(inputs, out_dir)
message("Saved inputs to ", out_dir)

# ---- quick numeric check ----
# Share of infection pressure each unit keeps for itself (diagonal of K)
self_share <- diag(inputs$K)
print(round(sort(self_share, decreasing = TRUE), 2))

# ---- sense-check figure: one unit's kernel row ----
# Row j of K = where unit j's onward infections land (engine: crossprod(K, I) * Rt).
# Expect: high on the unit itself, high on big/near neighbours, fading with distance.
focus_idx <- match(focus_unit, inputs$pop_tbl$borough_name)

map_df <- inputs$boros
map_df$weight <- inputs$K[focus_idx, ]
map_df$is_focus <- seq_len(nrow(map_df)) == focus_idx

kernel_map <- ggplot(map_df) +
  geom_sf(aes(fill = weight), colour = "white", linewidth = 0.2) +
  geom_sf(data = map_df[map_df$is_focus, ], fill = NA, colour = "black", linewidth = 0.8) +
  scale_fill_viridis_c(name = "K weight", trans = "sqrt") +
  labs(
    title = paste0("Gravity kernel row for ", focus_unit, " (outlined)"),
    subtitle = paste0("Where ", focus_unit, "'s onward infections land: K[j, i] ~ pop_i^",
                      gravity$alpha, " / d^", gravity$beta, "; row sums to 1")
  ) +
  theme_minimal()

fig_path <- file.path(out_dir, "kernel_row.png")
ggsave(fig_path, kernel_map, width = 5, height = 7, dpi = 120)
message("Saved figure to ", fig_path)

# ---- use it in the game ----
# load_inputs() still insists on 33 boroughs (London), so for now load the
# files directly or relax that check when the game is made location-aware:
#   inputs <- readRDS(...)  or  build_location_inputs(...) as above
