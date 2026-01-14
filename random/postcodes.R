# Basic visualisation of postcode areas & districts over London
# Files assumed in: data/figshare_postcode/

# Install (if needed):
pkgs <- c("sf", "dplyr", "ggplot2", "leaflet")
to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(to_install)) install.packages(to_install, dependencies = TRUE)

library(sf)
library(dplyr)
library(ggplot2)
library(leaflet)

# --- Paths and layers ---
gpkg_path <- "data/figshare_postcode/postcode_polygons.gpkg"
layer_area <- "postcode_area"       # e.g., SW, E, N
layer_district <- "postcode_district" # e.g., SW1, E2, N7

# --- Read polygons and put everything in WGS84 (EPSG:4326) ---
areas <- st_read(gpkg_path, layer = layer_area, quiet = TRUE)
dists <- st_read(gpkg_path, layer = layer_district, quiet = TRUE)

# In case they’re not already 4326, transform:
if (is.na(st_crs(areas))) stop("postcode_area layer has no CRS; set st_crs() before proceeding.")
if (is.na(st_crs(dists))) stop("postcode_district layer has no CRS; set st_crs() before proceeding.")

areas <- st_transform(areas, 4326)
dists <- st_transform(dists, 4326)

# Optional: fix invalid geometries if any
suppressWarnings({
  areas <- st_make_valid(areas)
  dists <- st_make_valid(dists)
})

# --- Load London boroughs and create London boundary ---
# Use the same boroughs file as the main app (33 London boroughs)
boroughs <- readRDS("data/boroughs.rds")
if (!inherits(boroughs, "sf")) {
  stop("boroughs.rds must contain an sf object")
}

# Ensure boroughs are in WGS84
if (st_crs(boroughs) != st_crs(4326)) {
  boroughs <- st_transform(boroughs, 4326)
}

# Create a union of all boroughs to get the London boundary polygon
london_boundary <- st_union(boroughs)
london_boundary <- st_make_valid(london_boundary)

# --- Filter postcodes to only those intersecting London boroughs ---
# Use st_filter to keep only postcodes that intersect with London
areas_lnd <- st_filter(areas, london_boundary)
dists_lnd <- st_filter(dists, london_boundary)

# Ensure they're valid geometries
suppressWarnings({
  areas_lnd <- st_make_valid(areas_lnd)
  dists_lnd <- st_make_valid(dists_lnd)
})

# Get bounding boxes from filtered postcode data (with padding to avoid clipping)
# Calculate bbox from the actual postcode data, not boroughs, to capture full extent
areas_bbox <- st_bbox(areas_lnd)
dists_bbox <- st_bbox(dists_lnd)

# Add padding (2% of range) to ensure full geometries are visible
pad_area_x <- (areas_bbox[["xmax"]] - areas_bbox[["xmin"]]) * 0.02
pad_area_y <- (areas_bbox[["ymax"]] - areas_bbox[["ymin"]]) * 0.02
pad_dist_x <- (dists_bbox[["xmax"]] - dists_bbox[["xmin"]]) * 0.02
pad_dist_y <- (dists_bbox[["ymax"]] - dists_bbox[["ymin"]]) * 0.02

# Get borough bbox for leaflet map
lon_bbox <- st_bbox(boroughs)

# --- Static maps with ggplot2 ---
# Map 1: Postcode Areas
p_areas <- ggplot() +
  geom_sf(data = areas_lnd, fill = "steelblue", color = "white", linewidth = 0.5) +
  coord_sf(xlim = c(areas_bbox[["xmin"]] - pad_area_x, areas_bbox[["xmax"]] + pad_area_x),
           ylim = c(areas_bbox[["ymin"]] - pad_area_y, areas_bbox[["ymax"]] + pad_area_y),
           expand = TRUE) +
  labs(title = "London Postcode Areas") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major = element_line(linewidth = 0.2, colour = "grey90"))
print(p_areas)

# Map 2: Postcode Districts
p_districts <- ggplot() +
  geom_sf(data = dists_lnd, fill = "lightcoral", color = "white", linewidth = 0.3) +
  coord_sf(xlim = c(dists_bbox[["xmin"]] - pad_dist_x, dists_bbox[["xmax"]] + pad_dist_x),
           ylim = c(dists_bbox[["ymin"]] - pad_dist_y, dists_bbox[["ymax"]] + pad_dist_y),
           expand = TRUE) +
  labs(title = "London Postcode Districts") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major = element_line(linewidth = 0.2, colour = "grey90"))
print(p_districts)

# Map 3: Postcode Districts with Boroughs Overlay
p_districts_boroughs <- ggplot() +
  # Districts layer (semi-transparent fill)
  geom_sf(data = dists_lnd, fill = "lightcoral", color = "white", linewidth = 0.2, alpha = 0.6) +
  # Boroughs layer (outlines on top)
  geom_sf(data = boroughs, fill = NA, color = "#0066CC", linewidth = 1.2) +
  coord_sf(xlim = c(dists_bbox[["xmin"]] - pad_dist_x, dists_bbox[["xmax"]] + pad_dist_x),
           ylim = c(dists_bbox[["ymin"]] - pad_dist_y, dists_bbox[["ymax"]] + pad_dist_y),
           expand = TRUE) +
  labs(title = "London Postcode Districts with Borough Boundaries") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major = element_line(linewidth = 0.2, colour = "grey90"))
print(p_districts_boroughs)

# --- Interactive map with leaflet (toggle layers) ---
# Helper function to create labels safely
create_label <- function(sf_obj) {
  # Try to find a suitable label column (name, label, or first character column)
  char_cols <- names(sf_obj)[sapply(sf_obj, function(x) is.character(x) || is.factor(x))]
  if (length(char_cols) > 0) {
    # Use the first character column, excluding geometry
    label_col <- char_cols[1]
    return(as.character(sf_obj[[label_col]]))
  }
  return(NULL)
}

areas_labels <- create_label(areas_lnd)
dists_labels <- create_label(dists_lnd)

leaflet() |>
  addTiles() |>
  addPolygons(
    data = boroughs,
    group = "Boroughs",
    weight = 2, color = "#FF0000", fillOpacity = 0,
    label = ~borough_name
  ) |>
  addPolygons(
    data = areas_lnd,
    group = "Areas",
    weight = 1, color = "#808080", fillOpacity = 0,
    label = areas_labels
  ) |>
  addPolygons(
    data = dists_lnd,
    group = "Districts",
    weight = 1, color = "#000000", fillOpacity = 0,
    label = dists_labels
  ) |>
  addLayersControl(
    overlayGroups = c("Boroughs", "Areas", "Districts"),
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  fitBounds(
    lng1 = lon_bbox[["xmin"]],
    lat1 = lon_bbox[["ymin"]],
    lng2 = lon_bbox[["xmax"]],
    lat2 = lon_bbox[["ymax"]]
  )

