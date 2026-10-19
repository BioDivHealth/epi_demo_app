# prepare_place_library.R (every country + big city, one level down) ---------
#
# Run once, offline, from the project root:
#   source("prepare_place_library.R")
#
# Inputs (download once into data_raw/global/, git-ignored):
#   geoBoundariesCGAZ_ADM1.gpkg, geoBoundariesCGAZ_ADM2.gpkg
#       https://github.com/wmgeolab/geoBoundaries/tree/main/releaseData/CGAZ
#   ghs_pop_2020_30ss.zip   GHS-POP 2020, 30 arcsec (~1 km), WGS84
#       https://ghsl.jrc.ec.europa.eu/download.php?ds=pop
#   geoBoundariesOpen-meta.csv   per-country boundary licences
#   Natural Earth countries + populated places (via rnaturalearth)
#
# Output (never data/):
#   locations/library/index.rds          one row per place, used for search
#   locations/library/LICENSES.csv       boundary licence per place
#   locations/library/<id>.rds           meta + game inputs (K rebuilt on load) + backdrop
#
# Rules (deliberately blunt, see R/place_library.R):
#   country -> admin-1 units (admin-2 if fewer than 8 admin-1), far islands dropped
#   city    -> admin-2 units within the urban radius of the city point
#              (cities = population >= 1 million, plus national capitals)
#   London  -> the game's original 33 boroughs in data/ (built-in entry)

# ---- packages ----
suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(exactextractr)
  library(rnaturalearth)
})

source("R/build_location_inputs.R")
source("R/place_library.R")

# Global boundaries have small self-intersections; GEOS (s2 off) is more forgiving
sf_use_s2(FALSE)

raw_dir <- file.path("data_raw", "global")
out_dir <- getOption("place_library.out_dir", file.path("locations", "library"))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

gravity <- list(alpha = 1, beta = 2)
min_units <- 3      # places with fewer units are skipped
max_country_adm2 <- 250  # small countries drop to admin-2 only if that stays playable
city_min_pop <- 1e6 # cities at least this big (capitals always included)

# Quick trial on a few places before the full (slow) run
test_run <- isTRUE(getOption("place_library.test_run", FALSE))
test_countries <- c("PRT", "GBR", "KEN", "USA", "CYP")
test_cities <- c("Lisbon", "London", "Nairobi", "Paris", "New York", "Istanbul", "Zhuhai")


# ---- population raster ----
ghs_tif <- file.path(raw_dir, "GHS_POP_E2020_GLOBE_R2023A_4326_30ss_V1_0.tif")
if (!file.exists(ghs_tif)) {
  unzip(file.path(raw_dir, "ghs_pop_2020_30ss.zip"), files = basename(ghs_tif), exdir = raw_dir)
}
pop_raster <- terra::rast(ghs_tif)


# ---- admin boundaries ----
# Extracting polygons can split one unit into several rows; merge them back
aggregate_duplicate_ids <- function(admin) {
  dup_ids <- unique(admin$shapeID[duplicated(admin$shapeID)])
  if (length(dup_ids) == 0) {
    return(sf::st_cast(admin, "MULTIPOLYGON"))
  }
  single <- admin[!admin$shapeID %in% dup_ids, ]
  multi <- admin[admin$shapeID %in% dup_ids, ]
  merged_geom <- lapply(dup_ids, function(id) sf::st_union(sf::st_geometry(multi[multi$shapeID == id, ])))
  merged <- multi[match(dup_ids, multi$shapeID), ]
  sf::st_geometry(merged) <- do.call(c, merged_geom)
  sf::st_cast(rbind(single, merged), "MULTIPOLYGON")
}

read_admin <- function(path) {
  admin <- sf::st_read(path, quiet = TRUE)
  admin <- sf::st_set_crs(admin, 4326)
  admin <- sf::st_make_valid(admin)

  # Repairs can leave stray lines/points; keep only the polygon parts
  admin <- suppressWarnings(sf::st_collection_extract(admin, "POLYGON"))
  admin <- admin[!sf::st_is_empty(admin), ]
  admin <- aggregate_duplicate_ids(admin)
  admin[, c("shapeName", "shapeID", "shapeGroup")]
}

# Boundaries + population per unit are slow to prepare, so cache them
admin_cache <- file.path(raw_dir, "admin_with_pop.rds")

if (file.exists(admin_cache)) {
  message("Reading cached admin units with population ...")
  admin_list <- readRDS(admin_cache)
  adm1 <- admin_list$adm1
  adm2 <- admin_list$adm2
} else {
  message("Reading admin boundaries ...")
  adm1 <- read_admin(file.path(raw_dir, "geoBoundariesCGAZ_ADM1.gpkg"))
  adm2 <- read_admin(file.path(raw_dir, "geoBoundariesCGAZ_ADM2.gpkg"))

  # Population per unit (people), computed once on the full geometry
  message("Summing population for ", nrow(adm1), " admin-1 units ...")
  adm1$pop <- exactextractr::exact_extract(pop_raster, adm1, "sum", progress = FALSE)
  message("Summing population for ", nrow(adm2), " admin-2 units ...")
  adm2$pop <- exactextractr::exact_extract(pop_raster, adm2, "sum", progress = FALSE)

  saveRDS(list(adm1 = adm1, adm2 = adm2), admin_cache)
}


# ---- country names (for search) and outlines (for the backdrop) ----
ne_names <- rnaturalearth::ne_countries(scale = 10, returnclass = "sf")
ne_names <- sf::st_drop_geometry(ne_names)

ne_outlines <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")
ne_outlines <- sf::st_make_valid(ne_outlines[, c("name", "adm0_a3")])

# Look up a Natural Earth row for an ISO3 code (codes differ in a few places)
country_names <- function(iso3) {
  hit <- which(ne_names$iso_a3 == iso3 | ne_names$adm0_a3 == iso3 | ne_names$iso_a3_eh == iso3)[1]
  if (is.na(hit)) {
    return(list(name = iso3, search = iso3))
  }
  candidates <- c(ne_names$name[hit], ne_names$name_long[hit], ne_names$admin[hit],
                  ne_names$formal_en[hit], ne_names$name_en[hit], iso3)
  candidates <- unique(candidates[!is.na(candidates) & nzchar(candidates)])
  list(name = ne_names$name[hit], search = paste(candidates, collapse = "|"))
}


# ---- helpers for building one place ----

# Backdrop: things inside the place's bbox (expanded) that are not in play
crop_context <- function(layer, in_play, expand = 0.5) {
  bbox <- sf::st_bbox(in_play)
  dx <- (bbox[["xmax"]] - bbox[["xmin"]]) * expand
  dy <- (bbox[["ymax"]] - bbox[["ymin"]]) * expand
  window <- sf::st_as_sfc(sf::st_bbox(c(xmin = bbox[["xmin"]] - dx, ymin = bbox[["ymin"]] - dy,
                                        xmax = bbox[["xmax"]] + dx, ymax = bbox[["ymax"]] + dy),
                                      crs = 4326))
  hits <- lengths(suppressMessages(sf::st_intersects(layer, window))) > 0
  context <- layer[hits, ]
  simplify_for_display(context)
}

# Build game inputs, then swap in display-weight polygons
build_place <- function(units, meta, context) {
  # Blank names get a placeholder; repeated names get a number
  unit_names <- as.character(units$shapeName)
  is_blank <- is.na(unit_names) | !nzchar(trimws(unit_names))
  unit_names[is_blank] <- paste("Unit", which(is_blank))
  units$shapeName <- make.unique(unit_names, sep = " ")

  inputs <- build_location_inputs(
    units_sf = units,
    pop = pmax(round(units$pop), 1),
    name_col = "shapeName",
    gravity = gravity,
    code_col = "shapeID"
  )

  # Adjacency, kernel and population came from full geometry; maps can be light.
  # The planar copy is re-derived on load (load_place), so it is not stored.
  inputs$boros <- simplify_for_display(inputs$boros)
  inputs$boros_planar <- NULL

  # K is rebuilt on load from stored centroids/areas (it is most of the bytes)
  inputs$K <- NULL

  meta$n_units <- nrow(units)
  meta$pop_total <- sum(inputs$pop_tbl$pop)
  meta$file <- paste0(meta$id, ".rds")

  saveRDS(list(meta = meta, inputs = inputs, context = context),
          file.path(out_dir, meta$file), compress = "xz")
  meta
}

# Run one place, log and skip on error
safely_build <- function(fun, ...) {
  tryCatch(fun(...), error = function(e) {
    message("  skipped: ", conditionMessage(e))
    NULL
  })
}


# ---- countries ----
build_country <- function(iso3) {
  units <- adm1[adm1$shapeGroup == iso3, ]
  level <- "ADM1"
  adm2_units <- adm2[adm2$shapeGroup == iso3, ]
  if (nrow(units) < 8 && nrow(adm2_units) <= max_country_adm2) {
    units <- adm2_units
    level <- "ADM2"
  }
  units <- units[keep_mainland(units), ]
  if (nrow(units) < min_units) {
    stop("only ", nrow(units), " units")
  }

  names_info <- country_names(iso3)
  context <- crop_context(ne_outlines, units, expand = 0.3)

  meta <- data.frame(
    id = paste0("country_", iso3),
    label = paste0(names_info$name, " (country)"),
    type = "country",
    country = names_info$name,
    iso3 = iso3,
    level = level,
    search_names = names_info$search,
    use_builtin = FALSE
  )
  build_place(units, meta, context)
}

country_codes <- sort(unique(adm1$shapeGroup))
if (test_run) country_codes <- test_countries
country_meta <- list()
for (iso3 in country_codes) {
  message("Country ", iso3)
  country_meta[[iso3]] <- safely_build(build_country, iso3)
}


# ---- cities ----
places <- rnaturalearth::ne_download(scale = 10, type = "populated_places",
                                     category = "cultural", returnclass = "sf")
cities <- places[places$POP_MAX >= city_min_pop | places$ADM0CAP == 1, ]

# Which country (boundary group) each city sits in
city_join <- suppressMessages(sf::st_join(cities, adm1[, "shapeGroup"], join = sf::st_intersects, left = TRUE))
city_join <- city_join[!duplicated(city_join$NE_ID), ]

# Coastal cities can fall just offshore of the coarse coastline: use the nearest unit
offshore <- which(is.na(city_join$shapeGroup))
if (length(offshore)) {
  nearest <- suppressMessages(sf::st_nearest_feature(city_join[offshore, ], adm1))
  city_join$shapeGroup[offshore] <- adm1$shapeGroup[nearest]
}
if (test_run) city_join <- city_join[city_join$NAME %in% test_cities, ]

build_city <- function(i) {
  city <- city_join[i, ]
  iso3 <- city$shapeGroup
  if (is.na(iso3)) {
    stop("city not inside any admin-1 unit")
  }

  # Prefer admin-2; very coarse countries fall back to admin-1
  candidates <- adm2[adm2$shapeGroup == iso3, ]
  level <- "ADM2"
  if (nrow(candidates) < min_units) {
    candidates <- adm1[adm1$shapeGroup == iso3, ]
    level <- "ADM1"
  }

  # Urban radius from Natural Earth's urban area (km^2); 15 km if missing
  urban_km2 <- city$MAX_AREAKM
  radius_km <- if (is.na(urban_km2) || urban_km2 <= 0) 15 else sqrt(urban_km2 / pi)

  coords <- sf::st_coordinates(city)
  in_play_idx <- pick_city_units(candidates, coords[1], coords[2], radius_km)
  units <- candidates[in_play_idx, ]
  if (nrow(units) < min_units) {
    stop("only ", nrow(units), " units")
  }

  # Backdrop: neighbouring admin units at the same level (any country)
  backdrop_layer <- if (level == "ADM2") adm2 else adm1
  backdrop <- backdrop_layer[!backdrop_layer$shapeID %in% units$shapeID, ]
  context <- crop_context(backdrop, units, expand = 0.6)

  country_name <- country_names(iso3)$name
  search <- c(city$NAME, city$NAMEASCII, city$NAMEPAR, unlist(strsplit(city$NAMEALT, "[|;]")))
  search <- unique(trimws(search[!is.na(search) & nzchar(search)]))

  meta <- data.frame(
    id = paste0("city_", city$NE_ID),
    label = paste0(city$NAME, " (city, ", country_name, ")"),
    type = "city",
    country = country_name,
    iso3 = iso3,
    level = level,
    search_names = paste(search, collapse = "|"),
    use_builtin = city$NAME == "London" && iso3 == "GBR"
  )
  build_place(units, meta, context)
}

city_meta <- list()
for (i in seq_len(nrow(city_join))) {
  message("City ", city_join$NAME[i])
  city_meta[[i]] <- safely_build(build_city, i)
}


# ---- index ----
index <- do.call(rbind, c(country_meta, city_meta))
rownames(index) <- NULL

# ---- licences ----
# Each place keeps its country's geoBoundaries licence; restricted ones dropped.
# geoBoundariesOpen-meta.csv:
#   https://github.com/wmgeolab/geoBoundaries/raw/main/releaseData/geoBoundariesOpen-meta.csv
index <- add_boundary_licenses(index, file.path(raw_dir, "geoBoundariesOpen-meta.csv"))
file.remove(setdiff(file.path(out_dir, list.files(out_dir, pattern = "^(country|city)_")),
                    file.path(out_dir, index$file)))
utils::write.csv(index[, c("label", "type", "iso3", "level", "boundary_license", "license_source")],
                 file.path(out_dir, "LICENSES.csv"), row.names = FALSE)

saveRDS(index, file.path(out_dir, "index.rds"))

message("Done: ", sum(index$type == "country"), " countries, ",
        sum(index$type == "city"), " cities in ", out_dir)
