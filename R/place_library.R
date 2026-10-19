# Place library: choose a country or city, play at the next admin level down --
#
# prepare_place_library.R (run once, offline) turns global admin boundaries
# and a population raster into one small .rds per place plus an index.
# At run time the app only looks things up here: no internet needed.
#
#   country -> the coarsest admin level (1, 2, 3) giving 10-250 areas
#   city    -> the coarsest admin level (2, 3) giving at least 10 areas inside
#              the urban radius; otherwise the 10 nearest at the finest level
#   Places that cannot reach 10 areas are left out (thin games are not playable).
#
# Each place file holds:
#   meta     one-row data frame (id, label, type, country, level, ...)
#   inputs   bundle shaped like load_inputs() (boros, nb, A, K, pop_tbl, ...)
#   context  sf of surrounding units (not in play) for the backdrop map


# ---- choosing units (used when building the library) ------------------------

#' Pick the admin units that make up a city
#'
#' Blunt rule: units whose centroid is within `radius_km` of the city point,
#' plus the unit containing the point. Too few (< `n_min`) -> widen to the
#' `n_min` nearest units, and flag it. Too many -> keep the `n_max` nearest.
#'
#' @param units_sf Candidate units (WGS84), e.g. all admin-2 in the country.
#' @param city_lon,city_lat City point.
#' @param radius_km Urban radius, e.g. sqrt(urban_area_km2 / pi).
#' @return Row indices of `units_sf` in play, nearest first, with attribute
#'   `widened` = TRUE when the radius gave fewer than `n_min` units.
#' @export
pick_city_units <- function(units_sf, city_lon, city_lat, radius_km,
                            n_min = 10, n_max = 60) {

  city_point <- sf::st_sfc(sf::st_point(c(city_lon, city_lat)), crs = 4326)

  # Distance from the city point to each unit centroid (km)
  centroids <- centroid_lonlat(units_sf)
  dist_km <- haversine_km(centroids[, 1], centroids[, 2], city_lon, city_lat)

  # Unit that contains the city point (always in play)
  contains_city <- suppressMessages(lengths(sf::st_intersects(units_sf, city_point)) > 0)

  in_play <- which(dist_km <= radius_km | contains_city)
  widened <- length(in_play) < n_min

  # Too few units: widen to the nearest n_min
  if (widened) {
    in_play <- order(dist_km)[seq_len(min(n_min, length(dist_km)))]
  }

  # Too many units: keep the nearest
  in_play <- in_play[order(dist_km[in_play])]
  if (length(in_play) > n_max) {
    in_play <- in_play[seq_len(n_max)]
  }

  attr(in_play, "widened") <- widened
  in_play
}


#' Drop far-off islands from a country
#'
#' Units are linked if they touch or their centroids are within `max_gap_km`.
#' Only the most populous linked cluster is kept, so the Azores, Hawaii or
#' Puerto Rico (with its own nearby islands) drop out of a mainland game.
#'
#' @param units_sf Units (WGS84) with a `pop` column.
#' @return Logical vector, TRUE = keep.
#' @export
keep_mainland <- function(units_sf, max_gap_km = 500) {
  n_units <- nrow(units_sf)
  if (n_units < 3) {
    return(rep(TRUE, n_units))
  }

  # Links: touching polygons ...
  touches <- suppressMessages(sf::st_touches(units_sf, sparse = FALSE))

  # ... or centroids close together
  centroids <- centroid_lonlat(units_sf)
  dist_km <- outer(seq_len(n_units), seq_len(n_units), function(i, j) {
    haversine_km(centroids[i, 1], centroids[i, 2], centroids[j, 1], centroids[j, 2])
  })
  linked <- touches | dist_km <= max_gap_km

  # Label clusters by spreading a label along links until nothing changes
  cluster <- seq_len(n_units)
  repeat {
    new_cluster <- apply(linked, 1, function(row) min(cluster[row]))
    new_cluster <- pmin(cluster, new_cluster)
    if (identical(new_cluster, cluster)) break
    cluster <- new_cluster
  }

  # Keep the cluster with the most people
  cluster_pop <- tapply(units_sf$pop, cluster, sum)
  main_cluster <- as.integer(names(cluster_pop)[which.max(cluster_pop)])
  cluster == main_cluster
}


#' Simplify polygons for display only
#'
#' Tolerance scales with the size of the place (bbox diagonal / 500), so a
#' country and a city both keep about the same visual detail.
#' Adjacency and population are always computed on the full geometry first.
#' @export
simplify_for_display <- function(units_sf) {
  bbox <- sf::st_bbox(units_sf)
  diagonal_deg <- sqrt((bbox[["xmax"]] - bbox[["xmin"]])^2 + (bbox[["ymax"]] - bbox[["ymin"]])^2)
  tolerance_deg <- diagonal_deg / 500

  old_s2 <- sf::sf_use_s2(FALSE)
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)))

  simple <- suppressWarnings(sf::st_simplify(units_sf, preserveTopology = TRUE, dTolerance = tolerance_deg))
  simple <- sf::st_make_valid(simple)
  simple
}


# ---- looking places up (used by the app) ------------------------------------

#' Read the place index
#' @param library_dir Folder written by prepare_place_library.R.
#' @export
read_place_index <- function(library_dir = file.path("locations", "library")) {
  index_path <- file.path(library_dir, "index.rds")
  if (!file.exists(index_path)) {
    stop("No place library at ", library_dir, "; run prepare_place_library.R first")
  }
  readRDS(index_path)
}

#' Find places matching free text
#'
#' Case- and accent-insensitive. Exact name matches first (countries before
#' cities, bigger cities first), then names that start with the text, then
#' names that contain it.
#'
#' @param query Text typed by the user, e.g. "lisbon" or "Kenya".
#' @param index Place index from read_place_index().
#' @param n Maximum number of matches.
#' @return Rows of `index`, best first.
#' @export
find_place <- function(query, index, n = 10) {
  query_key <- name_key(query)
  if (!nzchar(query_key)) {
    return(index[0, ])
  }

  # One row per (place, searchable name)
  name_lists <- strsplit(index$search_names, "|", fixed = TRUE)
  long <- data.frame(
    row = rep(seq_len(nrow(index)), lengths(name_lists)),
    key = name_key(unlist(name_lists))
  )

  # Score: 1 exact, 2 starts with, 3 contains
  long$score <- ifelse(long$key == query_key, 1,
                ifelse(startsWith(long$key, query_key), 2,
                ifelse(grepl(query_key, long$key, fixed = TRUE), 3, NA)))
  long <- long[!is.na(long$score), ]
  if (nrow(long) == 0) {
    return(index[0, ])
  }

  # Best score per place
  best <- stats::aggregate(score ~ row, data = long, FUN = min)
  hits <- index[best$row, ]
  hits$score <- best$score

  # Countries before cities, then bigger places first
  type_rank <- ifelse(hits$type == "country", 1, 2)
  hits <- hits[order(hits$score, type_rank, -hits$pop_total), ]
  hits$score <- NULL
  utils::head(hits, n)
}

#' Load one place: its game inputs and backdrop
#'
#' @param place_id `id` from the index.
#' @param gravity list(alpha, beta) for the kernel, rebuilt on load.
#' @return list(meta, inputs, context). For the built-in London entry the
#'   inputs come from data/ via load_inputs() (unchanged default).
#' @export
load_place <- function(place_id, index, library_dir = file.path("locations", "library"),
                       data_dir = "data", gravity = list(alpha = 1, beta = 2)) {
  row <- index[index$id == place_id, ]
  if (nrow(row) != 1) {
    stop("Unknown place id: ", place_id)
  }

  place <- readRDS(file.path(library_dir, row$file))

  # London keeps the original 33-borough inputs the game was built on
  if (isTRUE(row$use_builtin)) {
    place$inputs <- load_inputs(data_dir = data_dir)
  }

  # K is not stored in the library; rebuild it from centroids, areas, population
  if (is.null(place$inputs$K)) {
    boros <- place$inputs$boros
    place$inputs$K <- gravity_kernel_from_points(
      boros$centroid_lon, boros$centroid_lat, boros$area_km2,
      place$inputs$pop_tbl$pop, place$inputs$pop_tbl$borough_name,
      alpha = gravity$alpha, beta = gravity$beta
    )
  }

  # Planar copy is not stored in the library; re-derive it
  if (is.null(place$inputs$boros_planar)) {
    planar_crs <- local_equal_area_crs(place$inputs$boros)
    place$inputs$boros_planar <- sf::st_transform(place$inputs$boros, planar_crs)
  }

  place
}

# Lower case, strip accents and punctuation: "Sao Paulo" with accents -> "sao paulo".
# Base R only (no stringi), so it also runs in the browser (Shinylive).
accented_letters <- "\u00C0\u00C1\u00C2\u00C3\u00C4\u00C5\u00E0\u00E1\u00E2\u00E3\u00E4\u00E5\u0100\u0101\u0102\u0103\u0104\u0105\u00C7\u00E7\u0106\u0107\u010C\u010D\u010E\u010F\u0110\u0111\u00C8\u00C9\u00CA\u00CB\u00E8\u00E9\u00EA\u00EB\u0112\u0113\u0116\u0117\u0118\u0119\u011A\u011B\u011E\u011F\u0122\u0123\u00CC\u00CD\u00CE\u00CF\u00EC\u00ED\u00EE\u00EF\u012A\u012B\u012E\u012F\u0130\u0131\u0136\u0137\u0139\u013A\u013B\u013C\u013D\u013E\u0141\u0142\u00D1\u00F1\u0143\u0144\u0145\u0146\u0147\u0148\u00D2\u00D3\u00D4\u00D5\u00D6\u00D8\u00F2\u00F3\u00F4\u00F5\u00F6\u00F8\u014C\u014D\u0150\u0151\u0154\u0155\u0158\u0159\u015A\u015B\u015E\u015F\u0160\u0161\u0218\u0219\u0162\u0163\u0164\u0165\u021A\u021B\u00D9\u00DA\u00DB\u00DC\u00F9\u00FA\u00FB\u00FC\u016A\u016B\u016E\u016F\u0170\u0171\u0172\u0173\u00DD\u00FD\u00FF\u0178\u0179\u017A\u017B\u017C\u017D\u017E"
plain_letters    <- "AAAAAAaaaaaaAaAaAaCcCcCcDdDdEEEEeeeeEeEeEeEeGgGgIIIIiiiiIiIiIiKkLlLlLlLlNnNnNnNnOOOOOOooooooOoOoRrRrSsSsSsSsTtTtTtUUUUuuuuUuUuUuUuYyyYZzZzZz"

name_key <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- chartr(accented_letters, plain_letters, x)
  x <- gsub("\u00DF", "ss", x)                # German sharp s
  x <- gsub("[\u00C6\u00E6]", "ae", x)   # ligature ae
  x <- gsub("[\u0152\u0153]", "oe", x)   # ligature oe
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", " ", x)
  trimws(x)
}

# Centroids as a lon/lat matrix (planar centroid of lon/lat polygons;
# fine for picking nearby units, no s2 needed)
centroid_lonlat <- function(units_sf) {
  old_s2 <- suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)))
  centroids <- suppressWarnings(sf::st_centroid(sf::st_geometry(units_sf)))
  sf::st_coordinates(centroids)[, 1:2, drop = FALSE]
}


# ---- kernel and distance helpers (shared with build_location_inputs.R) ------

#' Gravity kernel from unit centroids and areas
#'
#' Same kernel as gravity_kernel(), from stored numbers instead of polygons
#' (the place library keeps these, so K is rebuilt on load and alpha/beta
#' can be changed at game time).
#'
#' @param lon,lat Unit centroids (degrees).
#' @param area_km2 Unit areas (km^2), for the within-unit distance.
#' @param pop Unit populations.
#' @param unit_names Names for the dimnames.
#' @export
gravity_kernel_from_points <- function(lon, lat, area_km2, pop, unit_names,
                                       alpha = 1, beta = 2) {
  n_units <- length(pop)

  # Centroid-to-centroid great-circle distances in km
  dist_km <- outer(seq_len(n_units), seq_len(n_units), function(i, j) {
    haversine_km(lon[i], lat[i], lon[j], lat[j])
  })

  # Within-unit distance for the diagonal (km)
  self_km <- (2 / 3) * sqrt(area_km2 / pi)
  diag(dist_km) <- self_km

  if (any(dist_km <= 0)) {
    stop("Zero distance between units; check for duplicate or empty polygons")
  }

  # Gravity weights: row = source unit, column = destination unit,
  # so each column carries the destination population
  pop_weight <- matrix(pop^alpha, nrow = n_units, ncol = n_units, byrow = TRUE)
  W <- pop_weight / dist_km^beta

  # Row-normalise so each row sums to 1
  K <- W / rowSums(W)

  dimnames(K) <- list(unit_names, unit_names)
  K
}

# Great-circle distance in km
haversine_km <- function(lon1, lat1, lon2, lat2) {
  to_rad <- pi / 180
  d_lat <- (lat2 - lat1) * to_rad
  d_lon <- (lon2 - lon1) * to_rad
  a <- sin(d_lat / 2)^2 + cos(lat1 * to_rad) * cos(lat2 * to_rad) * sin(d_lon / 2)^2
  6371 * 2 * asin(pmin(1, sqrt(a)))
}

# Lambert azimuthal equal-area projection centred on the units (metres).
# Works anywhere in the world without having to pick a UTM zone.
local_equal_area_crs <- function(units_wgs84) {
  bbox <- sf::st_bbox(units_wgs84)
  lon_0 <- mean(c(bbox[["xmin"]], bbox[["xmax"]]))
  lat_0 <- mean(c(bbox[["ymin"]], bbox[["ymax"]]))
  sprintf("+proj=laea +lat_0=%.4f +lon_0=%.4f +units=m +datum=WGS84", lat_0, lon_0)
}


# ---- data licences ----------------------------------------------------------

#' Attach the boundary licence to each place and drop restricted ones
#'
#' geoBoundaries data comes under each country's own licence (mostly ODbL,
#' CC BY, CC BY-SA or public domain). Places whose boundaries need direct
#' permission from the provider are removed, so the library can be shared.
#'
#' @param index Place index.
#' @param meta_csv geoBoundariesOpen-meta.csv (boundaryISO, boundaryType, boundaryLicense).
#' @return Index with `boundary_license` and `license_source`, restricted places dropped.
#' @export
add_boundary_licenses <- function(index, meta_csv) {
  gb_meta <- utils::read.csv(meta_csv, stringsAsFactors = FALSE)
  gb_meta <- gb_meta[!duplicated(gb_meta[, c("boundaryISO", "boundaryType")]), ]

  key_index <- paste(index$iso3, index$level)
  key_meta <- paste(gb_meta$boundaryISO, gb_meta$boundaryType)
  hit <- match(key_index, key_meta)

  index$boundary_license <- gb_meta$boundaryLicense[hit]
  index$license_source <- gb_meta$licenseSource[hit]
  index$boundary_license[is.na(hit)] <- "Unknown: check geoBoundaries metadata"

  # Not freely redistributable
  restricted <- grepl("Direct Permission", index$boundary_license)
  if (any(restricted)) {
    message("Dropping places with restricted boundary licences: ",
            paste(index$label[restricted], collapse = ", "))
  }
  index[!restricted, ]
}
