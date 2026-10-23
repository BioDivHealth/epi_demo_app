# Build the game's spatial inputs for any set of admin units ----------------
#
# The London game reads a precomputed bundle from data/ via load_inputs().
# These helpers build the same bundle for any polygons (e.g. Natural Earth
# admin-1 units for one country) so the game can be pointed at a new place.
#
# Kernel convention (same as the London engine, docs/rules/epidemiological-model.md):
#   Row j = where unit j's onward infections land; each row sums to 1, and the
#   engine spreads with crossprod(K, I) * Rt, so every case causes Rt new cases
#   wherever it is.
#
# Here K is a gravity kernel between unit centroids:
#     K[j, i]  proportional to  pop_i^alpha / d_ji^beta    (i = destination)
# with an explicit within-unit term on the diagonal, then rows normalised to 1.
#
# London builds its K differently (queen adjacency blended with a within-borough
# share inside load_inputs()). This K already includes the within-unit term,
# so use it as is: do NOT blend another within_share on top.
# Queen adjacency (nb, A) is still built and returned, as for London.
#
# Nothing here reads or writes data/. The London default is untouched.
#
# Needs R/place_library.R too: the small kernel/distance helpers shared with
# the app (gravity_kernel_from_points, haversine_km, local_equal_area_crs)
# live there, so the app never has to load this build-time file.


#' Build a game input bundle for any set of polygons
#'
#' @param units_sf sf object with one row per spatial unit (any CRS).
#' @param pop Population, either a numeric vector in the same order as
#'   `units_sf`, or a data frame with a `pop` column and a name column
#'   (called `name_col` or `borough_name`) that matches the unit names.
#' @param name_col Name of the column in `units_sf` holding unit names.
#' @param gravity List with `alpha` (population exponent) and `beta`
#'   (distance-decay exponent).
#' @param code_col Optional column with unit codes (used as `gss_code`).
#'   If NULL, codes "U001", "U002", ... are made up.
#' @param planar_crs Optional planar CRS for distances and adjacency. If NULL,
#'   a Lambert azimuthal equal-area projection centred on the units is used.
#' @return A list shaped like load_inputs(): boros, boros_planar, nb, A, K,
#'   K_measles (NULL), pop_tbl.
#' @export
build_location_inputs <- function(units_sf,
                                  pop,
                                  name_col,
                                  gravity = list(alpha = 1, beta = 2),
                                  code_col = NULL,
                                  planar_crs = NULL) {

  # ---- check inputs ----
  if (!requireNamespace("spdep", quietly = TRUE)) {
    stop("Package 'spdep' is needed for the adjacency; install.packages('spdep')")
  }
  if (!inherits(units_sf, "sf")) {
    stop("units_sf must be an sf object")
  }
  if (!name_col %in% names(units_sf)) {
    stop("Column '", name_col, "' not found in units_sf")
  }

  unit_names <- as.character(units_sf[[name_col]])
  n_units <- length(unit_names)

  # Relaxed check: any number of units is fine (London needs exactly 33,
  # but that check stays in load_inputs() only)
  if (n_units < 2) {
    stop("Need at least 2 units; found ", n_units)
  }
  if (anyNA(unit_names) || anyDuplicated(unit_names)) {
    stop("Unit names in '", name_col, "' must be non-missing and unique")
  }

  alpha <- gravity$alpha
  beta <- gravity$beta
  if (is.null(alpha) || is.null(beta)) {
    stop("gravity must be a list with 'alpha' and 'beta'")
  }

  # ---- unit codes ----
  if (is.null(code_col)) {
    unit_codes <- sprintf("U%03d", seq_len(n_units))
  } else {
    unit_codes <- as.character(units_sf[[code_col]])
  }

  # ---- population, aligned to unit order ----
  unit_pop <- align_population(pop, unit_names, name_col)

  # ---- tidy polygons ----
  # Keep only the columns the game uses, in the same names as London
  boros <- sf::st_sf(
    borough_name = unit_names,
    gss_code = unit_codes,
    pop = unit_pop,
    geometry = sf::st_geometry(units_sf)
  )
  boros <- sf::st_make_valid(boros)

  # WGS84 copy for leaflet
  boros_wgs84 <- sf::st_transform(boros, 4326)

  # ---- planar copy for distances and adjacency ----
  if (is.null(planar_crs)) {
    planar_crs <- local_equal_area_crs(boros_wgs84)
  }
  boros_planar <- sf::st_transform(boros, planar_crs)

  # ---- queen adjacency ----
  # Units are neighbours if they share at least one point. GEOS st_intersects
  # gives exactly the same London adjacency as spdep::poly2nb (prepare_data.R)
  # but is ~100x faster on detailed coastlines (Canada: 2 s vs 5 min).
  nb <- queen_nb(boros_planar)
  A <- spdep::nb2mat(nb, style = "B", zero.policy = TRUE)

  # Islands (no neighbours) get a 1 on the diagonal, as in London
  is_island <- rowSums(A) == 0
  if (any(is_island)) {
    diag(A)[is_island] <- 1
  }

  # ---- gravity kernel ----
  K <- gravity_kernel(boros_planar, unit_pop, alpha = alpha, beta = beta)

  # Keep centroid and area on the map layer, so K can be rebuilt later
  # (e.g. with other alpha/beta) without the full polygons
  points <- unit_points(boros_planar)
  boros_wgs84$centroid_lon <- points$centroid_lon
  boros_wgs84$centroid_lat <- points$centroid_lat
  boros_wgs84$area_km2 <- points$area_km2

  # ---- population table ----
  pop_tbl <- tibble::tibble(
    gss_code = unit_codes,
    borough_name = unit_names,
    pop = unit_pop
  )

  # ---- final checks (load_inputs() checks, minus the 33-borough rule) ----
  row_sums <- rowSums(K)
  if (nrow(K) != ncol(K) || nrow(K) != n_units) {
    stop("Kernel must be square with one row per unit")
  }
  if (any(abs(row_sums - 1) > 1e-6)) {
    stop("Kernel rows must sum to 1")
  }

  list(
    boros = boros_wgs84,
    boros_planar = boros_planar,
    nb = nb,
    A = A,
    K = K,
    K_measles = NULL,
    pop_tbl = pop_tbl
  )
}


#' Gravity kernel between unit centroids
#'
#' Row j = where unit j's onward infections land:
#' K[j, i] is proportional to pop_i^alpha / d_ji^beta (destination population),
#' rows normalised to 1. Spread with crossprod(K, I) * Rt.
#' The diagonal uses a within-unit distance: the mean distance from a random
#' point in a disc to its centre, (2/3) * sqrt(area / pi). Bigger units
#' therefore keep less of their own infection than small dense ones.
#'
#' @param units_planar sf object in a planar CRS (metres), with `borough_name`.
#' @param pop Numeric population, same order as `units_planar`.
#' @param alpha Population exponent.
#' @param beta Distance-decay exponent.
#' @return N x N row-normalised matrix with unit names as dimnames.
#' @export
gravity_kernel <- function(units_planar, pop, alpha = 1, beta = 2) {
  points <- unit_points(units_planar)
  gravity_kernel_from_points(points$centroid_lon, points$centroid_lat, points$area_km2,
                             pop, units_planar$borough_name, alpha = alpha, beta = beta)
}


#' Save a location bundle with the same file names load_inputs() reads
#'
#' Refuses to write into the app's own data/ folder (London inputs are
#' read-only). Point load_inputs(data_dir = out_dir) at the result.
#'
#' @param inputs Bundle from build_location_inputs().
#' @param out_dir Folder to write into (created if missing).
#' @export
save_location_inputs <- function(inputs, out_dir) {

  # Guard: never overwrite the London inputs
  out_full <- normalizePath(out_dir, winslash = "/", mustWork = FALSE)
  data_full <- normalizePath("data", winslash = "/", mustWork = FALSE)
  if (identical(out_full, data_full)) {
    stop("Refusing to write into data/; choose another out_dir")
  }

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  saveRDS(inputs$boros, file.path(out_dir, "boroughs.rds"))
  saveRDS(inputs$boros_planar, file.path(out_dir, "boroughs_planar.rds"))
  saveRDS(inputs$nb, file.path(out_dir, "adjacency_nb.rds"))
  saveRDS(inputs$A, file.path(out_dir, "adjacency_matrix.rds"))
  saveRDS(inputs$K, file.path(out_dir, "kernel_K.rds"))
  saveRDS(inputs$pop_tbl, file.path(out_dir, "pop_boroughs.rds"))

  invisible(out_dir)
}


#' Sum a population raster within each polygon
#'
#' Works with any people-per-cell raster (WorldPop, GHS-POP). Partial cells
#' on unit edges are weighted by the share of the cell inside the unit.
#'
#' @param units_sf sf polygons.
#' @param raster_path Path to a GeoTIFF of people per cell.
#' @return Numeric population per unit, same order as `units_sf`.
#' @export
raster_population <- function(units_sf, raster_path) {
  if (!requireNamespace("exactextractr", quietly = TRUE) || !requireNamespace("terra", quietly = TRUE)) {
    stop("Packages 'terra' and 'exactextractr' are needed; install.packages(c('terra', 'exactextractr'))")
  }

  pop_raster <- terra::rast(raster_path)

  # Match the raster CRS before extracting
  units_in_raster_crs <- sf::st_transform(units_sf, terra::crs(pop_raster))

  unit_pop <- exactextractr::exact_extract(pop_raster, units_in_raster_crs, "sum", progress = FALSE)
  unit_pop
}


# ---- small helpers ----------------------------------------------------------

# Centroid (lon/lat) and area (km^2) of each unit, from planar polygons
unit_points <- function(units_planar) {
  centroids <- sf::st_centroid(sf::st_geometry(units_planar))
  centroids_lonlat <- sf::st_coordinates(sf::st_transform(centroids, 4326))
  data.frame(
    centroid_lon = centroids_lonlat[, 1],
    centroid_lat = centroids_lonlat[, 2],
    area_km2 = as.numeric(sf::st_area(units_planar)) / 1e6
  )
}

# Queen neighbours as an spdep "nb" object (0L = no neighbours)
queen_nb <- function(units_planar) {
  touching <- sf::st_intersects(units_planar)

  nb <- lapply(seq_along(touching), function(i) {
    neighbours <- setdiff(touching[[i]], i)
    if (length(neighbours) == 0) 0L else sort(as.integer(neighbours))
  })

  class(nb) <- "nb"
  attr(nb, "region.id") <- as.character(seq_along(nb))
  attr(nb, "type") <- "queen"
  attr(nb, "sym") <- TRUE
  nb
}

# Return population as a numeric vector in the same order as unit_names
align_population <- function(pop, unit_names, name_col) {

  if (is.numeric(pop)) {
    if (length(pop) != length(unit_names)) {
      stop("pop has ", length(pop), " values but there are ", length(unit_names), " units")
    }
    unit_pop <- as.numeric(pop)
  } else if (is.data.frame(pop)) {
    # Find the name column in the population table
    pop_name_col <- intersect(c(name_col, "borough_name"), names(pop))[1]
    if (is.na(pop_name_col) || !"pop" %in% names(pop)) {
      stop("Population table needs a 'pop' column and a '", name_col,
           "' (or 'borough_name') column")
    }
    match_idx <- match(unit_names, as.character(pop[[pop_name_col]]))
    missing_names <- unit_names[is.na(match_idx)]
    if (length(missing_names)) {
      stop("Population missing for: ", paste(missing_names, collapse = ", "))
    }
    unit_pop <- as.numeric(pop$pop[match_idx])
  } else {
    stop("pop must be a numeric vector or a data frame")
  }

  if (anyNA(unit_pop) || any(unit_pop <= 0)) {
    stop("Population values must be positive and non-missing")
  }

  unit_pop
}
