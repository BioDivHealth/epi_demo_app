# Build the game's spatial inputs for any set of admin units ----------------
#
# The London game reads a precomputed bundle from data/ via load_inputs().
# These helpers build the same bundle for any polygons (e.g. Natural Earth
# admin-1 units for one country) so the game can be pointed at a new place.
#
# Differences from the London kernel (prepare_data.R):
#   - London K is queen adjacency, row-normalised (infection only moves to
#     touching boroughs).
#   - Here K is a gravity kernel between unit centroids:
#         K_ij  proportional to  pop_j^alpha / d_ij^beta
#     with an explicit within-unit term on the diagonal, then row-normalised
#     so each row sums to 1 (same semantics as docs/rules/epidemiological-model.md:
#     I_{t+1} ~ NegBin(K %*% I_t * Rt, k)).
#   - Queen adjacency (nb, A) is still built and returned, exactly as London.
#
# Nothing here reads or writes data/. The London default is untouched.


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

  # ---- queen adjacency (same recipe as prepare_data.R) ----
  nb <- spdep::poly2nb(sf::st_geometry(boros_planar), queen = TRUE)
  A <- spdep::nb2mat(nb, style = "B", zero.policy = TRUE)

  # Islands (no neighbours) get a 1 on the diagonal, as in London
  is_island <- rowSums(A) == 0
  if (any(is_island)) {
    diag(A)[is_island] <- 1
  }

  # ---- gravity kernel ----
  K <- gravity_kernel(boros_planar, unit_pop, alpha = alpha, beta = beta)

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
#' K_ij is proportional to pop_j^alpha / d_ij^beta, rows normalised to 1.
#' The diagonal uses a within-unit distance: the mean distance from a random
#' point in a disc to its centre, (2/3) * sqrt(area / pi). Bigger units
#' therefore keep less of their own infection than small dense ones.
#'
#' @param units_planar sf object in a planar CRS (metres).
#' @param pop Numeric population, same order as `units_planar`.
#' @param alpha Population exponent.
#' @param beta Distance-decay exponent.
#' @return N x N row-normalised matrix with unit names as dimnames.
#' @export
gravity_kernel <- function(units_planar, pop, alpha = 1, beta = 2) {

  # Centroid-to-centroid distances in km
  centroids <- sf::st_centroid(sf::st_geometry(units_planar))
  dist_m <- sf::st_distance(centroids)
  dist_km <- matrix(as.numeric(dist_m), nrow = length(pop)) / 1000

  # Within-unit distance for the diagonal (km)
  area_km2 <- as.numeric(sf::st_area(units_planar)) / 1e6
  self_km <- (2 / 3) * sqrt(area_km2 / pi)
  diag(dist_km) <- self_km

  if (any(dist_km <= 0)) {
    stop("Zero distance between units; check for duplicate or empty polygons")
  }

  # Gravity weights: column j is the source population
  pop_weight <- matrix(pop^alpha, nrow = length(pop), ncol = length(pop), byrow = TRUE)
  W <- pop_weight / dist_km^beta

  # Row-normalise so each row sums to 1
  K <- W / rowSums(W)

  unit_names <- units_planar$borough_name
  dimnames(K) <- list(unit_names, unit_names)
  K
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


# ---- small helpers ----------------------------------------------------------

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

# Lambert azimuthal equal-area projection centred on the units (metres).
# Works anywhere in the world without having to pick a UTM zone.
local_equal_area_crs <- function(units_wgs84) {
  bbox <- sf::st_bbox(units_wgs84)
  lon_0 <- mean(c(bbox[["xmin"]], bbox[["xmax"]]))
  lat_0 <- mean(c(bbox[["ymin"]], bbox[["ymax"]]))
  sprintf("+proj=laea +lat_0=%.4f +lon_0=%.4f +units=m +datum=WGS84", lat_0, lon_0)
}
