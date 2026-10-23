# Utility functions for loading static inputs -------------------------------

#' Load precomputed spatial inputs for the Outbreak London app
#'
#' @param data_dir Directory that contains the precomputed RDS files.
#' @param within_share Share of a case's onward infections that stay in its
#'   own borough (default kernel). The rest go to touching boroughs.
#' @param within_share_measles The same for the measles kernel (lower
#'   mobility, so more stays local).
#' @return A named list with sf objects, adjacency structures, kernel, and population table.
#' @export
load_inputs <- function(data_dir = "data", within_share = 0.7, within_share_measles = 0.8) {
  kernel_measles_path <- file.path(data_dir, "kernel_K_measles.rds")
  files <- list(
    boros = file.path(data_dir, "boroughs.rds"),
    boros_planar = file.path(data_dir, "boroughs_planar.rds"),
    nb = file.path(data_dir, "adjacency_nb.rds"),
    A = file.path(data_dir, "adjacency_matrix.rds"),
    K = file.path(data_dir, "kernel_K.rds"),
    pop_tbl = file.path(data_dir, "pop_boroughs.rds")
  )

  exists_mask <- vapply(files, file.exists, logical(1))
  if (!all(exists_mask)) {
    missing_files <- unlist(files[!exists_mask])
    stop(
      "Missing expected data files in ", data_dir, ": ",
      paste(basename(missing_files), collapse = ", ")
    )
  }

  inputs <- lapply(files, readRDS)

  boros <- inputs$boros
  if (!inherits(boros, "sf")) {
    stop("boroughs.rds must contain an sf object")
  }

  n_boroughs <- nrow(boros)
  if (n_boroughs != 33L) {
    stop("Expected 33 boroughs; found ", n_boroughs)
  }

  K <- inputs$K
  if (!is.matrix(K)) {
    stop("kernel_K.rds must be a matrix")
  }

  if (nrow(K) != ncol(K)) {
    stop("Spatial kernel must be square")
  }

  if (nrow(K) != n_boroughs) {
    stop("Spatial kernel dimension mismatch with borough count")
  }

  row_sums <- rowSums(K)
  if (any(!is.finite(row_sums))) {
    stop("Spatial kernel rows must have finite sums")
  }

  if (any(abs(row_sums - 1) > 1e-6)) {
    stop("Spatial kernel rows must sum to 1 within tolerance")
  }

  # Kernel convention: row j says where borough j's onward infections land,
  # and each row sums to 1, so every case causes Rt new cases wherever it is
  # (step_truth spreads with t(K) %*% I).
  #
  # kernel_K.rds holds only "spread evenly to touching boroughs" (A / degree,
  # no within-borough spread), so keep a share of each case's infections in
  # its own borough. These shares are guesses until a gravity kernel
  # replaces them.
  neighbour_spread <- K
  K <- within_share * diag(nrow(K)) + (1 - within_share) * neighbour_spread
  dimnames(K) <- dimnames(neighbour_spread)

  # The measles kernel is built the same way with more local spread.
  # (kernel_K_measles.rds is still checked below but no longer used.)
  K_measles_built <- within_share_measles * diag(nrow(K)) + (1 - within_share_measles) * neighbour_spread
  dimnames(K_measles_built) <- dimnames(neighbour_spread)

  K_measles <- NULL
  if (file.exists(kernel_measles_path)) {
    K_measles <- readRDS(kernel_measles_path)
    if (!is.matrix(K_measles)) {
      stop("kernel_K_measles.rds must be a matrix")
    }
    if (nrow(K_measles) != ncol(K_measles)) {
      stop("Measles kernel must be square")
    }
    if (nrow(K_measles) != nrow(K)) {
      stop("Measles kernel dimension mismatch with default kernel")
    }
    row_sums_measles <- rowSums(K_measles)
    if (any(!is.finite(row_sums_measles))) {
      stop("Measles kernel rows must have finite sums")
    }
    if (any(abs(row_sums_measles - 1) > 1e-6)) {
      stop("Measles kernel rows must sum to 1 within tolerance")
    }
  }
  K_measles <- K_measles_built

  pop_tbl <- inputs$pop_tbl
  if (!all(c("gss_code", "borough_name", "pop") %in% names(pop_tbl))) {
    stop("Population table must include gss_code, borough_name, pop")
  }

  if (any(is.na(pop_tbl$pop))) {
    stop("Population table contains missing population values")
  }

  if (nrow(pop_tbl) != n_boroughs) {
    stop("Population table row count must match borough count")
  }

  # Everything (K, A, populations, names) is indexed by position, in the
  # order of boroughs.rds. pop_boroughs.rds is stored alphabetically, so put
  # it in the map's order using the GSS code.
  pop_order <- match(boros$gss_code, pop_tbl$gss_code)
  if (any(is.na(pop_order))) {
    stop("Population table GSS codes do not match boroughs.rds")
  }
  pop_tbl <- pop_tbl[pop_order, , drop = FALSE]

  if (!all(pop_tbl$borough_name == boros$borough_name)) {
    stop("Borough names in the population table do not match boroughs.rds")
  }

  list(
    boros = boros,
    boros_planar = inputs$boros_planar,
    nb = inputs$nb,
    A = inputs$A,
    K = K,
    K_measles = K_measles,
    pop_tbl = pop_tbl
  )
}
