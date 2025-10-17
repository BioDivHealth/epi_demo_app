# Utility functions for loading static inputs -------------------------------

#' Load precomputed spatial inputs for the Outbreak London app
#'
#' @param data_dir Directory that contains the precomputed RDS files.
#' @return A named list with sf objects, adjacency structures, kernel, and population table.
#' @export
load_inputs <- function(data_dir = "data") {
  files <- list(
    boros = file.path(data_dir, "boroughs.rds"),
    boros_planar = file.path(data_dir, "boroughs_planar.rds"),
    nb = file.path(data_dir, "adjacency_nb.rds"),
    A = file.path(data_dir, "adjacency_matrix.rds"),
    K = file.path(data_dir, "kernel_K.rds"),
    pop_tbl = file.path(data_dir, "pop_boroughs.rds")
  )

  missing_files <- files[!vapply(files, file.exists, logical(1))]
  if (length(missing_files) > 0) {
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

  list(
    boros = boros,
    boros_planar = inputs$boros_planar,
    nb = inputs$nb,
    A = inputs$A,
    K = K,
    pop_tbl = pop_tbl
  )
}
