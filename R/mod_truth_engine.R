# Truth engine ----------------------------------------------------------------

#' Initialise the hidden outbreak state
#'
#' @param n_boroughs Number of spatial units.
#' @param seed_boroughs Integer indices of boroughs that start with cases.
#' @param seed_cases Integer vector of cases corresponding to `seed_boroughs`.
#' @return A list describing the hidden epidemic state.
#' @export
init_truth <- function(n_boroughs, seed_boroughs = 1L, seed_cases = 10L) {
  if (length(n_boroughs) != 1L || !is.numeric(n_boroughs)) {
    stop("n_boroughs must be a single numeric value")
  }

  n_boroughs <- as.integer(n_boroughs)
  if (n_boroughs <= 0L) {
    stop("n_boroughs must be positive")
  }

  incidence <- integer(n_boroughs)

  if (length(seed_boroughs) != length(seed_cases)) {
    stop("seed_boroughs and seed_cases must have the same length")
  }

  if (length(seed_boroughs) > 0) {
    idx <- as.integer(seed_boroughs)
    if (any(idx < 1L | idx > n_boroughs)) {
      stop("seed_boroughs must fall within [1, n_boroughs]")
    }
    incidence[idx] <- incidence[idx] + as.integer(seed_cases)
  }

  list(
    turn = 0L,
    incidence = incidence,
    cumulative = incidence,
    history = list(incidence)
  )
}

#' Advance the hidden outbreak by one turn
#'
#' @param state Current truth state as returned by `init_truth()` / `step_truth()`.
#' @param K Row-normalised spatial kernel matrix.
#' @param Rt_global Effective reproduction number applied uniformly.
#' @param k_overdisp Negative binomial size parameter controlling overdispersion.
#' @param imports_prob Probability an import event occurs in each borough on a turn.
#' @param lambda_import Mean size of the Poisson import when it occurs.
#' @return An updated state list with the next turn's incidence and cumulative totals.
#' @export
step_truth <- function(
    state,
    K,
    Rt_global,
    k_overdisp,
    imports_prob,
    lambda_import) {
  if (is.null(state) || !is.list(state)) {
    stop("state must be a list created by init_truth()")
  }

  I_t <- as.numeric(state$incidence)
  if (!is.matrix(K)) {
    stop("K must be a matrix")
  }

  if (ncol(K) != length(I_t)) {
    stop("Kernel dimension mismatch with incidence vector")
  }

  if (!is.numeric(Rt_global) || length(Rt_global) != 1L) {
    stop("Rt_global must be a single numeric value")
  }

  if (!is.numeric(k_overdisp) || length(k_overdisp) != 1L) {
    stop("k_overdisp must be a single numeric value")
  }

  if (k_overdisp <= 0 && !is.infinite(k_overdisp)) {
    stop("k_overdisp must be positive or Inf")
  }

  if (!is.numeric(imports_prob) || length(imports_prob) != 1L ||
      imports_prob < 0 || imports_prob > 1) {
    stop("imports_prob must be within [0, 1]")
  }

  if (!is.numeric(lambda_import) || length(lambda_import) != 1L ||
      lambda_import < 0) {
    stop("lambda_import must be non-negative")
  }

  mu <- as.numeric(K %*% I_t) * Rt_global
  mu[mu < 0] <- 0

  offspring <- if (is.infinite(k_overdisp)) {
    stats::rpois(length(mu), lambda = mu)
  } else {
    stats::rnbinom(length(mu), size = k_overdisp, mu = mu)
  }

  gates <- stats::rbinom(length(mu), size = 1L, prob = imports_prob)
  import_draws <- stats::rpois(length(mu), lambda = lambda_import)
  imports <- gates * import_draws

  I_next <- as.integer(offspring + imports)
  I_next[I_next < 0] <- 0L

  cumulative <- state$cumulative + I_next
  history <- c(state$history, list(I_next))

  list(
    turn = state$turn + 1L,
    incidence = I_next,
    cumulative = cumulative,
    history = history,
    last_mu = mu,
    last_imports = imports
  )
}
