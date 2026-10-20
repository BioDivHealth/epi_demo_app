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

  deaths <- integer(n_boroughs)

  list(
    turn = 0L,
    incidence = incidence,
    cumulative = incidence,
    history = list(incidence),
    deaths = deaths,
    cumulative_deaths = deaths,
    death_history = list(deaths),
    death_queue = list()
  )
}

#' Advance the hidden outbreak by one turn
#'
#' @param state Current truth state as returned by `init_truth()` / `step_truth()`.
#' @param K Spatial kernel. Row j = where borough j's onward infections land;
#'   each row sums to 1, so every case causes Rt_global new cases on average.
#' @param Rt_global Effective reproduction number applied uniformly.
#' @param k_overdisp Negative binomial size parameter controlling overdispersion.
#' @param imports_prob Probability an import event occurs in each borough on a turn.
#' @param lambda_import Mean size of the Poisson import when it occurs.
#' @param isolated Per-borough number of (effectively) isolated cases that do
#'   not transmit this turn.
#' @param local_multiplier Per-borough multiplier on onward transmission from
#'   cases in that borough (local restrictions).
#' @param susceptible_frac Share of the population susceptible at the start
#'   (1 = nobody immune). Rt_global is the R in a fully susceptible population.
#' @param gens_per_step Generations of infection in one step (report interval
#'   in days / generation time in days). Each case's onward spread per step
#'   is (R per generation)^gens_per_step, so a weekly step keeps the same
#'   growth per day as stepping one generation at a time. 1 = one generation.
#' @return An updated state list with the next turn's incidence and cumulative totals.
#' @export
step_truth <- function(
    state,
    K,
    Rt_global,
    k_overdisp,
    imports_prob,
    lambda_import,
    pop = NULL,
    death_ifr = 0,
    death_delay = 0,
    isolated = 0,
    local_multiplier = 1,
    susceptible_frac = 1,
    gens_per_step = 1) {
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

  if (!is.numeric(death_ifr) || length(death_ifr) != 1L || is.na(death_ifr) ||
      death_ifr < 0 || death_ifr > 1) {
    stop("death_ifr must be within [0, 1]")
  }

  if (!is.numeric(death_delay) || length(death_delay) != 1L || is.na(death_delay)) {
    stop("death_delay must be a single numeric value")
  }
  death_delay <- as.integer(round(death_delay))
  if (death_delay < 0L) {
    stop("death_delay must be non-negative")
  }

  # isolated cases (found by testing) do not pass the infection on, and
  # cases in boroughs under local restrictions pass it on less
  # (the per-generation multipliers compound over the generations in a step)
  g <- gens_per_step
  I_transmit <- pmax(I_t - isolated, 0) * local_multiplier^g

  # each borough's cases spread along its row of K:
  # mu[i] = Rt^g * sum_j K[j, i] * I_transmit[j]
  mu <- as.numeric(crossprod(K, I_transmit)) * Rt_global^g
  mu[mu < 0] <- 0

  # Only susceptible people can be infected: the share still susceptible
  # starts at susceptible_frac (e.g. after vaccination) and falls as people
  # are infected. Effective R = Rt x share still susceptible.
  if (!is.null(pop)) {
    if (length(pop) != length(I_t)) {
      stop("Length of pop vector must match number of boroughs")
    }
    S_frac <- susceptible_frac - (state$cumulative / pop)
    S_frac[!is.finite(S_frac)] <- 0
    S_frac <- pmax(0, pmin(1, S_frac))
    mu <- mu * S_frac^g
  }

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

  deaths_next <- integer(length(I_next))
  death_queue <- state$death_queue
  if (is.null(death_queue) || !is.list(death_queue)) {
    death_queue <- list()
  }

  if (death_delay <= 0L) {
    if (death_ifr > 0) {
      deaths_next <- stats::rbinom(length(I_next), size = I_next, prob = death_ifr)
    }
  } else {
    death_queue <- c(death_queue, list(I_next))
    if (length(death_queue) > death_delay) {
      exposed <- death_queue[[1]]
      death_queue <- death_queue[-1]
      if (length(exposed) == length(I_next) && death_ifr > 0) {
        deaths_next <- stats::rbinom(length(exposed), size = as.integer(exposed), prob = death_ifr)
      }
    }
  }

  cumulative <- state$cumulative + I_next
  history <- c(state$history, list(I_next))
  cumulative_deaths <- state$cumulative_deaths
  if (is.null(cumulative_deaths) || length(cumulative_deaths) != length(I_next)) {
    cumulative_deaths <- integer(length(I_next))
  }
  cumulative_deaths <- cumulative_deaths + as.integer(deaths_next)
  death_history <- state$death_history
  if (is.null(death_history) || !is.list(death_history)) {
    death_history <- list()
  }
  death_history <- c(death_history, list(as.integer(deaths_next)))

  list(
    turn = state$turn + 1L,
    incidence = I_next,
    cumulative = cumulative,
    history = history,
    last_mu = mu,
    last_imports = imports,
    deaths = as.integer(deaths_next),
    cumulative_deaths = cumulative_deaths,
    death_history = death_history,
    death_queue = death_queue
  )
}
