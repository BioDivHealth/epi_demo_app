# Guided auto-run allocation policies -----------------------------------------

validate_policy_request <- function(request) {
  if (!is.list(request)) {
    stop("request must be a list")
  }

  required_fields <- c(
    "policy_id",
    "allocs_manual",
    "pop_tbl",
    "budget",
    "intervention_cost",
    "current_turn",
    "observation_state",
    "available_now"
  )
  missing_fields <- setdiff(required_fields, names(request))
  if (length(missing_fields) > 0) {
    stop("request missing required fields: ", paste(missing_fields, collapse = ", "))
  }

  if (!is.character(request$policy_id) || length(request$policy_id) != 1L || is.na(request$policy_id)) {
    stop("request$policy_id must be a single character value")
  }

  if (!inherits(request$allocs_manual, "data.frame")) {
    stop("request$allocs_manual must be a data frame")
  }
  alloc_required <- c("borough_index", "borough_name", "n_ag", "n_pcr")
  missing_alloc <- setdiff(alloc_required, names(request$allocs_manual))
  if (length(missing_alloc) > 0) {
    stop("request$allocs_manual missing required columns: ", paste(missing_alloc, collapse = ", "))
  }

  if (!inherits(request$pop_tbl, "data.frame") ||
      !all(c("borough_name", "pop") %in% names(request$pop_tbl))) {
    stop("request$pop_tbl must be a data frame with columns borough_name and pop")
  }

  if (!is.list(request$budget) || is.null(request$budget$budget) || is.null(request$budget$costs)) {
    stop("request$budget must be a list with entries budget and costs")
  }

  if (!is.numeric(request$intervention_cost) || length(request$intervention_cost) != 1L || is.na(request$intervention_cost)) {
    stop("request$intervention_cost must be a single numeric value")
  }

  if (!is.numeric(request$current_turn) || length(request$current_turn) != 1L || is.na(request$current_turn)) {
    stop("request$current_turn must be a single numeric value")
  }

  if (!is.list(request$observation_state) ||
      is.null(request$observation_state$queue) ||
      is.null(request$observation_state$history)) {
    stop("request$observation_state must be a list with queue and history entries")
  }

  if (!inherits(request$available_now, "data.frame")) {
    stop("request$available_now must be a data frame")
  }

  invisible(TRUE)
}

as_non_negative_integer <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[!is.finite(x)] <- 0
  x <- floor(x)
  x <- pmax(x, 0)
  as.integer(x)
}

policy_base_allocations <- function(pop_tbl) {
  borough_index <- if ("borough_index" %in% names(pop_tbl)) {
    as.integer(pop_tbl$borough_index)
  } else {
    seq_len(nrow(pop_tbl))
  }

  tibble::tibble(
    borough_index = borough_index,
    borough_name = as.character(pop_tbl$borough_name),
    n_ag = integer(nrow(pop_tbl)),
    n_pcr = integer(nrow(pop_tbl))
  )
}

normalise_policy_allocations <- function(allocs, pop_tbl) {
  if (!inherits(allocs, "data.frame")) {
    stop("allocs must be a data frame")
  }

  required_cols <- c("borough_name", "n_ag", "n_pcr")
  missing_cols <- setdiff(required_cols, names(allocs))
  if (length(missing_cols) > 0) {
    stop("allocs missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  base_alloc <- policy_base_allocations(pop_tbl)

  idx_by_name <- match(base_alloc$borough_name, allocs$borough_name)
  idx <- idx_by_name

  if ("borough_index" %in% names(allocs)) {
    idx_by_index <- match(base_alloc$borough_index, allocs$borough_index)
    use_index <- is.na(idx) & !is.na(idx_by_index)
    idx[use_index] <- idx_by_index[use_index]
  }

  has_match <- !is.na(idx)
  if (any(has_match)) {
    base_alloc$n_ag[has_match] <- as_non_negative_integer(allocs$n_ag[idx[has_match]])
    base_alloc$n_pcr[has_match] <- as_non_negative_integer(allocs$n_pcr[idx[has_match]])
  }

  base_alloc
}

normalise_policy_weights <- function(weights) {
  weights <- suppressWarnings(as.numeric(weights))
  weights[!is.finite(weights)] <- 0
  weights <- pmax(weights, 0)

  if (length(weights) == 0 || sum(weights) <= 0) {
    weights <- rep(1, max(length(weights), 1L))
  }

  weights / sum(weights)
}

allocate_integer_by_weights <- function(total, weights) {
  total <- suppressWarnings(as.integer(total))
  if (is.na(total) || total <= 0L) {
    return(as.integer(rep(0L, length(weights))))
  }

  weights <- normalise_policy_weights(weights)
  raw <- total * weights
  base <- floor(raw)
  remainder <- as.integer(total - sum(base))

  if (remainder > 0L) {
    fractional <- raw - base
    order_idx <- order(-fractional, seq_along(fractional))
    add_idx <- order_idx[seq_len(remainder)]
    base[add_idx] <- base[add_idx] + 1
  }

  as.integer(base)
}

allocate_integer_with_floor <- function(total, weights, floor_each = 0L) {
  total <- suppressWarnings(as.integer(total))
  if (is.na(total) || total <= 0L) {
    return(as.integer(rep(0L, length(weights))))
  }

  n <- length(weights)
  if (n == 0L) {
    return(integer())
  }

  floor_each <- suppressWarnings(as.integer(floor_each))
  if (is.na(floor_each) || floor_each < 0L) {
    floor_each <- 0L
  }

  floor_total <- as.integer(n * floor_each)
  if (floor_total > 0L && total >= floor_total) {
    base <- as.integer(rep(floor_each, n))
    remainder <- as.integer(total - floor_total)
    if (remainder > 0L) {
      base <- base + allocate_integer_by_weights(remainder, weights)
    }
    return(as.integer(base))
  }

  allocate_integer_by_weights(total, weights)
}

allocate_with_capacity <- function(total, capacity) {
  total <- suppressWarnings(as.integer(total))
  capacity <- as_non_negative_integer(capacity)
  n <- length(capacity)

  if (n == 0L || is.na(total) || total <= 0L) {
    return(as.integer(rep(0L, n)))
  }

  max_assignable <- as.integer(sum(capacity))
  if (max_assignable <= 0L) {
    return(as.integer(rep(0L, n)))
  }

  total <- min(total, max_assignable)
  shares <- normalise_policy_weights(capacity)
  raw <- total * shares
  alloc <- pmin(floor(raw), capacity)
  remaining <- as.integer(total - sum(alloc))
  if (remaining <= 0L) {
    return(as.integer(alloc))
  }

  fractional <- raw - floor(raw)
  order_idx <- order(-fractional, -capacity, seq_along(capacity))

  while (remaining > 0L) {
    for (idx in order_idx) {
      if (remaining <= 0L) {
        break
      }

      if (alloc[idx] >= capacity[idx]) {
        next
      }

      alloc[idx] <- alloc[idx] + 1L
      remaining <- remaining - 1L
    }
  }

  as.integer(alloc)
}

policy_budget_components <- function(budget, intervention_cost) {
  total_budget <- suppressWarnings(as.numeric(budget$budget))
  if (!is.finite(total_budget)) {
    total_budget <- 0
  }

  intervention_cost <- suppressWarnings(as.numeric(intervention_cost))
  if (!is.finite(intervention_cost)) {
    intervention_cost <- 0
  }

  cost_ag <- suppressWarnings(as.numeric(budget$costs[["ag"]]))
  cost_pcr <- suppressWarnings(as.numeric(budget$costs[["pcr"]]))
  if (!is.finite(cost_ag) || cost_ag < 0) {
    cost_ag <- 0
  }
  if (!is.finite(cost_pcr) || cost_pcr < 0) {
    cost_pcr <- 0
  }

  remaining <- total_budget - intervention_cost
  if (!is.finite(remaining)) {
    remaining <- 0
  }

  list(
    remaining = remaining,
    cost_ag = cost_ag,
    cost_pcr = cost_pcr
  )
}

cap_allocations_to_budget <- function(allocs, budget, intervention_cost) {
  if (!inherits(allocs, "data.frame")) {
    stop("allocs must be a data frame")
  }
  required_cols <- c("borough_index", "borough_name", "n_ag", "n_pcr")
  missing_cols <- setdiff(required_cols, names(allocs))
  if (length(missing_cols) > 0) {
    stop("allocs missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  allocs$n_ag <- as_non_negative_integer(allocs$n_ag)
  allocs$n_pcr <- as_non_negative_integer(allocs$n_pcr)

  budget_parts <- policy_budget_components(budget, intervention_cost)
  remaining <- budget_parts$remaining
  cost_ag <- budget_parts$cost_ag
  cost_pcr <- budget_parts$cost_pcr

  if (remaining <= 0) {
    allocs$n_ag <- 0L
    allocs$n_pcr <- 0L
    return(allocs)
  }

  spend <- summarise_budget(
    allocations = allocs,
    costs = list(ag = cost_ag, pcr = cost_pcr),
    budget = budget$budget
  )$spend

  if (spend <= remaining + 1e-6) {
    return(allocs)
  }

  type_cost <- c(n_ag = cost_ag, n_pcr = cost_pcr)
  type_order <- names(sort(type_cost, decreasing = TRUE))

  while (spend > remaining + 1e-6) {
    reduced <- FALSE

    for (col_name in type_order) {
      unit_cost <- type_cost[[col_name]]
      if (unit_cost <= 0) {
        next
      }

      idx <- which(allocs[[col_name]] > 0L)
      if (length(idx) == 0) {
        next
      }

      target <- idx[[length(idx)]]
      allocs[[col_name]][target] <- allocs[[col_name]][target] - 1L
      spend <- spend - unit_cost
      reduced <- TRUE

      if (spend <= remaining + 1e-6) {
        break
      }
    }

    if (!reduced) {
      break
    }
  }

  if (spend > remaining + 1e-6) {
    allocs$n_ag <- 0L
    allocs$n_pcr <- 0L
  }

  allocs
}

policy_manual <- function(request) {
  request$allocs_manual
}

policy_all_boroughs <- function(request) {
  base_alloc <- policy_base_allocations(request$pop_tbl)
  budget_parts <- policy_budget_components(request$budget, request$intervention_cost)

  if (budget_parts$remaining <= 0) {
    return(base_alloc)
  }

  weights <- normalise_policy_weights(request$pop_tbl$pop)

  cost_ag <- budget_parts$cost_ag
  cost_pcr <- budget_parts$cost_pcr
  remaining <- budget_parts$remaining

  total_ag <- 0L
  total_pcr <- 0L

  if (cost_ag > 0 && cost_pcr > 0) {
    budget_for_pcr <- floor(remaining * 0.35)
    budget_for_ag <- remaining - budget_for_pcr

    total_pcr <- as.integer(floor(budget_for_pcr / cost_pcr))
    total_ag <- as.integer(floor(budget_for_ag / cost_ag))

    spend_now <- total_pcr * cost_pcr + total_ag * cost_ag
    leftover <- remaining - spend_now
    if (leftover >= min(cost_ag, cost_pcr)) {
      if (cost_ag <= cost_pcr) {
        total_ag <- total_ag + as.integer(floor(leftover / cost_ag))
      } else {
        total_pcr <- total_pcr + as.integer(floor(leftover / cost_pcr))
      }
    }
  } else if (cost_ag > 0) {
    total_ag <- as.integer(floor(remaining / cost_ag))
  } else if (cost_pcr > 0) {
    total_pcr <- as.integer(floor(remaining / cost_pcr))
  }

  total_tests <- as.integer(total_ag + total_pcr)
  if (total_tests <= 0L) {
    return(base_alloc)
  }

  coverage_floor <- if (total_tests >= nrow(base_alloc)) 1L else 0L
  tests_by_borough <- allocate_integer_with_floor(total_tests, weights, floor_each = coverage_floor)

  pcr_by_borough <- allocate_with_capacity(
    total = total_pcr,
    capacity = tests_by_borough
  )
  ag_by_borough <- as.integer(tests_by_borough - pcr_by_borough)

  base_alloc$n_ag <- ag_by_borough
  base_alloc$n_pcr <- pcr_by_borough

  base_alloc
}

build_symptomatic_weights <- function(request) {
  base_weights <- normalise_policy_weights(request$pop_tbl$pop)
  n_boros <- nrow(request$pop_tbl)

  history <- request$observation_state$history
  available_now <- request$available_now
  current_turn <- as.integer(request$current_turn)

  signal_parts <- list()

  if (inherits(history, "data.frame") && nrow(history) > 0) {
    if ("turn_available" %in% names(history)) {
      turn_min <- max(0L, current_turn - 2L)
      history <- dplyr::filter(history, turn_available >= turn_min)
    }

    if (nrow(history) > 0) {
      signal_parts[[length(signal_parts) + 1L]] <- dplyr::mutate(history, signal_weight = 0.35)
    }
  }

  if (inherits(available_now, "data.frame") && nrow(available_now) > 0) {
    signal_parts[[length(signal_parts) + 1L]] <- dplyr::mutate(available_now, signal_weight = 1)
  }

  if (length(signal_parts) == 0) {
    return(list(weights = base_weights, signal_strength = 0, total_positives = 0))
  }

  signal_tbl <- dplyr::bind_rows(signal_parts) |>
    dplyr::group_by(borough_name) |>
    dplyr::summarise(
      weighted_tests = sum(n * signal_weight, na.rm = TRUE),
      weighted_positives = sum(positives * signal_weight, na.rm = TRUE),
      .groups = "drop"
    )

  total_tests <- sum(signal_tbl$weighted_tests, na.rm = TRUE)
  total_positives <- sum(signal_tbl$weighted_positives, na.rm = TRUE)
  focus_ready <- isTRUE(total_tests >= 10 && total_positives > 0)

  if (!focus_ready) {
    return(list(
      weights = base_weights,
      signal_strength = total_tests,
      total_positives = total_positives,
      focus_ready = FALSE
    ))
  }

  signal_scores <- rep(0, n_boros)
  idx <- match(request$pop_tbl$borough_name, signal_tbl$borough_name)
  has_match <- !is.na(idx)

  if (any(has_match)) {
    local_tests <- signal_tbl$weighted_tests[idx[has_match]]
    local_pos <- signal_tbl$weighted_positives[idx[has_match]]
    signal_scores[has_match] <- local_pos + (0.15 * local_tests)
  }

  weights_signal <- normalise_policy_weights(signal_scores)
  blended <- normalise_policy_weights((0.75 * weights_signal) + (0.25 * base_weights))

  list(
    weights = blended,
    signal_strength = total_tests,
    total_positives = total_positives,
    focus_ready = TRUE
  )
}

focus_weights_to_top_boroughs <- function(weights, pop_tbl, top_n = 12L) {
  weights <- normalise_policy_weights(weights)

  top_n <- suppressWarnings(as.integer(top_n))
  if (is.na(top_n) || top_n < 1L) {
    return(rep(0, length(weights)))
  }

  if (length(weights) <= top_n) {
    return(weights)
  }

  borough_index <- if ("borough_index" %in% names(pop_tbl)) {
    suppressWarnings(as.integer(pop_tbl$borough_index))
  } else {
    seq_len(nrow(pop_tbl))
  }
  borough_index[!is.finite(borough_index)] <- seq_len(length(weights))[!is.finite(borough_index)]

  pop <- suppressWarnings(as.numeric(pop_tbl$pop))
  pop[!is.finite(pop)] <- 0

  rank_tbl <- tibble::tibble(
    idx = seq_along(weights),
    weight = weights,
    pop = pop,
    borough_index = borough_index
  ) |>
    dplyr::arrange(dplyr::desc(weight), dplyr::desc(pop), borough_index)

  keep_n <- min(top_n, nrow(rank_tbl))
  keep_idx <- rank_tbl$idx[seq_len(keep_n)]

  focused <- rep(0, length(weights))
  focused[keep_idx] <- weights[keep_idx]
  normalise_policy_weights(focused)
}

policy_symptomatic_proxy <- function(request) {
  base_alloc <- policy_base_allocations(request$pop_tbl)
  budget_parts <- policy_budget_components(request$budget, request$intervention_cost)

  if (budget_parts$remaining <= 0) {
    return(base_alloc)
  }

  signal <- build_symptomatic_weights(request)
  weights <- signal$weights
  if (isTRUE(signal$focus_ready)) {
    weights <- focus_weights_to_top_boroughs(weights, request$pop_tbl, top_n = 12L)
  }

  cost_ag <- budget_parts$cost_ag
  cost_pcr <- budget_parts$cost_pcr
  remaining <- budget_parts$remaining

  total_ag <- 0L
  total_pcr <- 0L

  if (cost_ag > 0 && cost_pcr > 0) {
    pcr_share <- if (signal$total_positives > 0) 0.5 else 0.35
    budget_for_pcr <- floor(remaining * pcr_share)
    budget_for_ag <- remaining - budget_for_pcr

    total_pcr <- as.integer(floor(budget_for_pcr / cost_pcr))
    total_ag <- as.integer(floor(budget_for_ag / cost_ag))

    spend_now <- total_pcr * cost_pcr + total_ag * cost_ag
    leftover <- remaining - spend_now
    if (leftover >= min(cost_ag, cost_pcr)) {
      if (signal$total_positives > 0) {
        total_pcr <- total_pcr + as.integer(floor(leftover / cost_pcr))
      } else {
        total_ag <- total_ag + as.integer(floor(leftover / cost_ag))
      }
    }
  } else if (cost_ag > 0) {
    total_ag <- as.integer(floor(remaining / cost_ag))
  } else if (cost_pcr > 0) {
    total_pcr <- as.integer(floor(remaining / cost_pcr))
  }

  base_alloc$n_ag <- allocate_integer_by_weights(total_ag, weights)
  base_alloc$n_pcr <- allocate_integer_by_weights(total_pcr, weights)

  base_alloc
}

policy_none <- function(request) {
  policy_base_allocations(request$pop_tbl)
}

policy_basic_surveillance <- function(request) {
  base_alloc <- policy_base_allocations(request$pop_tbl)
  budget_parts <- policy_budget_components(request$budget, request$intervention_cost)

  if (budget_parts$remaining <= 0) {
    return(base_alloc)
  }

  if (budget_parts$cost_ag <= 0) {
    return(base_alloc)
  }

  target_budget <- floor(0.1 * budget_parts$remaining)
  target_tests <- as.integer(floor(target_budget / budget_parts$cost_ag))
  max_affordable <- as.integer(floor(budget_parts$remaining / budget_parts$cost_ag))

  total_ag <- min(target_tests, max_affordable)
  if (total_ag <= 0L && max_affordable > 0L) {
    total_ag <- 1L
  }

  weights <- normalise_policy_weights(request$pop_tbl$pop)
  base_alloc$n_ag <- allocate_integer_by_weights(total_ag, weights)
  base_alloc$n_pcr <- 0L
  base_alloc
}

build_baseline_allocations <- function(mode, request) {
  if (!is.list(request)) {
    stop("request must be a list")
  }

  required_fields <- c("pop_tbl", "budget", "intervention_cost")
  missing_fields <- setdiff(required_fields, names(request))
  if (length(missing_fields) > 0) {
    stop("request missing required fields: ", paste(missing_fields, collapse = ", "))
  }

  mode <- as.character(mode)
  if (length(mode) != 1L || is.na(mode) || !mode %in% c("no_action", "basic_surveillance")) {
    mode <- "basic_surveillance"
  }

  allocs <- switch(
    mode,
    no_action = policy_none(request),
    basic_surveillance = policy_basic_surveillance(request),
    stop("unknown baseline mode: ", mode)
  )

  allocs <- normalise_policy_allocations(allocs, request$pop_tbl)
  cap_allocations_to_budget(allocs, request$budget, request$intervention_cost)
}

build_run_mode_allocations <- function(run_mode, policy_request) {
  run_mode <- as.character(run_mode)
  if (length(run_mode) != 1L || is.na(run_mode) || !run_mode %in% c("manual", "guided_auto")) {
    run_mode <- "manual"
  }

  validate_policy_request(policy_request)

  allocs <- if (identical(run_mode, "guided_auto")) {
    build_policy_allocations(policy_request)
  } else {
    normalise_policy_allocations(
      allocs = policy_request$allocs_manual,
      pop_tbl = policy_request$pop_tbl
    )
  }

  cap_allocations_to_budget(
    allocs = allocs,
    budget = policy_request$budget,
    intervention_cost = policy_request$intervention_cost
  )
}

build_policy_allocations <- function(request) {
  validate_policy_request(request)

  policy_id <- request$policy_id
  allocs <- switch(
    policy_id,
    manual = policy_manual(request),
    all_boroughs = policy_all_boroughs(request),
    random_population = policy_all_boroughs(request),
    symptomatic_proxy = policy_symptomatic_proxy(request),
    stop("unknown policy_id: ", policy_id)
  )

  allocs <- normalise_policy_allocations(allocs, request$pop_tbl)
  cap_allocations_to_budget(allocs, request$budget, request$intervention_cost)
}
