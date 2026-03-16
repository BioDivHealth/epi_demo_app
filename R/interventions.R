# Intervention helpers -------------------------------------------------------

intervention_catalog <- function() {
  tibble::tibble(
    id = c(
      "distancing",
      "case_isolation_tracing",
      "rapid_labs",
      "ventilation_guidance",
      "community_case_finding"
    ),
    label = c(
      "Social distancing",
      "Case isolation and tracing",
      "Rapid labs",
      "Ventilation and venue guidance",
      "Community case-finding"
    ),
    effect_family = c(
      "acute_rt",
      "surveillance_rt",
      "lab_delay",
      "infrastructure_rt",
      "case_finding"
    ),
    activation_pct_turn = c(0.20, 0.15, 0.12, 0.18, 0.10),
    activation_floor = c(40, 30, 25, 35, 20),
    upkeep_pct_turn = c(0.12, 0.10, 0.05, 0.05, 0.06),
    upkeep_floor = c(24, 20, 10, 10, 12),
    warmup_turns = c(0L, 1L, 1L, 2L, 0L),
    peak_end_turn = c(3L, 1L, 1L, 2L, 4L),
    reset_after_off_turns = c(3L, 2L, 3L, 4L, 2L)
  )
}

validate_intervention_catalog <- function(catalog) {
  if (!inherits(catalog, "data.frame")) {
    stop("catalog must be a data frame")
  }

  required_cols <- c(
    "id",
    "label",
    "effect_family",
    "activation_pct_turn",
    "activation_floor",
    "upkeep_pct_turn",
    "upkeep_floor",
    "warmup_turns",
    "peak_end_turn",
    "reset_after_off_turns"
  )
  missing_cols <- setdiff(required_cols, names(catalog))
  if (length(missing_cols) > 0) {
    stop("catalog missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (any(is.na(catalog$id)) || any(catalog$id == "")) {
    stop("catalog$id must be non-empty")
  }
  if (anyDuplicated(catalog$id) > 0) {
    stop("catalog$id values must be unique")
  }

  pct_cols <- c("activation_pct_turn", "upkeep_pct_turn")
  for (col in pct_cols) {
    values <- suppressWarnings(as.numeric(catalog[[col]]))
    if (any(!is.finite(values)) || any(values < 0) || any(values > 1)) {
      stop("catalog$", col, " must contain finite values between 0 and 1")
    }
  }

  floor_cols <- c("activation_floor", "upkeep_floor")
  for (col in floor_cols) {
    values <- suppressWarnings(as.numeric(catalog[[col]]))
    if (any(!is.finite(values)) || any(values < 0)) {
      stop("catalog$", col, " must contain finite non-negative values")
    }
  }

  int_cols <- c("warmup_turns", "peak_end_turn", "reset_after_off_turns")
  for (col in int_cols) {
    values <- suppressWarnings(as.integer(catalog[[col]]))
    if (any(is.na(values)) || any(values < 0)) {
      stop("catalog$", col, " must contain non-negative integers")
    }
  }

  peak_end <- suppressWarnings(as.integer(catalog$peak_end_turn))
  warmup <- suppressWarnings(as.integer(catalog$warmup_turns))
  if (any(peak_end < warmup)) {
    stop("catalog$peak_end_turn must be at least warmup_turns")
  }

  invisible(TRUE)
}

format_intervention_phase <- function(status) {
  dplyr::case_when(
    status == "warming_up" ~ "Warming up",
    status == "peak" ~ "Peak",
    status == "sustained" ~ "Sustained",
    TRUE ~ "Off"
  )
}

init_intervention_state <- function(catalog = intervention_catalog()) {
  validate_intervention_catalog(catalog)

  tibble::tibble(
    id = as.character(catalog$id),
    label = as.character(catalog$label),
    active_streak = integer(nrow(catalog)),
    inactive_streak = integer(nrow(catalog)),
    status = rep("off", nrow(catalog))
  )
}

normalise_intervention_state <- function(state, catalog = intervention_catalog()) {
  validate_intervention_catalog(catalog)
  base <- init_intervention_state(catalog)

  if (is.null(state) || !inherits(state, "data.frame") || nrow(state) == 0) {
    return(base)
  }

  state_tbl <- tibble::as_tibble(state)
  if (!"id" %in% names(state_tbl)) {
    return(base)
  }

  state_clean <- tibble::tibble(
    id = as.character(state_tbl$id),
    active_streak_in = if ("active_streak" %in% names(state_tbl)) {
      suppressWarnings(as.integer(state_tbl$active_streak))
    } else {
      NA_integer_
    },
    inactive_streak_in = if ("inactive_streak" %in% names(state_tbl)) {
      suppressWarnings(as.integer(state_tbl$inactive_streak))
    } else {
      NA_integer_
    },
    status_in = if ("status" %in% names(state_tbl)) {
      as.character(state_tbl$status)
    } else {
      NA_character_
    }
  )

  merged <- base |>
    dplyr::select(id, label) |>
    dplyr::left_join(state_clean, by = "id") |>
    dplyr::mutate(
      active_streak = pmax(dplyr::coalesce(active_streak_in, 0L), 0L),
      inactive_streak = pmax(dplyr::coalesce(inactive_streak_in, 0L), 0L),
      status = dplyr::coalesce(status_in, "off"),
      status = dplyr::if_else(
        status %in% c("off", "warming_up", "peak", "sustained"),
        status,
        "off"
      )
    ) |>
    dplyr::select(id, label, active_streak, inactive_streak, status)

  merged
}

intervention_status_from_streak <- function(active_streak, selected, warmup_turns, peak_end_turn) {
  if (!isTRUE(selected) || is.na(active_streak) || active_streak <= 0L) {
    return("off")
  }
  if (active_streak <= warmup_turns) {
    return("warming_up")
  }
  if (peak_end_turn > warmup_turns && active_streak <= peak_end_turn) {
    return("peak")
  }
  "sustained"
}

preview_intervention_state <- function(state, selected, catalog = intervention_catalog()) {
  validate_intervention_catalog(catalog)

  selected <- as.character(selected)
  state_tbl <- normalise_intervention_state(state, catalog)
  defs <- tibble::as_tibble(catalog)
  out <- state_tbl
  activation_flags <- rep(FALSE, nrow(defs))

  for (i in seq_len(nrow(defs))) {
    was_active <- out$active_streak[[i]]
    was_inactive <- out$inactive_streak[[i]]
    selected_now <- defs$id[[i]] %in% selected
    reset_after <- as.integer(defs$reset_after_off_turns[[i]])
    reset_after <- max(reset_after, 1L)
    was_reset <- was_inactive >= reset_after

    if (selected_now) {
      activation_flags[[i]] <- isTRUE(was_inactive > 0L || was_active <= 0L || was_reset)
      out$active_streak[[i]] <- if (was_active <= 0L || was_reset) 1L else was_active + 1L
      out$inactive_streak[[i]] <- 0L
    } else {
      out$inactive_streak[[i]] <- was_inactive + 1L
      out$active_streak[[i]] <- if (out$inactive_streak[[i]] >= reset_after) 0L else was_active
    }
  }

  out |>
    dplyr::mutate(
      selected = id %in% selected,
      is_activation = activation_flags,
      status = purrr::pmap_chr(
        list(
          active_streak = active_streak,
          selected = selected,
          warmup_turns = defs$warmup_turns,
          peak_end_turn = defs$peak_end_turn
        ),
        intervention_status_from_streak
      )
    )
}

empty_intervention_cost_details <- function() {
  list(
    total_cost = 0,
    total_cost_uncapped = 0,
    cost_share = 0,
    cap_applied = FALSE,
    by_intervention = tibble::tibble(
      id = character(),
      label = character(),
      status = character(),
      cost_mode = character(),
      cost_pct_turn = numeric(),
      cost_floor = numeric(),
      cost_raw = numeric(),
      cost_final = numeric()
    )
  )
}

compute_intervention_costs <- function(
    selected,
    turn_budget,
    catalog,
    state = NULL,
    max_share = 0.60) {
  validate_intervention_catalog(catalog)

  turn_budget <- suppressWarnings(as.numeric(turn_budget)[1])
  if (!is.finite(turn_budget) || is.na(turn_budget) || turn_budget < 0) {
    stop("turn_budget must be a finite non-negative number")
  }

  max_share <- suppressWarnings(as.numeric(max_share)[1])
  if (!is.finite(max_share) || is.na(max_share) || max_share < 0 || max_share > 1) {
    stop("max_share must be a finite value between 0 and 1")
  }

  preview <- preview_intervention_state(state = state, selected = selected, catalog = catalog)
  if (!any(preview$selected)) {
    return(empty_intervention_cost_details())
  }

  defs <- tibble::as_tibble(catalog) |>
    dplyr::left_join(preview |> dplyr::select(id, selected, status, active_streak, inactive_streak, is_activation), by = "id") |>
    dplyr::filter(selected)

  cost_mode <- ifelse(defs$is_activation, "activation", "upkeep")
  pct <- ifelse(cost_mode == "activation", defs$activation_pct_turn, defs$upkeep_pct_turn)
  floor_vals <- ifelse(cost_mode == "activation", defs$activation_floor, defs$upkeep_floor)

  raw_cost <- pmax(round(pct * turn_budget), floor_vals)
  raw_cost <- as.numeric(pmax(raw_cost, 0))

  total_uncapped <- sum(raw_cost)
  cap_abs <- max(0, round(max_share * turn_budget))

  final_cost <- raw_cost
  cap_applied <- isTRUE(total_uncapped > cap_abs)
  if (isTRUE(cap_applied)) {
    if (cap_abs <= 0 || total_uncapped <= 0) {
      final_cost <- rep(0, length(raw_cost))
    } else {
      scaled <- raw_cost * (cap_abs / total_uncapped)
      base <- floor(scaled)
      remainder <- as.integer(round(cap_abs - sum(base)))
      if (remainder > 0L) {
        fractional <- scaled - base
        idx <- order(-fractional, seq_along(fractional))[seq_len(remainder)]
        base[idx] <- base[idx] + 1
      }
      final_cost <- as.numeric(base)
    }
  }

  total_final <- sum(final_cost)
  cost_share <- if (turn_budget > 0) total_final / turn_budget else 0

  list(
    total_cost = as.numeric(total_final),
    total_cost_uncapped = as.numeric(total_uncapped),
    cost_share = as.numeric(cost_share),
    cap_applied = cap_applied,
    by_intervention = tibble::tibble(
      id = defs$id,
      label = defs$label,
      status = defs$status,
      cost_mode = cost_mode,
      cost_pct_turn = as.numeric(pct),
      cost_floor = as.numeric(floor_vals),
      cost_raw = as.numeric(raw_cost),
      cost_final = as.numeric(final_cost)
    )
  )
}

format_intervention_choice_labels <- function(catalog, turn_budget = NULL) {
  validate_intervention_catalog(catalog)

  activation_floors <- round(suppressWarnings(as.numeric(catalog$activation_floor)))
  upkeep_floors <- round(suppressWarnings(as.numeric(catalog$upkeep_floor)))

  if (is.null(turn_budget)) {
    labels <- sprintf(
      "%s (start £%s; keep £%s)",
      catalog$label,
      activation_floors,
      upkeep_floors
    )
  } else {
    turn_budget <- suppressWarnings(as.numeric(turn_budget)[1])
    if (!is.finite(turn_budget) || is.na(turn_budget) || turn_budget < 0) {
      turn_budget <- 0
    }

    activation_abs <- pmax(
      suppressWarnings(as.numeric(catalog$activation_floor)),
      round(suppressWarnings(as.numeric(catalog$activation_pct_turn)) * turn_budget)
    )
    upkeep_abs <- pmax(
      suppressWarnings(as.numeric(catalog$upkeep_floor)),
      round(suppressWarnings(as.numeric(catalog$upkeep_pct_turn)) * turn_budget)
    )
    labels <- sprintf(
      "%s (start £%s; keep £%s)",
      catalog$label,
      round(activation_abs),
      round(upkeep_abs)
    )
  }

  stats::setNames(catalog$id, labels)
}

recent_observed_positives <- function(observation_history, current_turn, window_turns = 2L) {
  if (is.null(observation_history) || !inherits(observation_history, "data.frame") || nrow(observation_history) == 0) {
    return(0)
  }

  history_tbl <- tibble::as_tibble(observation_history)
  turn_col <- if ("turn_available" %in% names(history_tbl)) "turn_available" else if ("turn_reported" %in% names(history_tbl)) "turn_reported" else NULL
  if (is.null(turn_col) || !"positives" %in% names(history_tbl)) {
    return(0)
  }

  lo <- as.integer(current_turn) - as.integer(window_turns) + 1L
  hi <- as.integer(current_turn)
  positives <- history_tbl |>
    dplyr::filter(.data[[turn_col]] >= lo, .data[[turn_col]] <= hi) |>
    dplyr::summarise(total = sum(positives, na.rm = TRUE), .groups = "drop") |>
    dplyr::pull(total)

  if (length(positives) == 0 || !is.finite(positives)) 0 else as.numeric(positives)
}

intervention_effect_row <- function(def_row, status_row, observation_history, current_turn) {
  neutral <- list(
    Rt_multiplier = 1,
    imports_multiplier = 1,
    delay_adjust = list(ag = 0, pcr = 0),
    sampling_yield_multiplier = 1
  )

  family <- as.character(def_row$effect_family[[1]])
  status <- as.character(status_row$status[[1]])

  if (identical(status, "off") || identical(status, "warming_up")) {
    return(neutral)
  }

  if (identical(family, "acute_rt")) {
    neutral$Rt_multiplier <- if (identical(status, "peak")) 0.75 else 0.85
    return(neutral)
  }

  if (identical(family, "surveillance_rt")) {
    recent_pos <- recent_observed_positives(observation_history, current_turn, window_turns = 2L)
    neutral$Rt_multiplier <- dplyr::case_when(
      recent_pos >= 20 ~ 0.85,
      recent_pos >= 5 ~ 0.92,
      TRUE ~ 0.98
    )
    return(neutral)
  }

  if (identical(family, "lab_delay")) {
    neutral$delay_adjust$pcr <- -1
    return(neutral)
  }

  if (identical(family, "infrastructure_rt")) {
    neutral$Rt_multiplier <- 0.90
    return(neutral)
  }

  if (identical(family, "case_finding")) {
    neutral$sampling_yield_multiplier <- if (identical(status, "peak")) 1.25 else 1.15
    return(neutral)
  }

  neutral
}

intervention_status_summary <- function(status_by_intervention) {
  if (is.null(status_by_intervention) || !inherits(status_by_intervention, "data.frame")) {
    return("None")
  }

  selected <- tibble::as_tibble(status_by_intervention) |>
    dplyr::filter(selected)
  if (nrow(selected) == 0) {
    return("None")
  }

  paste(
    sprintf("%s (%s)", selected$label, format_intervention_phase(selected$status)),
    collapse = ", "
  )
}

compute_intervention_effects <- function(
    selected,
    catalog = intervention_catalog(),
    state = NULL,
    observation_history = NULL,
    current_turn = 0L,
    turn_budget = NULL,
    max_share = 0.60) {
  validate_intervention_catalog(catalog)

  preview <- preview_intervention_state(state = state, selected = selected, catalog = catalog)
  selected_rows <- preview |>
    dplyr::filter(selected)

  if (nrow(selected_rows) == 0) {
    return(list(
      cost = 0,
      Rt_multiplier = 1,
      imports_multiplier = 1,
      delay_adjust = list(ag = 0, pcr = 0),
      sampling_yield_multiplier = 1,
      labels = character(),
      status_by_intervention = preview,
      next_state = preview |>
        dplyr::select(id, label, active_streak, inactive_streak, status),
      cost_details = empty_intervention_cost_details()
    ))
  }

  if (is.null(turn_budget)) {
    cost_details <- compute_intervention_costs(
      selected = selected,
      turn_budget = 0,
      catalog = catalog,
      state = state,
      max_share = max_share
    )
  } else {
    cost_details <- compute_intervention_costs(
      selected = selected,
      turn_budget = turn_budget,
      catalog = catalog,
      state = state,
      max_share = max_share
    )
  }

  defs <- tibble::as_tibble(catalog) |>
    dplyr::inner_join(selected_rows |> dplyr::select(id, status), by = "id")

  effects_list <- lapply(seq_len(nrow(defs)), function(i) {
    intervention_effect_row(
      def_row = defs[i, , drop = FALSE],
      status_row = defs[i, , drop = FALSE],
      observation_history = observation_history,
      current_turn = current_turn
    )
  })

  rt_values <- vapply(effects_list, function(x) x$Rt_multiplier, numeric(1))
  imports_values <- vapply(effects_list, function(x) x$imports_multiplier, numeric(1))
  ag_delay_values <- vapply(effects_list, function(x) x$delay_adjust$ag, numeric(1))
  pcr_delay_values <- vapply(effects_list, function(x) x$delay_adjust$pcr, numeric(1))
  sampling_values <- vapply(effects_list, function(x) x$sampling_yield_multiplier, numeric(1))

  rt_multiplier <- prod(rt_values)
  if (nrow(selected_rows) > 0) {
    rt_multiplier <- max(rt_multiplier, 0.65)
  }

  list(
    cost = cost_details$total_cost,
    Rt_multiplier = rt_multiplier,
    imports_multiplier = prod(imports_values),
    delay_adjust = list(
      ag = sum(ag_delay_values),
      pcr = sum(pcr_delay_values)
    ),
    sampling_yield_multiplier = min(prod(sampling_values), 1.5),
    labels = selected_rows$label,
    status_by_intervention = preview |>
      dplyr::left_join(
        cost_details$by_intervention |>
          dplyr::select(id, cost_mode, cost_final),
        by = "id"
      ) |>
      dplyr::mutate(
        cost_mode = dplyr::coalesce(cost_mode, "none"),
        cost_final = dplyr::coalesce(cost_final, 0)
      ),
    next_state = preview |>
      dplyr::select(id, label, active_streak, inactive_streak, status),
    cost_details = cost_details
  )
}
