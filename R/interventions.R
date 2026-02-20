# Intervention helpers -------------------------------------------------------

intervention_catalog <- function() {
  tibble::tibble(
    id = c("distancing", "quarantine", "rapid"),
    label = c("Social distancing", "Targeted quarantine", "Rapid labs"),
    cost_pct_turn = c(0.25, 0.15, 0.10),
    cost_floor = c(30, 25, 40),
    Rt_multiplier = c(0.8, 1, 1),
    imports_multiplier = c(1, 0.5, 1),
    ag_delay_adjust = c(0, 0, 0),
    pcr_delay_adjust = c(0, 0, -1)
  )
}

validate_intervention_catalog <- function(catalog) {
  if (!inherits(catalog, "data.frame")) {
    stop("catalog must be a data frame")
  }

  required_cols <- c(
    "id",
    "label",
    "cost_pct_turn",
    "cost_floor",
    "Rt_multiplier",
    "imports_multiplier",
    "ag_delay_adjust",
    "pcr_delay_adjust"
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

  pct <- suppressWarnings(as.numeric(catalog$cost_pct_turn))
  if (any(!is.finite(pct)) || any(pct < 0) || any(pct > 1)) {
    stop("catalog$cost_pct_turn must contain finite values between 0 and 1")
  }

  floor_vals <- suppressWarnings(as.numeric(catalog$cost_floor))
  if (any(!is.finite(floor_vals)) || any(floor_vals < 0)) {
    stop("catalog$cost_floor must contain finite non-negative values")
  }

  invisible(TRUE)
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
      cost_pct_turn = numeric(),
      cost_floor = numeric(),
      cost_raw = integer(),
      cost_final = integer()
    )
  )
}

compute_intervention_costs <- function(selected, turn_budget, catalog, max_share = 0.60) {
  validate_intervention_catalog(catalog)

  turn_budget <- suppressWarnings(as.numeric(turn_budget)[1])
  if (!is.finite(turn_budget) || is.na(turn_budget) || turn_budget < 0) {
    stop("turn_budget must be a finite non-negative number")
  }

  max_share <- suppressWarnings(as.numeric(max_share)[1])
  if (!is.finite(max_share) || is.na(max_share) || max_share < 0 || max_share > 1) {
    stop("max_share must be a finite value between 0 and 1")
  }

  selected <- as.character(selected)
  if (length(selected) == 0) {
    return(empty_intervention_cost_details())
  }

  defs <- catalog[catalog$id %in% selected, , drop = FALSE]
  if (nrow(defs) == 0) {
    return(empty_intervention_cost_details())
  }

  raw_cost <- pmax(
    suppressWarnings(as.numeric(defs$cost_floor)),
    round(suppressWarnings(as.numeric(defs$cost_pct_turn)) * turn_budget)
  )
  raw_cost <- as.integer(pmax(raw_cost, 0))

  total_uncapped <- as.integer(sum(raw_cost))
  cap_abs <- as.integer(round(max_share * turn_budget))

  final_cost <- raw_cost
  cap_applied <- isTRUE(total_uncapped > cap_abs)

  if (isTRUE(cap_applied)) {
    if (cap_abs <= 0L || total_uncapped <= 0L) {
      final_cost <- integer(length(raw_cost))
    } else {
      scaled <- raw_cost * (cap_abs / total_uncapped)
      base <- floor(scaled)
      remainder <- as.integer(cap_abs - sum(base))

      if (remainder > 0L) {
        fractional <- scaled - base
        idx <- order(-fractional, seq_along(fractional))[seq_len(remainder)]
        base[idx] <- base[idx] + 1
      }

      final_cost <- as.integer(base)
    }
  }

  total_final <- as.integer(sum(final_cost))
  cost_share <- if (turn_budget > 0) total_final / turn_budget else 0

  list(
    total_cost = as.numeric(total_final),
    total_cost_uncapped = as.numeric(total_uncapped),
    cost_share = as.numeric(cost_share),
    cap_applied = cap_applied,
    by_intervention = tibble::tibble(
      id = defs$id,
      label = defs$label,
      cost_pct_turn = suppressWarnings(as.numeric(defs$cost_pct_turn)),
      cost_floor = suppressWarnings(as.numeric(defs$cost_floor)),
      cost_raw = as.integer(raw_cost),
      cost_final = as.integer(final_cost)
    )
  )
}

format_intervention_choice_labels <- function(catalog, turn_budget = NULL) {
  validate_intervention_catalog(catalog)

  pct <- round(suppressWarnings(as.numeric(catalog$cost_pct_turn)) * 100)
  floors <- round(suppressWarnings(as.numeric(catalog$cost_floor)))

  if (is.null(turn_budget)) {
    labels <- sprintf(
      "%s (%s%%/turn, min £%s)",
      catalog$label,
      pct,
      floors
    )
  } else {
    turn_budget <- suppressWarnings(as.numeric(turn_budget)[1])
    if (!is.finite(turn_budget) || is.na(turn_budget) || turn_budget < 0) {
      turn_budget <- 0
    }

    current_abs <- pmax(
      suppressWarnings(as.numeric(catalog$cost_floor)),
      round(suppressWarnings(as.numeric(catalog$cost_pct_turn)) * turn_budget)
    )
    labels <- sprintf(
      "%s (%s%%/turn, min £%s, now ~£%s)",
      catalog$label,
      pct,
      floors,
      round(current_abs)
    )
  }

  stats::setNames(catalog$id, labels)
}

compute_intervention_effects <- function(
    selected,
    catalog = intervention_catalog(),
    turn_budget = NULL,
    max_share = 0.60) {
  validate_intervention_catalog(catalog)

  selected <- as.character(selected)
  defs <- catalog[catalog$id %in% selected, , drop = FALSE]

  if (nrow(defs) == 0) {
    return(list(
      cost = 0,
      Rt_multiplier = 1,
      imports_multiplier = 1,
      delay_adjust = list(ag = 0, pcr = 0),
      labels = character(),
      cost_details = empty_intervention_cost_details()
    ))
  }

  if (is.null(turn_budget)) {
    base_cost <- as.integer(round(suppressWarnings(as.numeric(defs$cost_floor))))
    total_cost <- as.numeric(sum(base_cost))
    cost_details <- list(
      total_cost = total_cost,
      total_cost_uncapped = total_cost,
      cost_share = NA_real_,
      cap_applied = FALSE,
      by_intervention = tibble::tibble(
        id = defs$id,
        label = defs$label,
        cost_pct_turn = suppressWarnings(as.numeric(defs$cost_pct_turn)),
        cost_floor = suppressWarnings(as.numeric(defs$cost_floor)),
        cost_raw = as.integer(base_cost),
        cost_final = as.integer(base_cost)
      )
    )
  } else {
    cost_details <- compute_intervention_costs(
      selected = selected,
      turn_budget = turn_budget,
      catalog = catalog,
      max_share = max_share
    )
    total_cost <- cost_details$total_cost
  }

  list(
    cost = total_cost,
    Rt_multiplier = prod(defs$Rt_multiplier),
    imports_multiplier = prod(defs$imports_multiplier),
    delay_adjust = list(
      ag = sum(defs$ag_delay_adjust),
      pcr = sum(defs$pcr_delay_adjust)
    ),
    labels = defs$label,
    cost_details = cost_details
  )
}
