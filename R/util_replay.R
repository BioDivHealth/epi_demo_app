# Replay utilities ---------------------------------------------------------

replay_build_truth_log <- function(truth_history, borough_names) {
  if (is.null(truth_history) || length(truth_history) == 0) {
    return(tibble::tibble())
  }
  n_boros <- length(borough_names)
  turns <- seq_len(length(truth_history)) - 1L
  tibble::tibble(
    turn = rep(turns, each = n_boros),
    borough_index = rep(seq_len(n_boros), times = length(turns)),
    gss_code = NA_character_,
    borough_name = rep(borough_names, times = length(turns)),
    incidence = as.integer(unlist(truth_history, use.names = FALSE)),
    imports = NA_integer_,
    Rt = NA_real_
  )
}

replay_resolve_truth_turns <- function(log_data) {
  if (is.null(log_data) || !is.list(log_data)) {
    return(integer())
  }
  truth_log <- log_data$truth
  truth_history <- log_data$truth_history
  history_n <- if (is.null(truth_history)) 0L else length(truth_history)
  log_turns <- if (!is.null(truth_log) && nrow(truth_log) > 0) {
    sort(unique(truth_log$turn))
  } else {
    integer()
  }
  log_n <- length(log_turns)

  if (history_n > log_n && history_n > 0) {
    return(seq_len(history_n) - 1L)
  }
  if (log_n > 0) {
    return(log_turns)
  }
  if (history_n > 0) {
    return(seq_len(history_n) - 1L)
  }
  integer()
}

replay_resolve_truth_log <- function(log_data, borough_names) {
  if (is.null(log_data) || !is.list(log_data)) {
    return(tibble::tibble())
  }
  truth_log <- log_data$truth
  truth_history <- log_data$truth_history
  history_n <- if (is.null(truth_history)) 0L else length(truth_history)
  log_turns <- if (!is.null(truth_log) && nrow(truth_log) > 0) unique(truth_log$turn) else integer()
  log_n <- length(log_turns)

  if (history_n > log_n && history_n > 0) {
    return(replay_build_truth_log(truth_history, borough_names))
  }
  if (!is.null(truth_log) && nrow(truth_log) > 0) {
    return(truth_log)
  }
  if (history_n > 0) {
    return(replay_build_truth_log(truth_history, borough_names))
  }
  tibble::tibble()
}

replay_coalesce_scalar <- function(x, fallback = 0) {
  if (is.null(x) || length(x) == 0) {
    fallback
  } else {
    val <- x[1]
    if (is.na(val)) fallback else val
  }
}

replay_safe_max <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    0
  } else {
    max(x, na.rm = TRUE)
  }
}

replay_base_map <- function(boros, bounds_list, reset_js, bbox, group) {
  leaflet::leaflet(
    boros,
    options = leaflet::leafletOptions(
      # keep the view on the place, at whatever zoom fits it
      maxBounds = bounds_list
    )
  ) |>
    leaflet::addPolygons(
      layerId = boros$borough_name,
      weight = 1,
      color = "#555555",
      fillOpacity = 0.7,
      fillColor = "#cccccc",
      label = ~borough_name,
      group = group,
      highlight = leaflet::highlightOptions(weight = 2, bringToFront = TRUE)
    ) |>
    leaflet::addEasyButton(
      leaflet::easyButton(
        icon = "fa-home",
        title = "Reset view",
        onClick = htmlwidgets::JS(reset_js)
      )
    ) |>
    leaflet::fitBounds(
      lng1 = bbox[["xmin"]],
      lat1 = bbox[["ymin"]],
      lng2 = bbox[["xmax"]],
      lat2 = bbox[["ymax"]]
    )
}

replay_summarise_observed <- function(obs_log, target_turn) {
  if (is.null(obs_log) || nrow(obs_log) == 0) {
    return(tibble::tibble())
  }
  obs_log |>
    dplyr::filter(turn_reported == target_turn) |>
    dplyr::group_by(borough_index, gss_code, borough_name) |>
    dplyr::summarise(
      tests = sum(n_returned),
      positives = sum(positives),
      positivity = ifelse(tests > 0, positives / tests, NA_real_),
      .groups = "drop"
    )
}

replay_summarise_actions <- function(action_log, target_turn) {
  if (is.null(action_log) || nrow(action_log) == 0) {
    return(tibble::tibble())
  }
  action_log |>
    dplyr::filter(turn == target_turn) |>
    dplyr::group_by(borough_index, gss_code, borough_name) |>
    dplyr::summarise(
      tests_ag = sum(ifelse(test_type == "ag", n_allocated, 0L)),
      tests_pcr = sum(ifelse(test_type == "pcr", n_allocated, 0L)),
      spend = sum(spend, na.rm = TRUE),
      allocation_spend = replay_safe_max(allocation_spend),
      intervention_cost = replay_safe_max(intervention_cost),
      total_turn_spend = replay_safe_max(total_turn_spend),
      cumulative_spend = replay_safe_max(cumulative_spend),
      budget_remaining = replay_safe_max(budget_remaining),
      interventions_active = dplyr::first(na.omit(interventions_active)),
      .groups = "drop"
    )
}

replay_build_action_points <- function(actions_df, centroid_tbl) {
  if (is.null(actions_df) || nrow(actions_df) == 0) {
    return(tibble::tibble())
  }

  points_df <- actions_df |>
    dplyr::left_join(centroid_tbl, by = "borough_name") |>
    dplyr::mutate(total_tests = tests_ag + tests_pcr)

  if (nrow(points_df) == 0) {
    return(tibble::tibble())
  }

  # small dots, growing gently with tests (large circles covered the map)
  radius <- pmin(10, 3 + log10(1 + pmax(points_df$total_tests, 0)))
  radius[!is.finite(radius)] <- 4

  point_labels <- vapply(
    seq_len(nrow(points_df)),
    function(i) {
      sprintf(
        paste0("%s<br/>Ag: %s, PCR: %s<br/>Spend %s ", teaching_params()$currency),
        points_df$borough_name[i],
        replay_coalesce_scalar(points_df$tests_ag[i], 0),
        replay_coalesce_scalar(points_df$tests_pcr[i], 0),
        round(replay_coalesce_scalar(points_df$spend[i], 0), 1)
      )
    },
    character(1)
  )

  points_df |>
    dplyr::mutate(
      radius = radius,
      label = point_labels
    )
}

replay_build_observed_layers <- function(borough_names, obs_df, actions_df, show_actions = TRUE) {
  n_boros <- length(borough_names)
  tests_vals <- rep(NA_integer_, n_boros)
  positivity_vals <- rep(NA_real_, n_boros)
  if (!is.null(obs_df) && nrow(obs_df) > 0) {
    idx_obs <- match(obs_df$borough_name, borough_names)
    valid_obs <- which(!is.na(idx_obs))
    if (length(valid_obs) > 0) {
      pos <- idx_obs[valid_obs]
      tests_vals[pos] <- obs_df$tests[valid_obs]
      positivity_vals[pos] <- obs_df$positivity[valid_obs]
    }
  }

  tests_ag_vals <- rep(0L, n_boros)
  tests_pcr_vals <- rep(0L, n_boros)
  if (!is.null(actions_df) && nrow(actions_df) > 0) {
    idx_actions <- match(actions_df$borough_name, borough_names)
    valid_act <- which(!is.na(idx_actions))
    if (length(valid_act) > 0) {
      pos <- idx_actions[valid_act]
      tests_ag_vals[pos] <- actions_df$tests_ag[valid_act]
      tests_pcr_vals[pos] <- actions_df$tests_pcr[valid_act]
    }
  }

  observed_palette <- leaflet::colorNumeric("YlOrRd", domain = c(0, 1), na.color = "#cccccc")
  fill_cols_obs <- observed_palette(positivity_vals)
  fill_cols_obs[is.na(positivity_vals)] <- "#cccccc"

  observed_labels <- vapply(
    seq_len(n_boros),
    function(i) {
      borough <- borough_names[i]
      tests_this <- tests_vals[i]
      obs_rate <- positivity_vals[i]
      observed_txt <- if (is.na(tests_this) || tests_this == 0) {
        "No returned results"
      } else {
        sprintf("%s tests, %.1f%% positive", tests_this, 100 * obs_rate)
      }

      actions_txt <- if (!isTRUE(show_actions)) {
        ""
      } else {
        ag <- tests_ag_vals[i]
        pcr <- tests_pcr_vals[i]
        if ((is.na(ag) || ag == 0) && (is.na(pcr) || pcr == 0)) {
          "Allocated tests: none"
        } else {
          sprintf(
            "Allocated tests: Ag %s, PCR %s",
            replay_coalesce_scalar(ag, 0),
            replay_coalesce_scalar(pcr, 0)
          )
        }
      }

      paste(
        sprintf("<strong>%s</strong>", borough),
        observed_txt,
        actions_txt,
        sep = "<br/>"
      )
    },
    character(1)
  )

  list(
    fill_cols = fill_cols_obs,
    labels = observed_labels
  )
}

replay_build_truth_layers <- function(
    borough_names,
    truth_df,
    truth_log,
    selected_turn,
    window,
    truth_metric) {
  n_boros <- length(borough_names)
  truth_new_vals <- rep(NA_real_, n_boros)
  truth_death_vals <- rep(NA_real_, n_boros)
  truth_imports <- rep(0L, n_boros)
  if (!is.null(truth_df) && nrow(truth_df) > 0) {
    idx_truth <- match(truth_df$borough_name, borough_names)
    valid_truth <- which(!is.na(idx_truth))
    if (length(valid_truth) > 0) {
      pos <- idx_truth[valid_truth]
      truth_new_vals[pos] <- truth_df$incidence[valid_truth]
      truth_death_vals[pos] <- truth_df$deaths[valid_truth]
      truth_imports[pos] <- truth_df$imports[valid_truth]
    }
  }

  window <- max(1L, as.integer(round(window)))

  active_df <- compute_active_infections(truth_log, turn = selected_turn, window = window)
  truth_active_vals <- rep(NA_real_, n_boros)
  if (nrow(active_df) > 0) {
    idx_active <- match(active_df$borough_name, borough_names)
    valid_active <- which(!is.na(idx_active))
    if (length(valid_active) > 0) {
      pos <- idx_active[valid_active]
      truth_active_vals[pos] <- active_df$active[valid_active]
    }
  }

  if (is.null(truth_metric) || !truth_metric %in% c("active", "new", "deaths")) {
    truth_metric <- "active"
  }
  truth_display_vals <- if (truth_metric == "active") {
    truth_active_vals
  } else if (truth_metric == "deaths") {
    truth_death_vals
  } else {
    truth_new_vals
  }

  max_truth <- suppressWarnings(max(truth_display_vals, na.rm = TRUE))
  if (!is.finite(max_truth) || max_truth <= 0) {
    max_truth <- 1
  }
  truth_palette <- leaflet::colorNumeric("Blues", domain = c(0, max_truth), na.color = "#cccccc")
  truth_fill <- truth_palette(truth_display_vals)
  truth_fill[is.na(truth_display_vals)] <- "#cccccc"

  fmt_count <- function(x) {
    if (is.na(x)) {
      "0"
    } else {
      format(round(x), big.mark = ",")
    }
  }

  truth_labels <- vapply(
    seq_len(n_boros),
    function(i) {
      new_val <- truth_new_vals[i]
      death_val <- truth_death_vals[i]
      active_val <- truth_active_vals[i]
      imports_val <- truth_imports[i]
      lines <- c(
        sprintf("<strong>%s</strong>", borough_names[i])
      )
      if (is.na(new_val) && is.na(active_val)) {
        lines <- c(lines, "No truth recorded")
      } else {
        lines <- c(
          lines,
          sprintf("New infections this turn: %s", fmt_count(new_val)),
          sprintf("Deaths this turn: %s", fmt_count(death_val)),
          sprintf("Active infections (%s-turn window): %s", window, fmt_count(active_val)),
          sprintf("Imports this turn: %s", fmt_count(imports_val))
        )
      }
      htmltools::HTML(paste(lines, collapse = "<br/>"))
    },
    character(1)
  )

  list(
    fill_cols = truth_fill,
    labels = truth_labels
  )
}

replay_action_table <- function(actions_df) {
  if (is.null(actions_df) || nrow(actions_df) == 0) {
    return(tibble::tibble(
      borough = "No allocations recorded this turn",
      antigen = NA,
      PCR = NA,
      spend = NA
    ))
  }

  actions_df |>
    dplyr::select(
      borough = borough_name,
      antigen = tests_ag,
      PCR = tests_pcr,
      spend
    ) |>
    dplyr::arrange(desc(spend))
}

replay_plot_prevalence <- function(truth_log, obs_log, pop_tbl, window = 3L) {
  if (is.null(truth_log) || nrow(truth_log) == 0) {
    return(NULL)
  }

  pop_total <- sum(pop_tbl$pop, na.rm = TRUE)
  if (!is.finite(pop_total) || pop_total <= 0) {
    return(NULL)
  }

  truth_ts <- truth_log |>
    dplyr::group_by(turn) |>
    dplyr::summarise(truth_incidence = sum(incidence), .groups = "drop") |>
    dplyr::arrange(turn)

  window <- as.integer(window)
  if (length(window) != 1 || is.na(window) || window < 1) {
    window <- 3L
  }

  active_total <- vapply(
    seq_len(nrow(truth_ts)),
    function(i) {
      start <- max(1L, i - window + 1L)
      sum(truth_ts$truth_incidence[start:i], na.rm = TRUE)
    },
    numeric(1)
  )
  truth_ts$truth_prev_pct <- 100 * active_total / pop_total

  obs_ts <- if (is.null(obs_log) || nrow(obs_log) == 0) {
    tibble::tibble(
      turn = truth_ts$turn,
      observed_tests = 0,
      observed_positives = 0
    )
  } else {
    obs_log |>
      dplyr::group_by(turn_reported) |>
      dplyr::summarise(
        observed_tests = sum(n_returned),
        observed_positives = sum(positives),
        .groups = "drop"
      ) |>
      dplyr::rename(turn = turn_reported)
  }
  obs_ts$observed_prev_pct <- ifelse(
    obs_ts$observed_tests > 0,
    100 * obs_ts$observed_positives / obs_ts$observed_tests,
    0
  )

  combined <- dplyr::full_join(truth_ts, obs_ts, by = "turn")
  if (nrow(combined) == 0) {
    return(NULL)
  }
  combined$truth_prev_pct[is.na(combined$truth_prev_pct)] <- 0
  combined$observed_prev_pct[is.na(combined$observed_prev_pct)] <- 0

  if (nrow(combined) <= 1) {
    return(
      ggplot2::ggplot(combined, ggplot2::aes(x = turn)) +
        ggplot2::geom_point(
          ggplot2::aes(y = truth_prev_pct, colour = "Truth prevalence (active)"),
          size = 3
        ) +
        ggplot2::geom_point(
          ggplot2::aes(y = observed_prev_pct, colour = "Observed positivity"),
          size = 3,
          shape = 17
        ) +
        ggplot2::labs(
          x = "Turn",
          y = "Prevalence (%)",
          colour = NULL
        ) +
        ggplot2::scale_colour_manual(values = c(
          "Truth prevalence (active)" = "#08519c",
          "Observed positivity" = "#ef6548"
        )) +
        ggplot2::theme_minimal()
    )
  }

  ggplot2::ggplot(combined, ggplot2::aes(x = turn)) +
    ggplot2::geom_line(
      ggplot2::aes(y = truth_prev_pct, colour = "Truth prevalence (active)"),
      linewidth = 1
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = observed_prev_pct, colour = "Observed positivity"),
      linewidth = 1,
      linetype = "dashed"
    ) +
    ggplot2::labs(
      x = "Turn",
      y = "Prevalence (%)",
      colour = NULL
    ) +
    ggplot2::scale_colour_manual(values = c(
      "Truth prevalence (active)" = "#08519c",
      "Observed positivity" = "#ef6548"
    )) +
    ggplot2::theme_minimal()
}

replay_plot_testing <- function(action_log) {
  if (is.null(action_log) || nrow(action_log) == 0) {
    return(NULL)
  }

  actions_ts <- action_log |>
    dplyr::group_by(turn) |>
    dplyr::summarise(
      antigen = sum(ifelse(test_type == "ag", n_allocated, 0L)),
      PCR = sum(ifelse(test_type == "pcr", n_allocated, 0L)),
      .groups = "drop"
    )

  if (nrow(actions_ts) == 0) {
    return(NULL)
  }

  actions_long <- dplyr::bind_rows(
    actions_ts |>
      dplyr::transmute(turn, test_type = "Antigen", tests = antigen),
    actions_ts |>
      dplyr::transmute(turn, test_type = "PCR", tests = PCR)
  )

  ggplot2::ggplot(actions_long, ggplot2::aes(x = turn, y = tests, fill = test_type)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::labs(
      x = "Turn",
      y = "Tests allocated",
      fill = "Test type"
    ) +
    ggplot2::scale_fill_manual(values = c("Antigen" = "#74c476", "PCR" = "#6baed6")) +
    ggplot2::theme_minimal()
}
