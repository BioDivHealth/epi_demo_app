# Replay utilities ---------------------------------------------------------

build_replay_snapshot <- function(player_branch, baseline_branch, comparison_report = NULL) {
  empty_logs <- init_turn_logs(
    tibble::tibble(borough_name = character(), gss_code = character(), pop = numeric())
  )
  empty_branch <- list(
    truth = empty_logs$truth,
    observations = empty_logs$observations,
    actions = empty_logs$actions,
    run_log = tibble::tibble(),
    truth_history = list()
  )

  branch_payload <- function(branch_state) {
    if (is.null(branch_state)) {
      return(empty_branch)
    }
    list(
      truth = branch_state$turn_logs$truth,
      observations = branch_state$turn_logs$observations,
      actions = branch_state$turn_logs$actions,
      run_log = branch_state$run_log,
      truth_history = branch_state$truth_history
    )
  }

  list(
    branches = list(
      player = branch_payload(player_branch),
      baseline = branch_payload(baseline_branch)
    ),
    comparison = if (!is.null(comparison_report)) comparison_report$comparison else NULL,
    turn_comparison = if (!is.null(comparison_report)) comparison_report$turn_comparison else NULL
  )
}

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

replay_branch_names <- function(log_data) {
  if (is.null(log_data) || !is.list(log_data)) {
    return(c("player"))
  }

  branch_payload <- log_data$branches
  if (!is.list(branch_payload) || length(branch_payload) == 0) {
    return(c("player"))
  }

  branch_names <- names(branch_payload)
  branch_names <- branch_names[!is.na(branch_names) & nzchar(branch_names)]
  if (length(branch_names) == 0) {
    return(c("player"))
  }

  unique(as.character(branch_names))
}

replay_get_branch_logs <- function(log_data, branch = "player") {
  empty_logs <- list(
    truth = tibble::tibble(),
    observations = tibble::tibble(),
    actions = tibble::tibble(),
    run_log = tibble::tibble(),
    truth_history = list()
  )

  if (is.null(log_data) || !is.list(log_data)) {
    return(empty_logs)
  }

  normalize_tbl <- function(x) {
    if (is.null(x) || !is.data.frame(x)) {
      tibble::tibble()
    } else {
      tibble::as_tibble(x)
    }
  }

  normalize_history <- function(x) {
    if (is.null(x) || !is.list(x)) {
      list()
    } else {
      x
    }
  }

  if (is.list(log_data$branches) && length(log_data$branches) > 0) {
    branch_names <- replay_branch_names(log_data)
    branch_key <- as.character(branch)[1]
    if (!branch_key %in% branch_names) {
      if ("player" %in% branch_names) {
        branch_key <- "player"
      } else {
        branch_key <- branch_names[[1]]
      }
    }

    branch_logs <- log_data$branches[[branch_key]]
    if (!is.list(branch_logs)) {
      return(empty_logs)
    }

    return(list(
      truth = normalize_tbl(branch_logs$truth),
      observations = normalize_tbl(branch_logs$observations),
      actions = normalize_tbl(branch_logs$actions),
      run_log = normalize_tbl(branch_logs$run_log),
      truth_history = normalize_history(branch_logs$truth_history)
    ))
  }

  list(
    truth = normalize_tbl(log_data$truth),
    observations = normalize_tbl(log_data$observations),
    actions = normalize_tbl(log_data$actions),
    run_log = normalize_tbl(log_data$run_log),
    truth_history = normalize_history(log_data$truth_history)
  )
}

replay_filter_branch <- function(df, branch) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(df)
  }
  if (!"branch" %in% names(df)) {
    return(df)
  }
  branch_value <- as.character(branch)[1]
  df[df$branch == branch_value, , drop = FALSE]
}

replay_resolve_truth_turns <- function(log_data, branch = "player") {
  if (is.null(log_data) || !is.list(log_data)) {
    return(integer())
  }
  branch_logs <- replay_get_branch_logs(log_data, branch = branch)
  truth_log <- replay_filter_branch(branch_logs$truth, branch = branch)
  truth_history <- branch_logs$truth_history
  history_n <- if (is.null(truth_history)) 0L else length(truth_history)
  log_turns <- if (!is.null(truth_log) && nrow(truth_log) > 0 && "turn" %in% names(truth_log)) {
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

replay_resolve_truth_log <- function(log_data, borough_names, branch = "player") {
  if (is.null(log_data) || !is.list(log_data)) {
    return(tibble::tibble())
  }
  branch_logs <- replay_get_branch_logs(log_data, branch = branch)
  truth_log <- replay_filter_branch(branch_logs$truth, branch = branch)
  truth_history <- branch_logs$truth_history
  history_n <- if (is.null(truth_history)) 0L else length(truth_history)
  log_turns <- if (!is.null(truth_log) && nrow(truth_log) > 0 && "turn" %in% names(truth_log)) unique(truth_log$turn) else integer()
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
      maxBounds = bounds_list,
      minZoom = 9
    )
  ) |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
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

replay_summarise_observed <- function(obs_log, target_turn, branch = "player") {
  if (is.null(obs_log) || nrow(obs_log) == 0) {
    return(tibble::tibble())
  }
  obs_log <- replay_filter_branch(obs_log, branch = branch)
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

replay_summarise_actions <- function(action_log, target_turn, branch = "player") {
  if (is.null(action_log) || nrow(action_log) == 0) {
    return(tibble::tibble())
  }
  action_log <- replay_filter_branch(action_log, branch = branch)
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

  radius <- 4 + sqrt(pmax(points_df$total_tests, 0))
  radius[!is.finite(radius)] <- 4

  point_labels <- vapply(
    seq_len(nrow(points_df)),
    function(i) {
      sprintf(
        "%s<br/>Ag: %s, PCR: %s<br/>Spend £%s",
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

replay_build_intervention_timeline <- function(run_log, turns, intervention_defs, action_log = NULL) {
  turns <- suppressWarnings(as.integer(turns))
  turns <- turns[is.finite(turns)]
  turns <- sort(unique(turns))
  if (length(turns) == 0) {
    return(tibble::tibble())
  }

  if (is.null(intervention_defs) || !is.data.frame(intervention_defs) || nrow(intervention_defs) == 0) {
    return(tibble::tibble())
  }
  if (!all(c("id", "label") %in% names(intervention_defs))) {
    return(tibble::tibble())
  }

  defs <- tibble::as_tibble(intervention_defs) |>
    dplyr::select(dplyr::any_of(c("id", "label"))) |>
    dplyr::filter(!is.na(id), !is.na(label), id != "", label != "") |>
    dplyr::distinct(id, label)
  if (nrow(defs) == 0) {
    return(tibble::tibble())
  }

  parse_labels <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x) || x == "" || identical(x, "None")) {
      return(character())
    }
    parts <- trimws(unlist(strsplit(as.character(x), ",", fixed = TRUE), use.names = FALSE))
    parts <- parts[nzchar(parts)]
    if (length(parts) == 0) character() else unique(parts)
  }

  labels_by_turn <- tibble::tibble(turn = turns, active_interventions = "None")

  if (!is.null(run_log) && is.data.frame(run_log) && nrow(run_log) > 0 &&
      "turn" %in% names(run_log) && "active_interventions" %in% names(run_log)) {
    run_by_turn <- tibble::as_tibble(run_log) |>
      dplyr::filter(!is.na(turn)) |>
      dplyr::group_by(turn) |>
      dplyr::summarise(
        active_interventions = dplyr::first(active_interventions[!is.na(active_interventions)]),
        .groups = "drop"
      )
    labels_by_turn <- labels_by_turn |>
      dplyr::left_join(run_by_turn, by = "turn", suffix = c("", "_log")) |>
      dplyr::mutate(active_interventions = dplyr::coalesce(active_interventions_log, active_interventions)) |>
      dplyr::select(turn, active_interventions)
  }

  if (!is.null(action_log) && is.data.frame(action_log) && nrow(action_log) > 0 &&
      "turn" %in% names(action_log) && "interventions_active" %in% names(action_log)) {
    action_by_turn <- tibble::as_tibble(action_log) |>
      dplyr::filter(!is.na(turn)) |>
      dplyr::group_by(turn) |>
      dplyr::summarise(
        interventions_active = dplyr::first(interventions_active[!is.na(interventions_active)]),
        .groups = "drop"
      )
    labels_by_turn <- labels_by_turn |>
      dplyr::left_join(action_by_turn, by = "turn") |>
      dplyr::mutate(
        active_interventions = ifelse(
          is.na(active_interventions) | active_interventions == "" | active_interventions == "None",
          interventions_active,
          active_interventions
        ),
        active_interventions = dplyr::coalesce(active_interventions, "None")
      ) |>
      dplyr::select(turn, active_interventions)
  }

  labels_lookup <- stats::setNames(defs$id, defs$label)
  active_rows <- lapply(seq_len(nrow(labels_by_turn)), function(i) {
    turn_i <- labels_by_turn$turn[[i]]
    labels_i <- parse_labels(labels_by_turn$active_interventions[[i]])
    if (length(labels_i) == 0) {
      return(NULL)
    }

    ids_i <- unname(labels_lookup[labels_i])
    ids_i <- ids_i[!is.na(ids_i) & ids_i != ""]
    if (length(ids_i) == 0) {
      return(NULL)
    }

    tibble::tibble(
      turn = turn_i,
      intervention_id = as.character(ids_i)
    )
  })
  active_df <- dplyr::bind_rows(active_rows)

  grid_df <- expand.grid(
    turn = turns,
    intervention_id = as.character(defs$id),
    stringsAsFactors = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::left_join(
      defs |>
        dplyr::rename(intervention_id = id, intervention_label = label),
      by = "intervention_id"
    ) |>
    dplyr::mutate(active = FALSE)

  if (nrow(active_df) > 0) {
    active_keys <- paste(active_df$turn, active_df$intervention_id, sep = "||")
    grid_keys <- paste(grid_df$turn, grid_df$intervention_id, sep = "||")
    grid_df$active <- grid_keys %in% active_keys
  }

  grid_df
}

replay_plot_prevalence <- function(
    truth_log,
    obs_log,
    pop_tbl,
    window = 3L,
    baseline_truth_log = NULL,
    player_run_log = NULL,
    player_action_log = NULL,
    intervention_defs = NULL) {
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

  baseline_ts <- NULL
  if (!is.null(baseline_truth_log) && nrow(baseline_truth_log) > 0) {
    baseline_ts <- baseline_truth_log |>
      dplyr::group_by(turn) |>
      dplyr::summarise(truth_incidence = sum(incidence), .groups = "drop") |>
      dplyr::arrange(turn)

    baseline_active_total <- vapply(
      seq_len(nrow(baseline_ts)),
      function(i) {
        start <- max(1L, i - window + 1L)
        sum(baseline_ts$truth_incidence[start:i], na.rm = TRUE)
      },
      numeric(1)
    )
    baseline_ts$baseline_truth_prev_pct <- 100 * baseline_active_total / pop_total
    baseline_ts <- baseline_ts |>
      dplyr::select(turn, baseline_truth_prev_pct)
  }

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
  if (!is.null(baseline_ts)) {
    combined <- dplyr::full_join(combined, baseline_ts, by = "turn")
  } else {
    combined$baseline_truth_prev_pct <- NA_real_
  }
  if (nrow(combined) == 0) {
    return(NULL)
  }
  combined$truth_prev_pct[is.na(combined$truth_prev_pct)] <- 0
  combined$observed_prev_pct[is.na(combined$observed_prev_pct)] <- 0
  combined$baseline_truth_prev_pct[is.na(combined$baseline_truth_prev_pct)] <- 0

  legend_cols <- c(
    "Observed prevalence" = "#ef6548",
    "Truth prevalence (active)" = "#08519c",
    "Baseline truth prevalence (active)" = "#31a354"
  )

  if (nrow(combined) <= 1) {
    plot_obj <- ggplot2::ggplot(combined, ggplot2::aes(x = turn)) +
      ggplot2::geom_point(
        ggplot2::aes(y = truth_prev_pct, colour = "Truth prevalence (active)"),
        size = 2.4
      ) +
      ggplot2::geom_point(
        ggplot2::aes(y = observed_prev_pct, colour = "Observed prevalence"),
        size = 2.4,
        shape = 17
      ) +
      ggplot2::geom_point(
        ggplot2::aes(y = baseline_truth_prev_pct, colour = "Baseline truth prevalence (active)"),
        size = 2.4,
        shape = 15
      )
  } else {
    plot_obj <- ggplot2::ggplot(combined, ggplot2::aes(x = turn)) +
      ggplot2::geom_line(
        ggplot2::aes(y = truth_prev_pct, colour = "Truth prevalence (active)"),
        linewidth = 1
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = observed_prev_pct, colour = "Observed prevalence"),
        linewidth = 1,
        linetype = "dashed"
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = baseline_truth_prev_pct, colour = "Baseline truth prevalence (active)"),
        linewidth = 1,
        linetype = "dotdash"
      )
  }

  plot_obj <- plot_obj +
    ggplot2::labs(
      x = "Turn",
      y = "Prevalence (%)",
      colour = NULL
    ) +
    ggplot2::scale_colour_manual(values = legend_cols) +
    ggplot2::scale_x_continuous(
      breaks = scales::breaks_pretty(n = 8)
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10),
      legend.title = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 10),
      legend.key.size = grid::unit(0.45, "cm"),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.direction = "horizontal",
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.spacing.x = grid::unit(0.2, "cm")
    )

  plot_obj
}

replay_plot_intervention_timeline <- function(run_log, action_log, intervention_defs, turns = NULL) {
  if (is.null(turns)) {
    turn_values <- integer()
    if (!is.null(run_log) && is.data.frame(run_log) && "turn" %in% names(run_log)) {
      turn_values <- c(turn_values, run_log$turn)
    }
    if (!is.null(action_log) && is.data.frame(action_log) && "turn" %in% names(action_log)) {
      turn_values <- c(turn_values, action_log$turn)
    }
    turns <- turn_values
  }

  turns <- suppressWarnings(as.integer(turns))
  turns <- turns[is.finite(turns)]
  turns <- sort(unique(turns))
  if (length(turns) == 0) {
    return(NULL)
  }

  timeline <- replay_build_intervention_timeline(
    run_log = run_log,
    turns = turns,
    intervention_defs = intervention_defs,
    action_log = action_log
  )
  if (nrow(timeline) == 0) {
    return(NULL)
  }

  defs <- tibble::as_tibble(intervention_defs) |>
    dplyr::select(dplyr::any_of(c("id", "label"))) |>
    dplyr::filter(!is.na(id), !is.na(label), id != "", label != "") |>
    dplyr::distinct(id, label)
  row_order <- as.character(defs$id[defs$id %in% timeline$intervention_id])
  if (length(row_order) == 0) {
    row_order <- sort(unique(timeline$intervention_id))
  }

  label_order <- defs |>
    dplyr::filter(id %in% row_order) |>
    dplyr::pull(label)
  if (length(label_order) == 0) {
    label_order <- unique(timeline$intervention_label)
  }

  timeline <- timeline |>
    dplyr::mutate(
      intervention_id = factor(intervention_id, levels = row_order),
      intervention_label = factor(intervention_label, levels = rev(label_order)),
      strip_state = ifelse(active, "Active", "Inactive")
    )

  ggplot2::ggplot(
    timeline,
    ggplot2::aes(x = turn, y = intervention_label, fill = strip_state)
  ) +
    ggplot2::geom_tile(width = 0.9, height = 0.72) +
    ggplot2::labs(
      x = "Turn",
      y = NULL,
      fill = "Intervention status"
    ) +
    ggplot2::scale_x_continuous(
      breaks = scales::breaks_pretty(n = 8)
    ) +
    ggplot2::scale_fill_manual(
      values = c("Active" = "#4d4d4d", "Inactive" = "#ececec")
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = 10),
      axis.text.x = ggplot2::element_text(size = 9),
      axis.text.y = ggplot2::element_text(size = 9),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.direction = "horizontal",
      legend.title = ggplot2::element_text(size = 9),
      legend.text = ggplot2::element_text(size = 9),
      legend.key.size = grid::unit(0.4, "cm"),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      plot.margin = ggplot2::margin(2, 5.5, 2, 5.5)
    )
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
    ggplot2::scale_x_continuous(
      breaks = scales::breaks_pretty(n = 8)
    ) +
    ggplot2::scale_fill_manual(values = c("Antigen" = "#74c476", "PCR" = "#6baed6")) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 11),
      axis.text = ggplot2::element_text(size = 10),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.direction = "horizontal",
      legend.title = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 10),
      legend.key.size = grid::unit(0.45, "cm"),
      legend.margin = ggplot2::margin(0, 0, 0, 0)
    )
}
