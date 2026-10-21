# Intervention helpers -------------------------------------------------------
#
# Costs, effects, lag and fatigue are set in teaching_params().

intervention_catalog <- function() {
  teaching_params()$interventions
}

# How strong is an intervention after it has been on for `streak` turns?
# Returns a number between 0 (no effect yet) and 1 (full effect).
intervention_strength <- function(streak, lag, fatigue) {
  # still in the lag period: no effect yet
  if (streak <= lag) {
    return(0)
  }

  # first effective turn is full strength, then it fades
  turns_effective <- streak - lag
  fatigue^(turns_effective - 1)
}

# Look up one intervention's streak, treating missing as 0
get_streak <- function(streaks, id) {
  if (is.null(streaks) || !(id %in% names(streaks))) {
    return(0L)
  }
  as.integer(streaks[[id]])
}

# Update how long each intervention has been running (its streak). A measure
# that is switched off loses one report of streak per report off, so the
# public's patience comes back gradually: switching off for one week does
# not reset the fading.
update_intervention_streaks <- function(streaks, selected, catalog = intervention_catalog()) {
  new_streaks <- stats::setNames(integer(nrow(catalog)), catalog$id)

  for (id in catalog$id) {
    previous <- get_streak(streaks, id)
    if (id %in% selected) {
      new_streaks[[id]] <- previous + 1L
    } else {
      new_streaks[[id]] <- max(0L, previous - 1L)
    }
  }

  new_streaks
}

# Combine the selected interventions into one set of effects.
# `streaks` is a named integer vector of consecutive turns switched on
# (including this one). If NULL, every selected intervention is treated as
# already past its lag and at full strength.
compute_intervention_effects <- function(selected, catalog = intervention_catalog(), streaks = NULL) {
  neutral <- list(
    cost = 0,
    Rt_multiplier = 1,
    imports_multiplier = 1,
    delay_adjust = list(ag = 0, pcr = 0),
    labels = character(),
    strength = numeric()
  )

  if (length(selected) == 0) {
    return(neutral)
  }

  defs <- catalog[catalog$id %in% selected, , drop = FALSE]
  if (nrow(defs) == 0) {
    return(neutral)
  }

  # strength of each selected intervention this turn
  strength <- vapply(seq_len(nrow(defs)), function(i) {
    if (is.null(streaks)) {
      return(1)
    }
    streak <- get_streak(streaks, defs$id[i])
    intervention_strength(streak, defs$lag[i], defs$fatigue[i])
  }, numeric(1))

  # scale each effect towards "no effect" by its strength
  rt_mult <- 1 - strength * (1 - defs$Rt_multiplier)
  imports_mult <- 1 - strength * (1 - defs$imports_multiplier)

  # delay changes are whole turns, so they are either on or off
  delay_on <- strength > 0

  list(
    cost = sum(defs$cost),
    Rt_multiplier = prod(rt_mult),
    imports_multiplier = prod(imports_mult),
    delay_adjust = list(
      ag = sum(defs$ag_delay_adjust[delay_on]),
      pcr = sum(defs$pcr_delay_adjust[delay_on])
    ),
    labels = defs$label,
    strength = stats::setNames(strength, defs$id)
  )
}
