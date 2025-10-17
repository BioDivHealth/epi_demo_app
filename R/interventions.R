# Intervention helpers -------------------------------------------------------

intervention_catalog <- function() {
  tibble::tibble(
    id = c("distancing", "quarantine", "rapid"),
    label = c("Social distancing", "Targeted quarantine", "Rapid labs"),
    cost = c(30, 25, 40),
    Rt_multiplier = c(0.8, 1, 1),
    imports_multiplier = c(1, 0.5, 1),
    ag_delay_adjust = c(0, 0, 0),
    pcr_delay_adjust = c(0, 0, -1)
  )
}

compute_intervention_effects <- function(selected, catalog = intervention_catalog()) {
  if (length(selected) == 0) {
    return(list(
      cost = 0,
      Rt_multiplier = 1,
      imports_multiplier = 1,
      delay_adjust = list(ag = 0, pcr = 0),
      labels = character()
    ))
  }

  defs <- catalog[catalog$id %in% selected, , drop = FALSE]
  if (nrow(defs) == 0) {
    return(list(
      cost = 0,
      Rt_multiplier = 1,
      imports_multiplier = 1,
      delay_adjust = list(ag = 0, pcr = 0),
      labels = character()
    ))
  }

  list(
    cost = sum(defs$cost),
    Rt_multiplier = prod(defs$Rt_multiplier),
    imports_multiplier = prod(defs$imports_multiplier),
    delay_adjust = list(
      ag = sum(defs$ag_delay_adjust),
      pcr = sum(defs$pcr_delay_adjust)
    ),
    labels = defs$label
  )
}
