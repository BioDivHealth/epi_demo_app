# Host admin store ------------------------------------------------------------

host_admin_defaults <- function() {
  scenario_defs <- scenario_catalog()
  default_scenario <- scenario_lookup("custom", scenario_defs)

  list(
    Rt = default_scenario$Rt[[1]],
    k = default_scenario$k[[1]],
    imports_prob = default_scenario$imports_prob[[1]],
    lambda_import = default_scenario$lambda_import[[1]],
    seed_borough = default_scenario$seed_borough[[1]],
    seed_cases = default_scenario$seed_cases[[1]],
    kernel_key = default_scenario$kernel_key[[1]],
    susceptible_frac = default_scenario$susceptible_frac[[1]],
    scenario_id = default_scenario$id[[1]],
    show_scenario = FALSE,
    death_ifr = 0.002,
    death_delay = 6L
  )
}

create_host_admin_store <- function(defaults = host_admin_defaults()) {
  fields <- names(defaults)
  state_args <- defaults
  state <- do.call(shiny::reactiveValues, state_args)
  reset_counter <- shiny::reactiveVal(0L)

  list(
    fields = fields,
    state = state,
    reset_counter = reset_counter
  )
}

host_admin_snapshot <- function(state, fields = NULL) {
  if (is.null(fields)) {
    fields <- names(shiny::reactiveValuesToList(state, all.names = TRUE))
  }

  out <- vector("list", length(fields))
  names(out) <- fields
  for (field in fields) {
    out[[field]] <- shiny::isolate(state[[field]])
  }
  out
}

trigger_host_reset <- function(reset_counter) {
  next_value <- shiny::isolate(reset_counter()) + 1L
  reset_counter(next_value)
  invisible(next_value)
}
