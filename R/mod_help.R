# Help modal -----------------------------------------------------------------

student_scenario_display <- function(scenario_id = NULL, show_scenario = FALSE, scenario_defs = scenario_catalog()) {
  hidden_display <- list(
    visible = FALSE,
    header_text = "",
    label = NULL,
    objective = NULL
  )

  if (!isTRUE(show_scenario)) {
    return(hidden_display)
  }

  if (is.null(scenario_id) || is.na(scenario_id) || scenario_id == "") {
    scenario_id <- "custom"
  }

  current <- scenario_lookup(scenario_id, scenario_defs)
  if (identical(current$id[[1]], "custom")) {
    return(hidden_display)
  }

  label <- current$label[[1]]
  if (is.null(label) || is.na(label) || identical(label, "")) {
    return(hidden_display)
  }

  objective <- current$objective[[1]]
  if (is.null(objective) || is.na(objective) || identical(objective, "")) {
    objective <- NULL
  }

  list(
    visible = TRUE,
    header_text = paste("Scenario:", label),
    label = label,
    objective = objective
  )
}

help_modal_body <- function(scenario_id = NULL, show_scenario = FALSE, scenario_defs = scenario_catalog()) {
  scenario_display <- student_scenario_display(
    scenario_id = scenario_id,
    show_scenario = show_scenario,
    scenario_defs = scenario_defs
  )

  shiny::tagList(
    shiny::tags$p(
      "Your goal is to use testing under budget and delay constraints to",
      "infer how the outbreak is evolving, then compare your interpretation",
      "with the reveal at the end."
    ),
    shiny::tags$p(
      "Auto mode is the default. Start by choosing a testing pool, a borough",
      "policy, and any interventions, then set the number of auto turns and",
      "press Start or Run Batch. In manual mode, you can switch test type,",
      "set Tests per click, and allocate tests directly by clicking boroughs",
      "before advancing the turn."
    ),
    shiny::tags$p(
      "Interventions use part of the turn budget while active, so stronger",
      "control usually means less testing coverage. Returned results, not the",
      "hidden outbreak, drive the map and dashboard."
    ),
    if (isTRUE(scenario_display$visible)) {
      shiny::tagList(
        shiny::tags$h5("Current scenario"),
        shiny::tags$div(
          class = "mb-3",
          shiny::tags$div(shiny::tags$strong(scenario_display$label)),
          if (!is.null(scenario_display$objective)) {
            shiny::tags$div(class = "text-muted small", scenario_display$objective)
          }
        ),
        shiny::tags$hr()
      )
    },
    shiny::tags$h5("How to read the game"),
    shiny::tags$ul(
      shiny::tags$li("Observed positivity and estimated Rt come from returned results only, so delays and sparse coverage can mislead you."),
      shiny::tags$li("Broad auto policies help you build coverage quickly; manual targeting is more useful when you want to investigate or concentrate resources."),
      shiny::tags$li("Grey or quiet areas may mean unsampled or not-yet-returned results, not necessarily low transmission.")
    ),
    shiny::tags$h5("Questions for the reveal"),
    shiny::tags$ul(
      shiny::tags$li("When did your observed signal match the underlying outbreak, and when did delays or coverage gaps distort it?"),
      shiny::tags$li("How did your balance between testing coverage and interventions shape what you noticed and what you missed?"),
      shiny::tags$li("What would you change next time: the testing pool, borough policy, intervention mix, or a switch into manual targeting?")
    )
  )
}
