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
      "Your goal is to allocate testing under budget and delay constraints",
      "to infer how the outbreak is evolving, then compare those inferences",
      "with the truth reveal. You can delegate specific tests to chosen",
      "boroughs manually, or use mass testing so the remaining budget is",
      "spread evenly across boroughs."
    ),
    shiny::tags$p(
      "Choose between PCR and antigen tests, and use the Tests per click",
      "slider to set how many tests each manual allocation applies. You can",
      "also activate interventions. Some work quickly but fade into a weaker",
      "steady effect (for example Social distancing), some need a warm-up but",
      "stay useful over many turns (for example Rapid labs and Ventilation),",
      "and some work best only when you are actually finding cases (Case",
      "isolation and tracing). Selected interventions consume a share of the",
      "turn budget while active, so spending more on interventions leaves less",
      "budget for testing coverage.",
      "Use the questions below as a guide for interpreting what you see and",
      "comparing it with the replay."
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
    shiny::tags$h5("What you might think of during play"),
    shiny::tags$ul(
      shiny::tags$li("How is your testing strategy evolving (targeted vs broad, PCR vs antigen), and why?"),
      shiny::tags$li("What do observed positivity and the estimated Rt suggest about the trajectory, given delays?"),
      shiny::tags$li("Where do results appear on the map, and what might be missing or delayed?")
    ),
    shiny::tags$h5("Reflective questions after reveal"),
    shiny::tags$ul(
      shiny::tags$li("How does observed positivity compare with true prevalence in the replay?"),
      shiny::tags$li("When did sampling bias or delays distort your inference?"),
      shiny::tags$li("When did intervention choices improve trajectory but reduce detection coverage?"),
      shiny::tags$li("How did your intervention spend compare with your baseline comparator?"),
      shiny::tags$li("What general testing strategy would you adjust next time?")
    )
  )
}
