# Help modal -----------------------------------------------------------------

help_modal_body <- function(scenario_id = NULL, scenario_defs = scenario_catalog()) {
  if (is.null(scenario_id) || is.na(scenario_id) || scenario_id == "") {
    scenario_id <- "custom"
  }

  current <- scenario_lookup(scenario_id, scenario_defs)
  current_label <- current$label[[1]]
  current_objective <- current$objective[[1]]

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
      "also activate interventions: Social distancing (lower Rt), Targeted",
      "quarantine (reduce imports), and Rapid labs (shorter PCR turnaround).",
      "Selected interventions consume a share of the turn budget while active,",
      "so spending more on interventions leaves less budget for testing coverage.",
      "Use the questions below as a guide for interpreting what you see and",
      "comparing it with the replay."
    ),
    shiny::tags$h5("Current scenario"),
    shiny::tags$div(
      class = "mb-3",
      shiny::tags$div(shiny::tags$strong(current_label)),
      if (!is.null(current_objective) && !is.na(current_objective) &&
          !identical(current_objective, "")) {
        shiny::tags$div(class = "text-muted small", current_objective)
      }
    ),
    shiny::tags$hr(),
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
