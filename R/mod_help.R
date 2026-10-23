# Help modal -----------------------------------------------------------------

help_modal_body <- function(scenario_id = NULL, scenario_defs = scenario_catalog(), show_scenario = TRUE) {
  if (is.null(scenario_id) || is.na(scenario_id) || scenario_id == "") {
    scenario_id <- "custom"
  }

  current <- scenario_lookup(scenario_id, scenario_defs)
  current_label <- current$label[[1]]
  current_objective <- current$objective[[1]]

  shiny::tagList(
    shiny::tags$h5("How to play"),
    shiny::tags$ol(
      shiny::tags$li("Set your response below the map: who to test, where, which test and how much to spend, plus any control measures."),
      shiny::tags$li("Press Next report. Your plan carries on each report until you change it."),
      shiny::tags$li("Read what came back: positives, tests, hospital reports. Ask what they really tell you."),
      shiny::tags$li(sprintf("When time runs out (after %d reports, or earlier) press End & review to see what really happened, then Replay this outbreak (in the Game menu) to try another strategy.", teaching_params()$game_turns))
    ),
    shiny::tags$details(
      shiny::tags$summary("More detail"),
    shiny::tags$p(
      "Your goal is to allocate testing under budget and delay constraints",
      "to infer how the outbreak is evolving, then compare those inferences",
      "with the truth reveal. You can delegate specific tests to chosen",
      "boroughs manually, or use mass testing so the remaining budget is",
      "spread evenly across boroughs."
    ),
    shiny::tags$p(
      "Set your response below the map: who to test (people with symptoms,",
      "or a random sample of residents), where (everywhere, boroughs with",
      "positive results so far, or boroughs you choose by clicking the map),",
      "which test, and how much to spend each turn. The plan carries on each",
      "turn until you change it. Switch the map to Restrict a borough to put",
      "local restrictions on a borough. You can also use London-wide Social",
      "distancing (lower Rt), Contact tracing (follows up your positives and",
      "the cases hospitals report), and Build better testing (faster,",
      "cheaper tests after a few turns, at half capacity meanwhile). Measures",
      "are charged every turn until switched off, only start working the turn",
      "after you switch them on, and some lose effect the longer they run. You",
      "have one budget for the whole outbreak. Hospital admissions, deaths, how",
      "full hospitals are and the lab backlog are reported whatever you do,",
      "but admissions are late and not always where the patient lives.",
      "Use the questions below as a guide for interpreting what you see and",
      "comparing it with the replay."
    ),
    ),
    shiny::tags$h5("Current scenario"),
    if (isTRUE(show_scenario)) {
      shiny::tags$div(
        class = "mb-3",
        shiny::tags$div(shiny::tags$strong(current_label)),
        if (!is.null(current_objective) && !is.na(current_objective) &&
            !identical(current_objective, "")) {
          shiny::tags$div(class = "text-muted small", current_objective)
        }
      )
    } else {
      shiny::tags$div(
        class = "mb-3 text-muted",
        "Hidden. Part of your job is to work out what kind of outbreak this is from the data."
      )
    },
    shiny::tags$h5("Be careful: the data can mislead you"),
    shiny::tags$ul(
      shiny::tags$li("Early alerts are not always right."),
      shiny::tags$li("A cheap test that is slightly wrong, used on many uninfected people, produces a lot of false positives."),
      shiny::tags$li("Symptomatic testing finds more cases, but who comes forward for a test, and what else is making people ill, both change what you see."),
      shiny::tags$li("Money spent on interventions is money not spent on seeing the outbreak.")
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
      shiny::tags$li("Which of your 'hotspots' were real, and which were false positives or a borough that tests more?"),
      shiny::tags$li("Did positivity fall because infections fell, or because more uninfected people were being tested?"),
      shiny::tags$li("What general testing strategy would you adjust next time?")
    )
  )
}
