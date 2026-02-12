<!-- Generated from cursor rule 'r-shiny-conventions' -->

# R/Shiny Coding Conventions

## Style
- Use tidyverse-friendly patterns: `dplyr`, `tibble`, `ggplot2`
- Naming: `snake_case` for functions and variables
- Pure functions for all math/logic; side effects only in Shiny server/reactives

## Module structure
- UI functions: `mod_*_ui(id)`
- Server functions: `mod_*_server(id, ...)` using `shiny::moduleServer()`
- Always namespace inputs with `ns <- shiny::NS(id)` in UI functions
- Auth gating and admin role checks are configured in `app.R` and passed into modules as needed (e.g., `is_admin` for `mod_controls_server()`).

## Key modules in this project
- [mod truth engine.R](../../../../R/mod_truth_engine.R) — `init_truth()`, `step_truth()` (pure, deterministic under fixed RNG seed)
- [mod observation.R](../../../../R/mod_observation.R) — `observe_tests()` returns available results and updated queues
- [mod map.R](../../../../R/mod_map.R) — Leaflet-based allocation interface
- [mod controls.R](../../../../R/mod_controls.R) — Parameter controls (epidemiology, testing, budget)
- [mod dashboard.R](../../../../R/mod_dashboard.R) — Displays incidence, Rt, queues, spend
- [mod replay.R](../../../../R/mod_replay.R) — Post-run turn-by-turn replay viewer
- [scenarios.R](../../../../R/scenarios.R) — Scenario preset catalog (e.g., measles variants)
- [starter intel.R](../../../../R/starter_intel.R) — Starter allocation helpers
- [scoring.R](../../../../R/scoring.R) — End-of-run metrics and reports
- [util io.R](../../../../R/util_io.R) — Input data loaders
- [util logging.R](../../../../R/util_logging.R) — Turn logging utilities
- [util replay.R](../../../../R/util_replay.R) — Replay state helpers

## Auth/Admin flow
- `shinymanager::secure_app()` wraps the top-level UI in `app.R`.
- `shinymanager::secure_server()` handles login; admin status is derived from the credentials file and exposed via a reactive `is_admin`.
- Credentials default to `config/credentials.csv` (override with `EPILONDON_AUTH_FILE`), and auth can be disabled with `EPILONDON_AUTH_DISABLE=true`.

## Plotting
- Use `ggplot2` for charts
- Use `leaflet` with `sf` geometries for maps
- Always use WGS84 (`boroughs.rds`) for leaflet rendering

## Randomness
- Seed via `set.seed()` on app start
- UI provides reseed control
