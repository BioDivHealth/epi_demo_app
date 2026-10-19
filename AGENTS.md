# AGENTS.md

A guide for coding agents (e.g., OpenAI Codex) working on **Outbreak London (boroughs)** — a teaching-oriented R/Shiny app that simulates an early-phase respiratory outbreak across London boroughs using a spatial branching process. Students allocate testing under budget and delay constraints and see only sampled results (“fog of war”).

---
## Here are the main guidelines for working on this project:

- [Data Protection](docs/rules/data-protection.md)
- [Epidemiological Model](docs/rules/epidemiological-model.md) - Epidemiological model equations and observation logic for the outbreak simulation
- [Lightsail Deploy](docs/rules/lightsail-deploy.md) - Lightsail deployment checklist and renv notes
- [R Shiny Conventions](docs/rules/r-shiny-conventions.md)
- [Testing Conventions](docs/rules/testing-conventions.md)


## What success looks like
- A **playable** Shiny app launched with `shiny::runApp()` that:
  - Advances one turn per click; the hidden process uses a branching model with a **row-normalised kernel K** over 33 boroughs.
  - Lets users allocate **PCR vs Antigen** tests per borough; respects **sensitivity/specificity** and **turnaround delays**.
  - Displays **observed** incidence and **Rt** from *returned results only*; unsampled boroughs remain grey.
- Tests pass (`devtools::test()`), core functions are **pure**, and we **never overwrite `data/*.rds`**.

---

## Repo map (important directories)
- `R/` — Shiny modules & helpers:
  - `mod_help.R` (welcome/help modal copy)
  - `mod_truth_engine.R` (hidden branching process)
  - `mod_observation.R` (testing model, delays, budget)
  - `mod_map.R` (leaflet allocation UI)
  - `mod_controls.R` (sliders, toggles, budget)
  - `mod_dashboard.R` (incidence/Rt/queues/spend)
  - `mod_replay.R` (post-run replay viewer)
  - `interventions.R` (intervention definitions & effects)
  - `scenarios.R` (scenario preset catalog)
  - `scoring.R` (end-of-run metrics)
  - `starter_intel.R` (starter allocation helpers)
  - `util_io.R` (loads inputs from `data/`)
  - `util_logging.R` (turn logging utilities)
  - `util_replay.R` (replay state helpers)
- `app.R` — wires modules into the UI.
- `tests/testthat/` — unit & smoke tests.
- `data/` — **precomputed, read-only inputs** (see below).

---

## Data files (read-only)
These **already exist** and must be **read** (not regenerated). Do **not** write to `data/`.

- `data/boroughs.rds` — sf polygons for the 33 London boroughs (WGS84; leaflet-ready).
- `data/boroughs_planar.rds` — sf polygons in a planar CRS (EPSG:27700) for adjacency math.
- `data/adjacency_nb.rds` — `spdep::nb` queen-contiguity neighbours.
- `data/adjacency_matrix.rds` — binary N×N adjacency derived from `nb`.
- `data/kernel_K.rds` — **row-normalised** spatial kernel (each row sums to 1).
- `data/kernel_K_measles.rds` — **row-normalised** measles kernel variant (lower mobility).
- `data/pop_boroughs.rds` — `gss_code, borough_name, pop` table.

**Guardrail:** Never modify or overwrite these files. If you need mock data for tests, create fixtures in `tests/fixtures/`.

## Setup & run commands (execute in this order)

**System assumptions:** R ≥ 4.3; internet access only to install CRAN packages. No external data downloads during runtime.

```r
# 1) Open R at repo root
install.packages("renv", repos = "https://cloud.r-project.org")
if (!requireNamespace("renv", quietly = TRUE)) stop("renv failed to install")
if (!file.exists("renv.lock")) renv::init() else renv::restore()

# 2) Run the app (manual)
shiny::runApp()

# 3) Run tests
install.packages(c("devtools","testthat"), repos = "https://cloud.r-project.org")
devtools::test()
```

## Core model (terms the code should respect)

- State per borough *i*, turn *t*:
  Use a branching process with overdispersion:
  - I_{i,t+1} ~ NegBin(mean = mu_{i,t}, size = k)
  - mu_{i,t} = R_t * sum_j K[j, i] * I_{j,t}   (crossprod(K, I) * R_t)
  where:
  - Row j of K = where borough j's onward infections land; rows sum to 1, so each case causes R_t new cases wherever it is. Built in load_inputs() from data/kernel_K.rds plus a within-borough share.
  - k controls overdispersion (superspreading).

  Imports: allow small stochastic introductions each turn. Example (pseudocode):
      gate <- runif(length(I_t)) < imports_prob
      imports <- ifelse(gate, rpois(length(I_t), lambda_import), 0L)
      I_next <- I_next + imports

- Observation model (what students "see"):
  Given allocations n_{i,t}^{PCR} and n_{i,t}^{Ag} and a prevalence proxy p_{i,t} = I_{i,t} / pop_i,
  compute effective positivity with test performance:
  - p_star = Se * p + (1 - Sp) * (1 - p)
  Then draw observed positives:
  - X ~ Binomial(n, p_star)

  Maintain per-test delay queues (integer turns). Antigen typically short delay; PCR longer.
  Only results dequeued this turn are visible in the UI (unsampled/undelivered boroughs remain grey).

- Analytics shown to students (from observed data only):
  - Build incidence over turns using only returned results.
  - Estimate R_t from the observed time series (fixed serial-interval assumption).
  - Do not leak hidden truth (e.g., true I_t or prevalence) into any observed panel or score mid-run.


---

## Coding style & conventions
- **R style:** tidyverse-friendly; snake_case; pure functions for math; side effects only in Shiny server/reactives.
- **Module boundaries:**
  - `step_truth()` is pure and deterministic under a fixed RNG seed.
  - `observe_tests()` calculates draws and updates queues; returns a tidy tibble for the turn.
- **Plotting:** `ggplot2`; mapping via `leaflet` with `sf` geometries.
- **Randomness:** seed via `set.seed()` on app start; provide a UI control to reseed.

## Tasks you may perform (✅ allowed)
- Create/modify R files under `R/`, `app.R`, and tests.
- Read `.rds` from `data/` using `readRDS()`.
- Add missing lightweight deps to `DESCRIPTION` or `renv`.
- Write small fixtures to `tests/fixtures/` (never `data/`).

## Tasks that need explicit approval (⚠️ ask first)
- Changing model equations or kernel semantics.
- Adding heavy dependencies or non-CRAN repos.
- Network calls beyond CRAN installs.
- Any change that writes to or deletes files in `data/`.

## Tasks you must not do (⛔)
- Overwrite or delete **any** file in `data/`.
- Leak hidden truth into observed outputs.
- Commit secrets, tokens, or user data.

## Test plan (what “green” means)
- `test_adjacency.R` — A and K are square; `rowSums(K) == 1`; `length(nb) == 33`.
- `test_branching.R` — With a fixed seed, `step_truth()` returns non-negative integers; mean growth tracks \(R_t\).
- `test_observation.R` — \(p^\*\) behaves with extremes (Se/Sp in {0,1}); delays respected (queues advance correctly).
- `test_interventions.R` — Intervention effects compute correctly.
- `test_starter_intel.R` — Starter intel allocations are valid.
- `test_logging.R` — Turn logging utilities work correctly.
- `test_reporting.R` — Run report generation works.
- `test_app.R` — app launches; key UI elements exist (map, controls, Advance button).

## UI expectations
- **Map:** Boroughs grey unless results are **available this turn**; choropleth by **observed positivity** for available ones.
- **Controls:** scenario mode toggle (Default vs Custom), sliders for `Rt`, `k`, imports, Se/Sp, delays, per-test cost, starter intel chooser, intervention checkboxes, and total budget; buttons: **Advance**, **End simulation**, **Reset**.
- **Dashboard:** observed incidence, estimated Rt (fixed serial interval), queues (pending PCR/Ag), spend & % positive.
- **Auth/Admin:** login gate via `shinymanager`; admin users can change Rt/k/imports/seed values in the controls panel.

## Auth/Admin flow (shinymanager)
- Auth is enabled by default; disable with `EPILONDON_AUTH_DISABLE=true`.
- Credentials are read from `config/credentials.csv` (override with `EPILONDON_AUTH_FILE`).
- Admin users are those with `admin=TRUE` in the credentials file.
- `app.R` wraps the UI with `secure_app()` and initializes `secure_server()`, then passes `is_admin` into `mod_controls_server()` to show admin controls.

## Common pitfalls & how to avoid them
- **Wrong CRS on leaflet:** always render `boroughs.rds` (WGS84); keep `boroughs_planar.rds` for adjacency math only.
- **Kernel drift:** ensure each row of K sums to 1; re-normalise after any edits to A.
- **Truth leakage:** never compute Rt or positivity from hidden states; only from **returned** results.
- **Zero prevalence edge cases:** add a small floor before \(p\) when computing \(p^\*\) to avoid degenerate all-zero binomials at very low prevalence.

## Scenario presets (optional)
- **Single-seed:** one borough seeded at t=0.
- **Multi-seed:** 2–3 boroughs seeded at low rate for first 3 turns.
- **Low prevalence + low Se:** stress false positives and sampling bias.
- **Measles variants:** scenario presets can swap kernels and susceptible fractions (see `R/scenarios.R`).

## Commands the agent may run
```sh
# Install/restore
R -q -e 'renv::restore()'

# Launch Shiny (manual testing)
R -q -e 'shiny::runApp()'

# Run tests
R -q -e 'devtools::test()'
```
