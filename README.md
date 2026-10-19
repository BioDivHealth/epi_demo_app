# Outbreak London (boroughs)

An instructional Shiny application that simulates an emerging respiratory outbreak across London boroughs. The hidden process follows a spatial branching model over 33 boroughs and learners explore how delayed, noisy testing constrains situational awareness.

## Getting started

1. Restore the project library with [`renv`](https://rstudio.github.io/renv/):
   ```r
   install.packages("renv", repos = "https://cloud.r-project.org")
   renv::init()
   # or, if `renv.lock` already exists
   renv::restore()
   ```
2. Launch the app from the project root:
   ```r
   shiny::runApp()
   ```
3. Run the automated checks:
   ```r
   install.packages(c("devtools", "testthat"), repos = "https://cloud.r-project.org")
   devtools::test()
   ```

## Authentication (admin vs student)

This app uses in-memory login via `shinymanager`. Create a local credentials file
and keep it out of git.

1. Create `config/credentials.csv` with columns `user`, `password`, `admin`:
   ```csv
   user,password,admin
   admin,ChangeMe,TRUE
   student,ClassPass,FALSE
   ```
2. (Optional) point to a different file path using:
   ```bash
   export EPILONDON_AUTH_FILE="/path/to/credentials.csv"
   ```
3. Start the app. Admin users will see the admin-only controls and can reset all
   sessions or update global parameters.

For automated checks or local demos, you can disable auth entirely:
```bash
export EPILONDON_AUTH_DISABLE="true"
```

Note: this is intended for classroom use. If you need stronger protection, put
the app behind basic auth or host it on a platform with access control.

## Deployment (AWS Lightsail)

For a one-day demo or class, a small Ubuntu 22.04 Lightsail instance is a good fit. The canonical command sequence lives in `.cmds`.

1. Create an Ubuntu 22.04 instance, attach a Static IP, and open TCP port `3838`.
2. Install R 4.5.x from CRAN and required system packages:
   ```bash
   sudo apt update
   sudo apt install -y --no-install-recommends software-properties-common dirmngr
   wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc \
     | sudo tee /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
   sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu jammy-cran40/"
   sudo apt update
   sudo apt install -y r-base
   sudo apt install -y \
     gdebi-core cmake libabsl-dev gdal-bin pandoc \
     libssl-dev libcurl4-openssl-dev libxml2-dev \
     libgdal-dev libproj-dev libgeos-dev libudunits2-dev
   ```
3. Install Shiny Server:
   ```bash
   wget https://download3.rstudio.org/ubuntu-20.04/x86_64/shiny-server-1.5.23.1030-amd64.deb
   sudo gdebi -n shiny-server-1.5.23.1030-amd64.deb
   ```
4. Upload the app (use `.rsyncignore`; do **not** exclude `renv/activate.R`):
   ```bash
   rsync -av --exclude-from='.rsyncignore' \
     ./ ubuntu@SERVER_IP:/home/ubuntu/EpiAppLondon/
   ```
5. Move into the Shiny Server directory and restore packages with renv:
   ```bash
   sudo mv /home/ubuntu/EpiAppLondon /srv/shiny-server/
   sudo chown -R shiny:shiny /srv/shiny-server/EpiAppLondon
   sudo R -q -e 'install.packages("renv", repos="https://cloud.r-project.org")'
   sudo -u shiny R -q -e 'setwd("/srv/shiny-server/EpiAppLondon"); source("renv/activate.R"); Sys.setenv(MAKEFLAGS="-j1"); renv::restore()'
   ```
6. Open the app at: `http://<PUBLIC_IP>:3838/EpiAppLondon/`

Notes:
- If `terra`/`s2` fail to compile on small instances, add swap (see `.cmds`).
- Logs live under `/var/log/shiny-server/`.
- `.rsconnectignore` is used for shinyapps.io bundling; `.rsyncignore` is for Lightsail uploads.

## Gameplay overview

- Allocate antigen vs. PCR tests per borough each turn under a shared budget.
- Observe only results that have returned; unsampled or pending boroughs remain grey on the map (fog of war).
- Dashboards summarise observed incidence, quick Rt estimates (parametric serial interval), pending queues, spend, and positivity.
- Toggle from the default setup into a custom scenario to tweak hidden dynamics, starter intel, and per-turn interventions (distancing, quarantine, rapid labs).
- Choose **starter intel** options to begin with some initial testing allocation (alert or sweep).
- Activate **interventions** per turn: social distancing (reduces Rt), targeted quarantine (reduces imports), and rapid labs (shortens PCR delay).
- Ending a run shows summary scoring metrics: time to first detection, peak true incidence, peak observed positives, overall positivity, cumulative spend, and a calibration gap between inferred and true prevalence.
- The end-of-run modal includes a turn-by-turn table comparing true incidence with observed results, allocations, and interventions.
- After ending, a **replay viewer** unlocks, allowing you to scrub or animate through turns and compare the hidden truth side-by-side with what was observed.

## Project structure

```
├── app.R                  # Main Shiny app entry point
├── R/
│   ├── mod_truth_engine.R # Hidden branching process (step_truth)
│   ├── mod_observation.R  # Testing model, delays, queues (observe_tests)
│   ├── mod_map.R          # Leaflet allocation UI
│   ├── mod_controls.R     # Sliders, toggles, budget controls
│   ├── mod_dashboard.R    # Incidence/Rt/queues/spend displays
│   ├── mod_replay.R       # Turn-by-turn replay viewer
│   ├── interventions.R    # Intervention definitions & effects
│   ├── starter_intel.R    # Starter intel allocation helpers
│   ├── scoring.R          # End-of-run metrics (score_run, build_run_report)
│   ├── util_io.R          # Data loaders (load_inputs)
│   └── util_logging.R     # Turn-by-turn logging utilities
├── tests/testthat/        # Unit & smoke tests
├── data/                  # Precomputed read-only inputs
└── AGENTS.md              # Instructions for AI coding agents
```

## Data inputs

The app reads six precomputed RDS files located in `./data/`:

| File | Description |
|------|-------------|
| `boroughs.rds` | sf polygons for 33 London boroughs (WGS84, leaflet-ready) |
| `boroughs_planar.rds` | sf polygons in planar CRS (EPSG:27700) for adjacency math |
| `adjacency_nb.rds` | `spdep::nb` queen-contiguity neighbours |
| `adjacency_matrix.rds` | Binary N×N adjacency matrix |
| `kernel_K.rds` | Row-normalised spatial kernel (each row sums to 1) |
| `pop_boroughs.rds` | Borough populations (`gss_code`, `borough_name`, `pop`) |

These originate from Greater London Authority and ONS sources and must **not** be modified or regenerated by the app. If tests require fixtures, create them under `tests/fixtures/` instead of writing into `data/`.

### Generating inputs for a new location

`R/build_location_inputs.R` builds the same bundle for any polygons (e.g. Natural Earth admin-1 units), without touching `data/`:

```r
source("R/build_location_inputs.R")
inputs <- build_location_inputs(units_sf, pop = pop_tbl, name_col = "name",
                                gravity = list(alpha = 1, beta = 2))
save_location_inputs(inputs, "locations/my_country")
```

- `K` is a **gravity kernel**: `K_ij ∝ pop_j^alpha / d_ij^beta` between unit centroids, with a within-unit distance `(2/3)·sqrt(area/π)` on the diagonal, row-normalised to 1. (London's `kernel_K.rds` is plain queen adjacency; it is unchanged.)
- Queen adjacency (`nb`, `A`) is built exactly as in `prepare_data.R`.
- `pop` is a numeric vector in unit order, or a data frame with `pop` and a name column. Natural Earth admin-1 has **no population**; sum a WorldPop or GHS-POP raster over each polygon, or use census tables.
- Worked example (Portugal, plus a map of one unit's kernel row to sense-check the weights): `source("examples/build_new_location.R")`.
- `load_inputs()` still requires 33 boroughs, so the app itself stays London-only for now.

## Tests

The test suite covers:

- `test_adjacency.R` — Validates adjacency matrix and kernel properties
- `test_build_location_inputs.R` — New-location bundle: K square, rows sum to 1, population aligned, nearer/bigger units weighted more
- `test_branching.R` — Verifies `step_truth()` produces valid incidence
- `test_observation.R` — Tests observation model and delay queues
- `test_interventions.R` — Checks intervention effect calculations
- `test_starter_intel.R` — Tests starter intel allocation logic
- `test_logging.R` — Validates turn logging utilities
- `test_reporting.R` — Tests run report generation
- `test_app.R` — Smoke test that the app launches
