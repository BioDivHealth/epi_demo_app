<!-- Generated from cursor rule 'data-protection' -->

# Data Protection Rules

## Read-only data files
The `data/` directory contains precomputed inputs that must **never** be modified:

- [boroughs.rds](../../../../data/boroughs.rds) — sf polygons (WGS84) for leaflet
- [boroughs planar.rds](../../../../data/boroughs_planar.rds) — sf polygons (EPSG:27700) for adjacency math
- [adjacency nb.rds](../../../../data/adjacency_nb.rds) — `spdep::nb` queen-contiguity neighbours
- [adjacency matrix.rds](../../../../data/adjacency_matrix.rds) — Binary N×N adjacency
- [kernel K.rds](../../../../data/kernel_K.rds) — Row-normalised spatial kernel (rows sum to 1)
- [kernel K measles.rds](../../../../data/kernel_K_measles.rds) — Measles kernel variant (rows sum to 1)
- [pop boroughs.rds](../../../../data/pop_boroughs.rds) — `gss_code`, `borough_name`, `pop`

## Critical constraints
- ⛔ **Never** overwrite, delete, or regenerate files in `data/`
- ⛔ **Never** leak hidden truth (true incidence, prevalence) into observed outputs
- ✅ Read data with `readRDS()` via [util io.R](../../../../R/util_io.R)
- ✅ Create test fixtures in `tests/fixtures/`, not `data/`

## CRS handling
- Use `boroughs.rds` (WGS84) for leaflet map rendering
- Use `boroughs_planar.rds` (EPSG:27700) only for adjacency calculations
