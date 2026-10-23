# Build the browser version of the game (Shinylive) -----------------------------
#
# Shinylive runs R inside the web browser (webR), so the game becomes a set of
# plain files: anyone can play it from a link, on any computer, with nothing
# installed, and there is no server to fall over. Those files can be put on
# GitHub Pages (or any static host), or served locally for testing.
#
# Usage, from the repo root:
#   Rscript --vanilla shinylive/build_site.R <output_folder>
#
# Build into a folder OUTSIDE OneDrive: OneDrive drops desktop.ini files into
# the package folders and shinylive::export() then fails.
#
# app.R, R/ and data/ are not edited. A copy is staged in a temp folder with:
#   1. login switched off (its database package does not run in the browser,
#      and a static site cannot keep passwords secret anyway)
#   2. R/build_location_inputs.R left out (only used to build the place
#      library; it would pull in terra, spdep and exactextractr)
#   3. only what the game needs at run time: app.R, R/, data/, the place
#      library and the benchmarks

args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) >= 1) args[1] else file.path(tempdir(), "epi_game_site")

# Stage a copy of the app
stage_dir <- file.path(tempdir(), "epi_game_src")
unlink(stage_dir, recursive = TRUE)
dir.create(stage_dir)

file.copy(c("R", "data", "benchmarks"), stage_dir, recursive = TRUE)
dir.create(file.path(stage_dir, "locations"))
file.copy(file.path("locations", "library"), file.path(stage_dir, "locations"), recursive = TRUE)
file.remove(file.path(stage_dir, "R", "build_location_inputs.R"))

app_lines <- readLines("app.R")
app_lines <- c('Sys.setenv(EPILONDON_AUTH_DISABLE = "true")  # browser build: no login', app_lines)
writeLines(app_lines, file.path(stage_dir, "app.R"))

# Export: the packages are bundled with the site, so it does not depend on
# anyone else's server when people play it
unlink(out_dir, recursive = TRUE)
shinylive::export(appdir = stage_dir, destdir = out_dir)

# Report the size
site_files <- list.files(out_dir, recursive = TRUE, full.names = TRUE)
message(sprintf("Site written to %s (%.0f MB, %d files)", out_dir, sum(file.size(site_files)) / 1e6, length(site_files)))
message("To play it locally: serve that folder (e.g. httpuv::runStaticServer(\"", out_dir, "\")) and open the address.")
