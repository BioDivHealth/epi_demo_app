<!-- Generated from cursor rule 'lightsail-deploy' -->

> Lightsail deployment checklist and renv notes

# Lightsail deployment checklist

Reference: see the canonical command list in [.cmds](../../../../.cmds) and the deployment section in [README.md](../../../../README.md).

- Use Ubuntu 22.04 and install R 4.5.x from CRAN before restoring packages.
- Install geo/build deps for `sf`/`terra`: `gdal-bin`, `pandoc`, `cmake`, `libabsl-dev`, plus GDAL/PROJ/GEOS headers.
- Upload the app with `.rsyncignore` (exclude only `renv/library/`, not [renv/activate.R](../../../../renv/activate.R)).
- Restore packages by activating renv in the project, e.g. `source("renv/activate.R")` before `renv::restore()`.
- If `terra`/`s2` fail on small instances, add swap and run builds with `MAKEFLAGS=-j1`.
- App logs are under `/var/log/shiny-server/` on the instance.
