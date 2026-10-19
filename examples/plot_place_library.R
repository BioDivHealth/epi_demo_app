# plot_place_library.R --------------------------------------------------------
#
# Sense-check figure for the place library: a grid of maps, one per place,
# with the units in play coloured by population and the surroundings in grey.
#
# Run from the project root after prepare_place_library.R:
#   source("examples/plot_place_library.R")
# Output: locations/place_library_check.png

suppressPackageStartupMessages({
  library(sf)
  library(ggplot2)
})
sf_use_s2(FALSE)

source("R/build_location_inputs.R")
source("R/place_library.R")

library_dir <- file.path("locations", "library")
index <- read_place_index(library_dir)

# A spread of countries and cities across continents
places_to_plot <- c("Portugal", "Kenya", "Japan", "Brazil", "United Kingdom", "India",
                    "London", "Lagos", "São Paulo", "Istanbul", "Mumbai", "Sydney")

# Draw one place: grey surroundings, in-play units shaded by population
plot_one_place <- function(query) {
  hit <- find_place(query, index, n = 1)
  if (nrow(hit) == 0) {
    return(NULL)
  }
  place <- readRDS(file.path(library_dir, hit$file))
  units <- place$inputs$boros
  bbox <- sf::st_bbox(units)

  ggplot() +
    geom_sf(data = place$context, fill = "grey88", colour = "grey65", linewidth = 0.1) +
    geom_sf(data = units, aes(fill = log10(pop)), colour = "white", linewidth = 0.15) +
    scale_fill_distiller(palette = "OrRd", direction = 1, guide = "none") +
    coord_sf(xlim = bbox[c("xmin", "xmax")], ylim = bbox[c("ymin", "ymax")], expand = TRUE) +
    labs(title = hit$label,
         subtitle = paste0(hit$n_units, " ", hit$level, " units, ",
                           round(hit$pop_total / 1e6, 1), " M people")) +
    theme_void(base_size = 8) +
    theme(panel.background = element_rect(fill = "#DCEBF5", colour = NA))
}

plots <- lapply(places_to_plot, plot_one_place)
plots <- plots[!vapply(plots, is.null, logical(1))]

check_figure <- patchwork::wrap_plots(plots, ncol = 4)
ggsave(file.path("locations", "place_library_check.png"), check_figure,
       width = 13, height = 10, dpi = 100)
message("Saved locations/place_library_check.png")
