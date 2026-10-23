# Chart style -------------------------------------------------------------------
#
# All charts have a transparent background, so they take on the colour of the
# page around them. In dark mode (the browser's prefers-color-scheme, sent by
# app.R as input$dark_mode) text and lines switch to light colours.
# Charts are drawn with renderPlot(..., bg = "transparent").

chart_colours <- function(dark = FALSE) {
  if (isTRUE(dark)) {
    list(
      text = "#e6e6e6",
      line = "#cfd8dc",
      grid = "#4a5560",
      accent = "#ff6b6b",
      real = "#f5f5f5",
      muted = "#a8b3bd",
      shade = "#3a4552"
    )
  } else {
    list(
      text = "#2b2b2b",
      line = "#2c3e50",
      grid = "#e3e3e3",
      accent = "#c0392b",
      real = "#000000",
      muted = "#666666",
      shade = "#d9d9d9"
    )
  }
}

chart_theme <- function(dark = FALSE, base_size = 10) {
  col <- chart_colours(dark)
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.key = ggplot2::element_rect(fill = "transparent", colour = NA),
      text = ggplot2::element_text(colour = col$text),
      axis.text = ggplot2::element_text(colour = col$text),
      strip.text = ggplot2::element_text(colour = col$text),
      panel.grid.major = ggplot2::element_line(colour = col$grid),
      panel.grid.minor = ggplot2::element_blank()
    )
}
