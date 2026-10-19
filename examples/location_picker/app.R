# Location picker demo --------------------------------------------------------
#
# Try the "Choose country or city" picker on its own, before it is wired
# into the game. From the project root:
#   shiny::runApp("examples/location_picker")
#
# Needs the place library: source("prepare_place_library.R") once.

library(shiny)

# Project root is two folders up from this app
root <- normalizePath(file.path("..", ".."))

source(file.path(root, "R", "util_io.R"))
source(file.path(root, "R", "build_location_inputs.R"))
source(file.path(root, "R", "place_library.R"))
source(file.path(root, "R", "mod_location_picker.R"))

ui <- bslib::page_fillable(
  title = "Outbreak: choose a place",
  mod_location_picker_ui("picker"),
  shiny::verbatimTextOutput("bundle_check")
)

server <- function(input, output, session) {
  place <- mod_location_picker_server(
    "picker",
    library_dir = file.path(root, "locations", "library"),
    data_dir = file.path(root, "data")
  )

  # What the game would receive
  output$bundle_check <- shiny::renderText({
    inputs <- place()$inputs
    sprintf("Game inputs: %d units | K %d x %d | rows sum to 1: %s | queen links: %d",
            nrow(inputs$pop_tbl), nrow(inputs$K), ncol(inputs$K),
            all(abs(rowSums(inputs$K) - 1) < 1e-6), sum(inputs$A > 0) / 2)
  })
}

shinyApp(ui, server)
