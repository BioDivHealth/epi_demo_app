# Location picker module ------------------------------------------------------
#
# Free-text "Choose country or city" box. The best match is loaded from the
# prebuilt place library (prepare_place_library.R) and shown on a map:
#   - surrounding area in grey (context, not in play)
#   - units in play coloured by population, with names on hover
# The server returns a reactive with the chosen place:
#   list(meta = ..., inputs = <load_inputs()-shaped bundle>, context = ...)
# so the game can later run on `place()$inputs` instead of data/.
#
# Self-contained: not wired into app.R yet.


mod_location_picker_ui <- function(id, default_query = "London") {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = TRUE,
    bslib::card_header("Choose where the outbreak happens"),
    shiny::div(
      class = "d-flex gap-2 align-items-end flex-wrap",
      shiny::textInput(
        ns("query"),
        label = "Choose country or city",
        value = default_query,
        placeholder = "e.g. Kenya, Lisbon, New York",
        width = "320px"
      ),
      shiny::actionButton(ns("go"), "Set up", class = "btn-primary mb-3")
    ),
    shiny::uiOutput(ns("matches")),
    shiny::div(class = "fw-semibold", shiny::textOutput(ns("summary"))),
    leaflet::leafletOutput(ns("map"), height = "480px"),
    shiny::div(
      class = "small text-muted mt-1",
      "Boundaries: geoBoundaries (licence varies by country, see docs/DATA_SOURCES.md); ",
      "population: GHS-POP 2020 (EC JRC, CC BY 4.0); Natural Earth."
    )
  )
}


mod_location_picker_server <- function(id,
                                       library_dir = file.path("locations", "library"),
                                       data_dir = "data") {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    index <- read_place_index(library_dir)

    # ---- search: runs on "Set up" (and once at start) ----
    search_text <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$go, {
      search_text(input$query)
    }, ignoreNULL = FALSE)

    hits <- shiny::reactive({
      shiny::req(search_text())
      find_place(search_text(), index, n = 6)
    })

    # Other matches, best one pre-selected (e.g. Hyderabad, India vs Pakistan)
    output$matches <- shiny::renderUI({
      found <- hits()
      if (nrow(found) == 0) {
        return(shiny::div(class = "text-danger mb-2",
                          paste0("No place called '", search_text(), "'. Try a country or a big city.")))
      }
      if (nrow(found) == 1) {
        return(NULL)
      }
      shiny::radioButtons(
        ns("place_id"),
        label = "Did you mean:",
        choices = stats::setNames(found$id, found$label),
        selected = found$id[1],
        inline = TRUE
      )
    })

    # ---- the chosen place ----
    chosen_id <- shiny::reactive({
      found <- hits()
      shiny::req(nrow(found) > 0)
      if (nrow(found) == 1 || is.null(input$place_id) || !input$place_id %in% found$id) {
        found$id[1]
      } else {
        input$place_id
      }
    })

    place <- shiny::reactive({
      load_place(chosen_id(), index, library_dir = library_dir, data_dir = data_dir)
    })

    output$summary <- shiny::renderText({
      p <- place()
      n_units <- nrow(p$inputs$pop_tbl)
      pop_total <- sum(p$inputs$pop_tbl$pop)
      unit_word <- if (p$meta$type == "city") "local areas" else "regions"
      sprintf("%s: %d %s in play (%s), %s people",
              p$meta$label, n_units, unit_word, p$meta$level,
              format(round(pop_total), big.mark = ","))
    })

    # ---- map ----
    output$map <- leaflet::renderLeaflet({
      p <- place()
      units <- p$inputs$boros
      context <- p$context

      pop_palette <- leaflet::colorNumeric(c("#FEE8C8", "#FDBB84", "#E34A33", "#7F0000"),
                                           domain = log10(units$pop))
      unit_labels <- sprintf("%s: %s people", units$borough_name,
                             format(round(units$pop), big.mark = ","))
      # Zoom to the units in play, with a margin so the surroundings show
      bbox <- sf::st_bbox(units)
      margin_x <- (bbox[["xmax"]] - bbox[["xmin"]]) * 0.15
      margin_y <- (bbox[["ymax"]] - bbox[["ymin"]]) * 0.15

      context_label <- if ("shapeName" %in% names(context)) context$shapeName else context$name

      # No web basemap: the grey surrounding units are the backdrop, so the
      # map works offline (and needs no tile-provider API key)
      leaflet::leaflet(options = leaflet::leafletOptions(zoomSnap = 0.25)) |>
        htmlwidgets::onRender("function(el) { el.style.background = '#DCEBF5'; }") |>
        # Surrounding area: grey, not in play
        leaflet::addPolygons(
          data = context,
          fillColor = "#BDBDBD", fillOpacity = 0.35,
          color = "#9E9E9E", weight = 0.6,
          label = context_label,
          group = "Surrounding area"
        ) |>
        # Units in play: coloured by population
        leaflet::addPolygons(
          data = units,
          fillColor = pop_palette(log10(units$pop)), fillOpacity = 0.75,
          color = "#3F3F3F", weight = 1,
          label = unit_labels,
          highlightOptions = leaflet::highlightOptions(weight = 3, color = "#000000", bringToFront = TRUE),
          group = "In play"
        ) |>
        leaflet::addLegend(
          position = "bottomright",
          colors = c("#E34A33", "#BDBDBD"),
          labels = c("In play (darker = more people)", "Surrounding area"),
          opacity = 0.8
        ) |>
        leaflet::fitBounds(bbox[["xmin"]] - margin_x, bbox[["ymin"]] - margin_y,
                           bbox[["xmax"]] + margin_x, bbox[["ymax"]] + margin_y)
    })

    place
  })
}
