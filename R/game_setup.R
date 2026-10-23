# Game codes ---------------------------------------------------------------------
#
# Every game has a 6-digit code. The code decides the disease, the place, the
# date the first patient is reported (a day in 2027) and the outbreak itself,
# so typing a code back in replays exactly that game. A place chosen with
# "Change location" overrides the code's place.

new_game_code <- function() {
  sample.int(899999L, 1L) + 100000L
}

# Read a code from text; NA if it is not a 6-digit number
parse_game_code <- function(x) {
  code <- suppressWarnings(as.integer(x))
  if (length(code) != 1 || is.na(code) || code < 100000L || code > 999999L) {
    return(NA_integer_)
  }
  code
}

# Draw the disease, place and date for a code (the same code always gives the
# same answer; the outbreak itself is drawn later by new_game(seed = code))
draw_game_setup <- function(game_code, place_index = NULL, place_override = NULL) {
  drawn <- with_stream(new_stream(game_code), {
    # one of the real disease presets (not the "custom" placeholder)
    diseases <- setdiff(scenario_catalog()$id, "custom")
    scenario_id <- sample(diseases, 1)

    # anywhere in the place library (London included)
    place_id <- if (is.null(place_index)) NA_character_ else sample(place_index$id, 1)

    # a day in 2027 (fixed year, so a code gives the same date on any day)
    start_date <- as.Date("2027-01-01") + sample(0:364, 1)

    list(scenario_id = scenario_id, place_id = place_id, start_date = start_date)
  })$value

  # a place chosen by the player wins over the code's place
  if (!is.null(place_override) && !is.null(place_index) && place_override %in% place_index$id) {
    drawn$place_id <- place_override
  }

  drawn$game_code <- game_code
  drawn
}
