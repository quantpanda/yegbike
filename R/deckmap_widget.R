# ---------------------------------------------------------------------------
# deckmap_widget.R -- Shiny output/render pair for the deck.gl animation.
#
# A hand-rolled binding rather than an htmlwidget: the widget needs its own
# playback state (clock, speed, mode) that lives entirely in the browser, and
# we only ever push a new payload when the date changes. Wrapping deck.gl in
# htmlwidgets would add a dependency without buying anything here.
#
# The JS libraries are vendored under www/vendor so the app is reproducible.
# Basemap *tiles* still come from Carto's free endpoint -- those cannot be
# bundled -- so the widget needs network access to draw a background, though
# the data layers render regardless.
# ---------------------------------------------------------------------------

deckmap_deps <- function() {
  htmltools::htmlDependency(
    name = "yegbike-deckmap",
    version = "1.0.0",
    src = c(file = "www"),
    script = c("vendor/maplibre-gl.js", "vendor/deck.gl.min.js", "deckmap.js"),
    stylesheet = c("vendor/maplibre-gl.css", "deckmap.css")
  )
}

#' Placeholder for the animated map.
deckmapOutput <- function(id, height = "620px") {
  htmltools::tagList(
    deckmap_deps(),
    htmltools::div(
      id = id,
      class = "deckmap-output shiny-deckmap-output",
      style = sprintf("height:%s;", htmltools::validateCssUnit(height))
    )
  )
}

#' Send a payload built by build_sim_payload() to the widget.
renderDeckmap <- function(expr, env = parent.frame(), quoted = FALSE) {
  func <- shiny::exprToFunction(expr, env, quoted)
  shiny::createRenderFunction(
    func,
    transform = function(value, session, name, ...) value,
    outputFunc = deckmapOutput
  )
}
