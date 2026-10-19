# ---------------------------------------------------------------------------
# Map -- the "is my route open" view. Routes, counters and live construction
# on one Leaflet canvas.
#
# Everything is drawn once when the map renders. The sidebar controls only
# show and hide layer groups, so a click costs a few bytes over the websocket
# rather than re-sending the route geometry.
# ---------------------------------------------------------------------------

# Fixed colour per classification. Assigned once by identity, so filtering the
# list never repaints the survivors.
CLASS_COLOURS <- c(
  "Protected Bike Lane"             = "#2a78d6",
  "Painted Bike Lane"               = "#3987e5",
  "Contra-Flow Bike Lane"           = "#6da7ec",
  "Local Street Bikeway"            = "#1baf7a",
  "Shared Pathway"                  = "#008300",
  "Shared Trail"                    = "#4a7c2f",
  "Shared Roadway - Lower Traffic"  = "#eda100",
  "Shared Roadway - Higher Traffic" = "#eb6834",
  "Bus / Bike / Taxi Lane"          = "#e87ba4",
  "Walkway / Breezeway"             = "#b9b6ac",
  "Maintenance Access"              = "#8d8a82"
)

# Esri's gray canvas needs no API key. CARTO's basemaps, which this map used
# before, now return an "API KEY REQUIRED" image for every tile.
MAP_TILES <- "Esri.WorldGrayCanvas"

map_ui <- function(id) {
  ns <- NS(id)
  # as_fill_carrier marks this wrapper as both a fill item and a fill
  # container. Without it the div is opaque to bslib's fill chain, the sidebar
  # layout inside gets no height, and the map's height:100% resolves against an
  # auto-height parent -- collapsing the map to zero pixels.
  bslib::as_fill_carrier(div(class = "page page-flush",
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        width = 300, title = "Layers",
        # Each label carries its line colour, which stands in for a legend.
        checkboxGroupInput(ns("classes"), "Route type",
                           choiceNames = lapply(names(CLASS_COLOURS), function(cls) tagList(
                             span(class = "swatch", style = sprintf("background:%s", CLASS_COLOURS[[cls]])),
                             cls)),
                           choiceValues = names(CLASS_COLOURS),
                           selected = setdiff(names(CLASS_COLOURS), NON_CYCLING_CLASS)),
        div(class = "sidebar-note",
            "Walkways, breezeways and maintenance access are in the city's bike routes data but are not cycling infrastructure, so they start hidden."),
        hr(),
        checkboxInput(ns("show_counters"), "Counters", value = TRUE),
        checkboxInput(ns("show_construction"), "Construction affecting bikes", value = TRUE),
        hr(),
        uiOutput(ns("summary"))
      ),
      withSpinner(leafletOutput(ns("map"), height = "100%"), type = 8, color = PAL$cat[1])
    )
  ))
}

map_server <- function(id, bundle) {
  moduleServer(id, function(input, output, session) {

    class_km <- reactive({
      rl <- route_lengths(req(bundle())$routes)
      stats::setNames(rl$km, rl$classification)
    })

    output$summary <- renderUI({
      b <- req(bundle())
      cls <- input$classes
      km <- sum(class_km()[intersect(names(class_km()), cls)], na.rm = TRUE)
      n_seg <- sum(b$routes$classification %in% cls)
      tagList(
        div(class = "sidebar-stat",
            span(class = "sidebar-stat-value", sprintf("%s km", fmt_int(km))),
            span(class = "sidebar-stat-label", "shown on map")),
        div(class = "sidebar-stat",
            span(class = "sidebar-stat-value", fmt_int(n_seg)),
            span(class = "sidebar-stat-label", "route segments"))
      )
    })

    output$map <- renderLeaflet({
      b <- req(bundle())
      m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
        addProviderTiles(MAP_TILES) |>
        setView(-113.4938, 53.5461, 11) |>
        addScaleBar(position = "bottomleft")

      # Routes: one merged shape per classification, one group each.
      r <- b$map_routes
      if (!is.null(r) && nrow(r)) {
        km <- class_km()
        for (i in seq_len(nrow(r))) {
          cls <- r$classification[i]
          noncyc <- cls %in% NON_CYCLING_CLASS
          col <- unname(CLASS_COLOURS[cls])
          if (is.na(col)) col <- PAL$muted
          m <- addPolylines(
            m, data = r[i, ], group = cls,
            color = col,
            weight = if (noncyc) 1.4 else 2.4,
            opacity = if (noncyc) 0.5 else 0.9,
            label = sprintf("%s — %s km city-wide", cls, fmt_int(km[cls])),
            smoothFactor = 1.5
          )
        }
      }

      m <- add_counter_layer(m, b$counts)
      m <- add_construction_layer(m, b$construction_map)

      # Match whatever the sidebar says at render time; the observers below
      # take over from there.
      isolate({
        m <- hideGroup(m, setdiff(names(CLASS_COLOURS), input$classes))
        if (!isTRUE(input$show_counters))     m <- hideGroup(m, "counters")
        if (!isTRUE(input$show_construction)) m <- hideGroup(m, "construction")
      })
      m
    })

    observeEvent(input$classes, ignoreNULL = FALSE, ignoreInit = TRUE, {
      cls <- input$classes
      leafletProxy("map") |>
        showGroup(intersect(names(CLASS_COLOURS), cls)) |>
        hideGroup(setdiff(names(CLASS_COLOURS), cls))
    })

    observeEvent(input$show_counters, ignoreInit = TRUE, {
      p <- leafletProxy("map")
      if (isTRUE(input$show_counters)) showGroup(p, "counters") else hideGroup(p, "counters")
    })

    observeEvent(input$show_construction, ignoreInit = TRUE, {
      p <- leafletProxy("map")
      if (isTRUE(input$show_construction)) showGroup(p, "construction") else hideGroup(p, "construction")
    })
  })
}

#' Counter sites, sized by 90-day mean volume.
add_counter_layer <- function(m, counts) {
  latest <- max(counts$date, na.rm = TRUE)
  s <- sensor_summary(counts, from = latest - 90, to = latest)
  s <- s[is.finite(s$lat) & is.finite(s$lon) & s$reporting > 0, ]
  if (!nrow(s)) return(m)

  # Radius on sqrt of volume: area then reads as magnitude rather than
  # exaggerating the busiest sites.
  rad <- 5 + 16 * sqrt(s$cyc_mean / max(s$cyc_mean, na.rm = TRUE))
  addCircleMarkers(
    m, data = s, lng = ~lon, lat = ~lat, group = "counters",
    radius = rad, stroke = TRUE, weight = 2, color = "#ffffff",
    fillColor = PAL$cat[1], fillOpacity = 0.85,
    label = lapply(sprintf(
      "<strong>%s</strong><br/>%s cyclists/day (90-day mean)<br/>%s total · %s%% uptime",
      s$sensor, fmt_int(s$cyc_mean), fmt_int(s$cyc_total),
      round(100 * pmin(1, s$coverage))), HTML)
  )
}

#' Open construction permits affecting bike facilities.
#'
#' @param cm construction_map() output from the shared bundle
add_construction_layer <- function(m, cm) {
  if (is.null(cm)) return(m)
  if (!is.null(cm$lines) && nrow(cm$lines)) {
    m <- addPolylines(m, data = cm$lines, group = "construction",
                      color = PAL$status[["critical"]], weight = 5,
                      opacity = 0.75, label = ~label, smoothFactor = 2)
  }
  if (!is.null(cm$points) && nrow(cm$points)) {
    m <- addCircleMarkers(
      m, data = cm$points, lng = ~lon, lat = ~lat, group = "construction",
      radius = 5, color = PAL$status[["critical"]], fillOpacity = 0.8, weight = 1,
      label = ~label)
  }
  m
}
