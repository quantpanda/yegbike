# ---------------------------------------------------------------------------
# Map -- the "is my route open" view. Routes, counters and live construction
# on one Leaflet canvas.
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

map_ui <- function(id) {
  ns <- NS(id)
  # as_fill_carrier marks this wrapper as both a fill item and a fill
  # container. Without it the div is opaque to bslib's fill chain, the sidebar
  # layout inside gets no height, and the map's height:100% resolves against an
  # auto-height parent -- collapsing the map to zero pixels. The Simulation tab
  # has the same structure but survives on the widget's own min-height.
  bslib::as_fill_carrier(div(class = "page page-flush",
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        width = 300, title = "Layers",
        checkboxInput(ns("cycling_only"), "Cycling infrastructure only", value = TRUE),
        div(class = "sidebar-note",
            "Walkways, breezeways and maintenance access make up 241 km of this dataset. They are not cycling infrastructure — including them overstates the network by about a fifth."),
        hr(),
        checkboxGroupInput(ns("classes"), "Classification", choices = NULL, selected = NULL),
        hr(),
        checkboxInput(ns("show_counters"), "Counters", value = TRUE),
        checkboxInput(ns("show_construction"), "Construction affecting bikes", value = TRUE),
        checkboxInput(ns("show_disruptions"), "Road closures", value = FALSE),
        hr(),
        uiOutput(ns("summary"))
      ),
      withSpinner(leafletOutput(ns("map"), height = "100%"), type = 8, color = PAL$cat[1])
    )
  ))
}

map_server <- function(id, bundle, clean_mode) {
  moduleServer(id, function(input, output, session) {

    observeEvent(bundle(), {
      b <- bundle(); req(b$routes)
      present <- intersect(names(CLASS_COLOURS), unique(b$routes$classification))
      cycling <- setdiff(present, NON_CYCLING_CLASS)
      updateCheckboxGroupInput(session, "classes", choices = present, selected = cycling)
    }, once = TRUE)

    observeEvent(input$cycling_only, {
      b <- req(bundle())
      present <- intersect(names(CLASS_COLOURS), unique(b$routes$classification))
      sel <- if (isTRUE(input$cycling_only)) setdiff(present, NON_CYCLING_CLASS) else present
      updateCheckboxGroupInput(session, "classes", selected = sel)
    }, ignoreInit = TRUE)

    shown_routes <- reactive({
      b <- req(bundle()); req(b$map_routes)
      cls <- input$classes
      if (is.null(cls) || !length(cls)) return(b$map_routes[0, ])
      b$map_routes[b$map_routes$classification %in% cls, ]
    })

    # Length per classification, computed once from full-resolution geometry.
    # Doing it per filter change re-projected 9,860 features on every click,
    # and simplified geometry would under-report the total besides.
    class_km <- reactive({
      b <- req(bundle())
      rl <- route_lengths(b$routes)
      stats::setNames(rl$km, rl$classification)
    })

    output$summary <- renderUI({
      r <- shown_routes()
      km <- sum(class_km()[intersect(names(class_km()), unique(r$classification))], na.rm = TRUE)
      tagList(
        div(class = "sidebar-stat",
            span(class = "sidebar-stat-value", sprintf("%s km", fmt_int(km))),
            span(class = "sidebar-stat-label", "shown on map")),
        div(class = "sidebar-stat",
            span(class = "sidebar-stat-value", fmt_int(nrow(r))),
            span(class = "sidebar-stat-label", "route segments"))
      )
    })

    output$map <- renderLeaflet({
      # Base map is drawn once; layers are swapped by observers so panning and
      # zoom survive every filter change.
      leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
        addProviderTiles("CartoDB.Positron", group = "Light") |>
        addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        addLayersControl(baseGroups = c("Light", "Satellite"),
                         options = layersControlOptions(collapsed = TRUE)) |>
        setView(-113.4938, 53.5461, 11) |>
        addScaleBar(position = "bottomleft")
    })

    observe({
      r <- shown_routes()
      pal <- CLASS_COLOURS[intersect(names(CLASS_COLOURS), unique(r$classification))]
      m <- leafletProxy("map") |> clearGroup("routes") |> removeControl("legend")
      if (!nrow(r)) return(m)

      # Plain-text labels rather than HTML. lapply(HTML) built one htmltools
      # object per feature -- 9,860 of them on a full render, which dominated
      # the server side of drawing this layer. Leaflet takes a character
      # vector directly.
      street <- ifelse(is.na(r$street_name_full) | r$street_name_full == "",
                       r$classification, r$street_name_full)
      labels <- ifelse(is.na(r$construction_year),
                       sprintf("%s — %s", street, r$classification),
                       sprintf("%s — %s (built %s)", street, r$classification,
                               r$construction_year))
      noncyc <- r$classification %in% NON_CYCLING_CLASS

      m |>
        addPolylines(
          data = r, group = "routes",
          color = unname(CLASS_COLOURS[r$classification]),
          weight = ifelse(noncyc, 1.4, 2.4),
          opacity = ifelse(noncyc, 0.5, 0.9),
          label = labels,
          # smoothFactor tells Leaflet how aggressively to simplify per zoom;
          # the default of 1 leaves a lot of on-screen work for a network this
          # dense. Combined with the pre-simplified geometry this is what makes
          # the layer usable with every class switched on.
          smoothFactor = 2,
          highlightOptions = highlightOptions(weight = 6, color = "#0b0b0b",
                                              opacity = 1, bringToFront = TRUE)
        ) |>
        addLegend("bottomright", layerId = "legend", colors = unname(pal),
                  labels = names(pal), title = "Route type", opacity = 0.95)
    })

    observe({
      b <- req(bundle())
      m <- leafletProxy("map") |> clearGroup("counters")
      if (!isTRUE(input$show_counters)) return(m)

      d <- counts_view(b$counts, clean_mode())
      latest <- max(d$date, na.rm = TRUE)
      s <- sensor_summary(d, from = latest - 90, to = latest)
      s <- s[is.finite(s$lat) & is.finite(s$lon) & s$reporting > 0, ]
      if (!nrow(s)) return(m)

      # Radius on sqrt of volume: area then reads as magnitude rather than
      # exaggerating the busiest sites.
      rad <- 5 + 16 * sqrt(s$cyc_mean / max(s$cyc_mean, na.rm = TRUE))
      m |> addCircleMarkers(
        data = s, lng = ~lon, lat = ~lat, group = "counters",
        radius = rad, stroke = TRUE, weight = 2, color = "#ffffff",
        fillColor = PAL$cat[1], fillOpacity = 0.85,
        label = lapply(sprintf(
          "<strong>%s</strong><br/>%s cyclists/day (90-day mean)<br/>%s total · %s%% uptime",
          s$sensor, fmt_int(s$cyc_mean), fmt_int(s$cyc_total),
          round(100 * pmin(1, s$coverage))), HTML)
      )
    })

    observe({
      b <- req(bundle())
      m <- leafletProxy("map") |> clearGroup("construction")
      if (!isTRUE(input$show_construction)) return(m)
      cs <- active_construction(b$construction)
      if (is.null(cs) || !nrow(cs)) return(m)

      # Draw the affected extents in ONE call. The previous version looped over
      # permits and then over each permit's rings, issuing a separate
      # addPolylines per ring -- about 1,500 round trips through the widget for
      # a layer that is a single multi-line geometry. Assemble it as sf and
      # hand Leaflet the lot.
      has_geom <- which(lengths(cs$rings) > 0)
      if (length(has_geom)) {
        geoms <- lapply(cs$rings[has_geom], function(rings) {
          sf::st_multilinestring(lapply(rings, function(m) m[, 1:2, drop = FALSE]))
        })
        ext <- sf::st_sf(
          label = sprintf("%s — %s (until %s)",
                          cs$street[has_geom],
                          tools::toTitleCase(tolower(cs$reason[has_geom])),
                          format(cs$finish_date[has_geom], "%d %b %Y")),
          geometry = sf::st_sfc(geoms, crs = 4326)
        )
        m <- addPolylines(m, data = ext, group = "construction",
                          color = PAL$status[["critical"]], weight = 5,
                          opacity = 0.75, label = ~label, smoothFactor = 2)
      }
      pts <- cs[lengths(cs$rings) == 0 & is.finite(cs$lat), ]
      if (nrow(pts)) {
        m <- addCircleMarkers(
          m, data = pts, lng = ~lon, lat = ~lat, group = "construction",
          radius = 5, color = PAL$status[["critical"]], fillOpacity = 0.8, weight = 1,
          label = sprintf("%s — %s", pts$street,
                          tools::toTitleCase(tolower(pts$reason))))
      }
      m
    })

    observe({
      b <- req(bundle())
      m <- leafletProxy("map") |> clearGroup("disruptions")
      if (!isTRUE(input$show_disruptions)) return(m)
      ds <- b$disruptions
      if (is.null(ds) || !nrow(ds)) return(m)
      ds <- ds[is.finite(ds$lat) & is.finite(ds$lon), ]
      if (!nrow(ds)) return(m)

      m |> addCircleMarkers(
        data = ds, lng = ~lon, lat = ~lat, group = "disruptions",
        radius = 6, color = PAL$status[["warning"]], weight = 1.5,
        fillColor = PAL$status[["warning"]], fillOpacity = 0.55,
        label = lapply(sprintf(
          "<strong>%s</strong><br/>%s from %s to %s<br/><span style='color:#6b7280'>%s</span>",
          ds$on_street, ds$impact, ds$from_street, ds$to_street,
          substr(ifelse(is.na(ds$details), "", ds$details), 1, 140)), HTML))
    })
  })
}
