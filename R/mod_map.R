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
  div(class = "page page-flush",
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
  )
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
      b <- req(bundle()); req(b$routes)
      cls <- input$classes
      if (is.null(cls) || !length(cls)) return(b$routes[0, ])
      b$routes[b$routes$classification %in% cls, ]
    })

    output$summary <- renderUI({
      b <- req(bundle())
      r <- shown_routes()
      km <- if (nrow(r)) sum(as.numeric(sf::st_length(sf::st_transform(r, 3402)))) / 1000 else 0
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

      labels <- sprintf(
        "<strong>%s</strong><br/>%s<br/><span style='color:#6b7280'>%s</span>",
        ifelse(is.na(r$street_name_full) | r$street_name_full == "",
               r$classification, r$street_name_full),
        r$classification,
        ifelse(is.na(r$construction_year), r$type,
               paste0(r$type, " · built ", r$construction_year))
      ) |> lapply(HTML)

      m |>
        addPolylines(
          data = r, group = "routes",
          color = unname(CLASS_COLOURS[r$classification]),
          weight = ifelse(r$classification %in% NON_CYCLING_CLASS, 1.4, 2.4),
          opacity = ifelse(r$classification %in% NON_CYCLING_CLASS, 0.5, 0.9),
          label = labels,
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

      # Draw the affected extent where the permit carries geometry; fall back to
      # a point marker where it does not.
      for (i in seq_len(nrow(cs))) {
        rings <- cs$rings[[i]]
        if (!length(rings)) next
        lab <- HTML(sprintf(
          "<strong>%s</strong><br/>%s<br/><span style='color:#6b7280'>Until %s</span>",
          cs$street[i], tools::toTitleCase(tolower(cs$reason[i])),
          format(cs$finish_date[i], "%d %b %Y")))
        for (rg in rings) {
          m <- addPolylines(m, lng = rg[, 1], lat = rg[, 2], group = "construction",
                            color = PAL$status[["critical"]], weight = 5,
                            opacity = 0.75, label = lab)
        }
      }
      pts <- cs[lengths(cs$rings) == 0 & is.finite(cs$lat), ]
      if (nrow(pts)) {
        m <- addCircleMarkers(
          m, data = pts, lng = ~lon, lat = ~lat, group = "construction",
          radius = 5, color = PAL$status[["critical"]], fillOpacity = 0.8, weight = 1,
          label = lapply(sprintf("<strong>%s</strong><br/>%s", pts$street,
                                 tools::toTitleCase(tolower(pts$reason))), HTML))
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
