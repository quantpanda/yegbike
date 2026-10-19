# ---------------------------------------------------------------------------
# About -- what the bike network is made of, and what the app corrects in the
# City's counter data. Replaces the separate Network and Data quality tabs.
# ---------------------------------------------------------------------------

about_ui <- function(id) {
  ns <- NS(id)
  div(class = "page",
    div(class = "page-head",
      h2("The network"),
      p(class = "lede", "Composition and build-out of Edmonton's cycling infrastructure.")
    ),
    uiOutput(ns("tiles")),
    div(class = "grid-2",
      card(full_screen = TRUE,
        card_header("Kilometres by route type"),
        withSpinner(plotlyOutput(ns("composition"), height = "360px"), type = 8, color = PAL$cat[1]),
        card_footer(class = "card-note",
          "Walkways, breezeways and maintenance access are shipped in the city's Bike Routes dataset but are not cycling infrastructure. They are shown separately rather than dropped silently.")
      ),
      card(full_screen = TRUE,
        card_header("Protected and painted lanes, by year built"),
        withSpinner(plotlyOutput(ns("buildout"), height = "360px"), type = 8, color = PAL$cat[1]),
        card_footer(class = "card-note",
          "Construction year is recorded for only about 7% of segments, so this shows the shape of the build-out programme rather than its full extent.")
      )
    ),

    div(class = "page-head page-head-section",
      h2("Data quality"),
      p(class = "lede",
        "The City's counter data has real faults: a sensor that reported 86,235 cyclists in a February, duplicated days, and counters renamed mid-series. Every chart in this app uses the cleaned figures below.")
    ),
    div(class = "grid-2",
      card(
        card_header("Correction rules"),
        withSpinner(DTOutput(ns("rules")), type = 8, color = PAL$cat[1]),
        card_footer(class = "card-note",
          "Rules that alter data replace a value with a gap; the rest only annotate.")
      ),
      card(full_screen = TRUE,
        card_header("Before and after"),
        withSpinner(plotlyOutput(ns("compare"), height = "320px"), type = 8, color = PAL$cat[1]),
        card_footer(class = "card-note",
          "City-wide daily cyclists per counter. The raw line's spikes are single malfunctioning sensors, not busy days.")
      )
    )
  )
}

about_server <- function(id, bundle) {
  moduleServer(id, function(input, output, session) {

    lengths_df <- reactive(route_lengths(req(req(bundle())$routes)))

    output$tiles <- renderUI({
      r <- req(bundle()$routes)
      rl <- lengths_df()
      cyc_km <- sum(rl$km[rl$cycling])
      dedicated <- sum(rl$km[rl$classification %in%
        c("Protected Bike Lane", "Painted Bike Lane", "Contra-Flow Bike Lane")])
      recent_from <- as.integer(format(Sys.Date(), "%Y")) - 5
      recent <- sum(!is.na(r$construction_year) & r$construction_year >= recent_from)

      div(class = "tiles",
        stat_tile(sprintf("%s km", fmt_int(cyc_km)), "Cycling network", accent = PAL$cat[1]),
        stat_tile(sprintf("%s km", fmt_int(dedicated)), "Dedicated bike lanes",
                  sub = "protected, painted or contra-flow", accent = PAL$cat[3]),
        stat_tile(fmt_int(nrow(r)), "Route segments", accent = PAL$cat[4]),
        stat_tile(fmt_int(recent), sprintf("Segments built since %d", recent_from),
                  sub = "where a build year is recorded", accent = PAL$cat[5])
      )
    })

    output$composition <- renderPlotly({
      rl <- lengths_df()
      validate(need(nrow(rl) > 0, "No route data."))
      rl <- rl[order(rl$km), ]
      rl$label <- factor(rl$classification, levels = rl$classification)
      cols <- unname(CLASS_COLOURS[as.character(rl$classification)])
      cols[is.na(cols)] <- PAL$muted

      plot_ly(rl, y = ~label, x = ~km, type = "bar", orientation = "h",
              marker = list(color = cols, line = list(color = PAL$surface, width = 2)),
              text = ~sprintf("%s km", fmt_int(km)), textposition = "auto",
              insidetextfont = list(color = "#fff", size = 11),
              outsidetextfont = list(color = PAL$ink2, size = 11),
              customdata = ~ifelse(cycling, "cycling infrastructure", "not cycling infrastructure"),
              hovertemplate = "<b>%{y}</b><br>%{x:.1f} km<br>%{customdata}<extra></extra>") |>
        plotly_base(xlab = "Kilometres", legend = FALSE, hovermode = "closest") |>
        layout(yaxis = list(title = "", tickfont = list(size = 10.5, color = PAL$ink2),
                            gridcolor = "rgba(0,0,0,0)", linecolor = "rgba(0,0,0,0)"))
    })

    output$buildout <- renderPlotly({
      r <- req(bundle()$routes)
      yr <- r$construction_year
      keep <- !is.na(yr) & yr >= 2005 & yr <= as.integer(format(Sys.Date(), "%Y"))
      validate(need(any(keep), "No construction years recorded."))

      dedicated <- r$classification[keep] %in% c("Protected Bike Lane", "Painted Bike Lane",
                                                 "Contra-Flow Bike Lane", "Local Street Bikeway")
      df <- data.frame(year = yr[keep], km = r$km[keep],
                       kind = ifelse(dedicated, "Dedicated bike lane", "Shared path or roadway"))
      agg <- stats::aggregate(km ~ year + kind, data = df, FUN = sum)

      plot_ly(agg, x = ~year, y = ~km, color = ~kind, type = "bar",
              colors = c("Dedicated bike lane" = PAL$cat[1],
                         "Shared path or roadway" = PAL$cat[3]),
              marker = list(line = list(color = PAL$surface, width = 2)),
              hovertemplate = "<b>%{x}</b><br>%{y:.1f} km<extra>%{fullData.name}</extra>") |>
        plotly_base(xlab = "Year built", ylab = "Kilometres", legend = TRUE,
                    hovermode = "x unified") |>
        layout(barmode = "stack",
               xaxis = list(dtick = 2, gridcolor = "rgba(0,0,0,0)", linecolor = PAL$axis,
                            tickfont = list(size = 11, color = PAL$muted)))
    })

    output$rules <- renderDT({
      s <- req(bundle()$qa_summary)
      tab <- data.frame(
        Rule = s$rule,
        What = s$label,
        Readings = s$n,
        Effect = ifelse(s$destructive, "Alters data", "Annotates only"),
        check.names = FALSE, stringsAsFactors = FALSE
      )
      datatable(tab, rownames = FALSE, class = "compact stripe",
                options = list(dom = "t", pageLength = 20)) |>
        formatCurrency("Readings", "", digits = 0) |>
        formatStyle("Effect", fontWeight = styleEqual("Alters data", "600"),
                    color = styleEqual(c("Alters data", "Annotates only"),
                                       c(PAL$status[["serious"]], PAL$muted)))
    })

    output$compare <- renderPlotly({
      d <- req(bundle()$counts)
      raw <- d
      raw$cyc <- raw$cyc_raw
      raw   <- daily_series(raw, "cyc", "mean")
      clean <- daily_series(d,   "cyc", "mean")

      plot_ly() |>
        add_trace(data = raw, x = ~date, y = ~value, type = "scatter", mode = "lines",
                  name = "Raw", line = list(color = PAL$cat[2], width = 1.4),
                  hovertemplate = "%{x|%d %b %Y}<br>Raw <b>%{y:.0f}</b><extra></extra>") |>
        add_trace(data = clean, x = ~date, y = ~value, type = "scatter", mode = "lines",
                  name = "Cleaned", line = list(color = PAL$cat[1], width = 1.6),
                  hovertemplate = "%{x|%d %b %Y}<br>Cleaned <b>%{y:.0f}</b><extra></extra>") |>
        plotly_base(ylab = "Cyclists per counter per day", legend = TRUE)
    })
  })
}
