# ---------------------------------------------------------------------------
# Network -- what the bike network is actually made of, and when it was built.
# ---------------------------------------------------------------------------

network_ui <- function(id) {
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
    card(full_screen = TRUE,
      card_header("Route inventory"),
      withSpinner(DTOutput(ns("table")), type = 8, color = PAL$cat[1])
    )
  )
}

network_server <- function(id, bundle) {
  moduleServer(id, function(input, output, session) {

    lengths_df <- reactive({
      b <- req(bundle()); req(b$routes)
      route_lengths(b$routes)
    })

    output$tiles <- renderUI({
      b <- req(bundle())
      rl <- lengths_df()
      cyc_km <- sum(rl$km[rl$cycling])
      protected <- sum(rl$km[rl$classification %in%
        c("Protected Bike Lane", "Painted Bike Lane", "Contra-Flow Bike Lane")])
      n_seg <- nrow(b$routes)
      yr <- suppressWarnings(as.integer(b$routes$construction_year))
      recent <- sum(!is.na(yr) & yr >= as.integer(format(Sys.Date(), "%Y")) - 5)

      div(class = "tiles",
        stat_tile(sprintf("%s km", fmt_int(cyc_km)), "Cycling network", accent = PAL$cat[1]),
        stat_tile(sprintf("%s km", fmt_int(protected)), "Dedicated bike lanes",
                  sub = "protected, painted or contra-flow", accent = PAL$cat[3]),
        stat_tile(fmt_int(n_seg), "Route segments", accent = PAL$cat[4]),
        stat_tile(fmt_int(recent), "Segments built since 2021",
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
      b <- req(bundle())
      r <- b$routes
      yr <- suppressWarnings(as.integer(r$construction_year))
      keep <- !is.na(yr) & yr >= 2005 & yr <= as.integer(format(Sys.Date(), "%Y"))
      validate(need(any(keep), "No construction years recorded."))

      len_km <- as.numeric(sf::st_length(sf::st_transform(r[keep, ], 3402))) / 1000
      cls <- r$classification[keep]
      dedicated <- cls %in% c("Protected Bike Lane", "Painted Bike Lane",
                              "Contra-Flow Bike Lane", "Local Street Bikeway")
      df <- data.frame(year = yr[keep], km = len_km,
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

    output$table <- renderDT({
      b <- req(bundle())
      r <- sf::st_drop_geometry(b$routes)
      rl <- lengths_df()
      n_by <- as.data.frame(table(r$classification), stringsAsFactors = FALSE)
      names(n_by) <- c("classification", "segments")
      tab <- merge(rl, n_by, by = "classification")
      out <- data.frame(
        Classification = tab$classification,
        `Cycling infrastructure` = ifelse(tab$cycling, "Yes", "No"),
        Kilometres = round(tab$km, 1),
        Segments = tab$segments,
        `Share of network` = sprintf("%.1f%%", 100 * tab$km / sum(tab$km)),
        check.names = FALSE, stringsAsFactors = FALSE
      )
      out <- out[order(-out$Kilometres), ]
      datatable(out, rownames = FALSE, class = "compact stripe hover",
                options = list(pageLength = 12, dom = "t")) |>
        formatStyle("Cycling infrastructure", color = styleEqual(
          c("Yes", "No"), c(PAL$status[["good"]], PAL$muted)))
    })
  })
}
