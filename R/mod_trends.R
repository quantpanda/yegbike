# ---------------------------------------------------------------------------
# Trends -- ridership over time, with the fleet-growth problem handled openly.
#
# The counter fleet went from 14 sensors in 2018 to 48 today, so a raw
# city-wide total is mostly a chart of how many counters the city has bought.
# The normalisation control is therefore a first-class input, not a footnote.
# ---------------------------------------------------------------------------

trends_ui <- function(id) {
  ns <- NS(id)
  div(class = "page",
    div(class = "page-head",
      h2("Trends"),
      p(class = "lede", "How Edmonton's cycling volumes move through the week, the season and the years.")
    ),
    div(class = "control-row",
      radioButtons(ns("norm"), NULL, inline = TRUE,
        choices = c("Mean per counter" = "mean",
                    "Fixed panel"      = "panel",
                    "Raw total"        = "total"),
        selected = "mean"),
      radioButtons(ns("measure"), NULL, inline = TRUE,
        choices = c("Cyclists" = "cyc", "Pedestrians" = "ped"), selected = "cyc")
    ),
    uiOutput(ns("norm_note")),
    card(full_screen = TRUE,
      card_header("Daily volume"),
      withSpinner(plotlyOutput(ns("series"), height = "330px"), type = 8, color = PAL$cat[1])
    ),
    div(class = "grid-2",
      card(full_screen = TRUE,
        card_header("Year over year"),
        withSpinner(plotlyOutput(ns("yoy"), height = "300px"), type = 8, color = PAL$cat[1]),
        card_footer(class = "card-note",
          "Cumulative through the year, on the counters common to every year shown.")
      ),
      card(full_screen = TRUE,
        card_header("When people ride"),
        withSpinner(plotlyOutput(ns("heat"), height = "300px"), type = 8, color = PAL$cat[1]),
        card_footer(class = "card-note",
          "Mean cyclists per hour from the 15-minute counter feed, last 18 months.")
      )
    ),
    card(full_screen = TRUE,
      card_header("Winter cycling"),
      withSpinner(plotlyOutput(ns("winter"), height = "280px"), type = 8, color = PAL$cat[1]),
      card_footer(class = "card-note",
        "December to February volumes as a share of that year's July peak. Rising share means more Edmontonians riding year-round, independent of how big the summer was.")
    )
  )
}

trends_server <- function(id, bundle, clean_mode) {
  moduleServer(id, function(input, output, session) {

    counts <- reactive(counts_view(req(bundle())$counts, clean_mode()))

    output$norm_note <- renderUI({
      msg <- switch(input$norm %||% "mean",
        mean  = "Averaged across whichever counters reported each day. Comparable over time; a single busy new counter cannot inflate it.",
        panel = "Summed over only those counters present in every year shown — the strictest like-for-like comparison, at the cost of dropping newer sites.",
        total = "Raw sum across all reporting counters. This is what most dashboards show, and it is misleading: the rise since 2018 is substantially the city installing more counters.")
      cls <- if ((input$norm %||% "mean") == "total") "inline-note is-caution" else "inline-note"
      div(class = cls, msg)
    })

    series <- reactive({
      d <- counts()
      daily_series(d, input$measure %||% "cyc", input$norm %||% "mean")
    })

    output$series <- renderPlotly({
      s <- series()
      validate(need(nrow(s) > 0, "No data for this selection."))
      s$roll <- stats::filter(s$value, rep(1 / 28, 28), sides = 2)
      lab <- if ((input$measure %||% "cyc") == "cyc") "Cyclists" else "Pedestrians"

      plot_ly(s) |>
        add_trace(x = ~date, y = ~value, type = "scatter", mode = "lines",
                  line = list(color = PAL$cat[1], width = 0.7), opacity = 0.35,
                  name = "Daily", hoverinfo = "skip") |>
        add_trace(x = ~date, y = ~roll, type = "scatter", mode = "lines",
                  line = list(color = PAL$cat[1], width = 2.2), name = "28-day average",
                  hovertemplate = "%{x|%d %b %Y}<br><b>%{y:.0f}</b><extra></extra>") |>
        plotly_base(ylab = lab, legend = TRUE) |>
        layout(xaxis = list(rangeslider = list(thickness = 0.07, bgcolor = PAL$plane),
                            gridcolor = PAL$grid, linecolor = PAL$axis,
                            tickfont = list(size = 11, color = PAL$muted)))
    })

    output$yoy <- renderPlotly({
      d <- counts()
      meas <- input$measure %||% "cyc"
      d <- d[!is.na(d[[meas]]), ]
      d$year <- as.integer(format(d$date, "%Y"))
      years <- sort(unique(d$year))
      years <- utils::tail(years, 6)
      d <- d[d$year %in% years, ]
      # Common counters only: otherwise later years win purely on fleet size.
      panel <- Reduce(intersect, lapply(years, function(y) unique(d$sensor[d$year == y])))
      validate(need(length(panel) > 0, "No counters common to every year shown."))
      d <- d[d$sensor %in% panel, ]

      d$doy <- as.integer(format(d$date, "%j"))
      agg <- stats::aggregate(d[[meas]], by = list(year = d$year, doy = d$doy), FUN = sum)
      names(agg)[3] <- "v"
      agg <- agg[order(agg$year, agg$doy), ]
      agg$cum <- unlist(lapply(split(agg$v, agg$year), cumsum))

      cols <- ramp_n(c(PAL$seq_blue[2], PAL$seq_blue[7]), length(years))
      p <- plot_ly()
      for (i in seq_along(years)) {
        sub <- agg[agg$year == years[i], ]
        p <- add_trace(p, data = sub, x = ~doy, y = ~cum, type = "scatter", mode = "lines",
                       name = as.character(years[i]),
                       line = list(color = cols[i], width = if (i == length(years)) 2.6 else 1.6),
                       hovertemplate = paste0("<b>", years[i], "</b> day %{x}<br>%{y:,.0f} cumulative<extra></extra>"))
      }
      plotly_base(p, xlab = "Day of year", ylab = "Cumulative count",
                  legend = TRUE, hovermode = "closest") |>
        layout(annotations = list(list(
          x = 1, y = 1, xref = "paper", yref = "paper", xanchor = "right",
          text = sprintf("%d counters common to %d–%d", length(panel), min(years), max(years)),
          showarrow = FALSE, font = list(size = 10, color = PAL$muted))))
    })

    output$heat <- renderPlotly({
      b <- req(bundle())
      latest <- max(b$counts$date, na.rm = TRUE)
      hp <- hour_profile(latest - 545, latest)
      validate(need(nrow(hp) > 0, "Hourly profile unavailable."))
      meas <- input$measure %||% "cyc"
      hp$v <- hp[[meas]] / pmax(1, hp$n)

      dows <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
      z <- matrix(NA_real_, nrow = 7, ncol = 24)
      z[cbind(hp$dow + 1, hp$hour + 1)] <- hp$v
      # Monday-first reads better than the API's Sunday-first ordering.
      ord <- c(2:7, 1)

      plot_ly(
        x = sprintf("%02d", 0:23), y = dows[ord], z = z[ord, ],
        type = "heatmap", colorscale = lapply(seq_along(PAL$seq_blue), function(i)
          list((i - 1) / (length(PAL$seq_blue) - 1), PAL$seq_blue[i])),
        hovertemplate = "<b>%{y} %{x}:00</b><br>%{z:.1f} per counter per 15 min<extra></extra>",
        colorbar = list(title = "", thickness = 10, len = 0.9,
                        tickfont = list(size = 10, color = PAL$muted))
      ) |>
        plotly_base(xlab = "Hour of day", legend = FALSE, hovermode = "closest") |>
        layout(yaxis = list(tickfont = list(size = 11, color = PAL$ink2),
                            gridcolor = "rgba(0,0,0,0)", linecolor = "rgba(0,0,0,0)"))
    })

    output$winter <- renderPlotly({
      d <- counts()
      d <- d[!is.na(d$cyc), ]
      d$year <- as.integer(format(d$date, "%Y"))
      d$mon <- as.integer(format(d$date, "%m"))

      # Winter is compared to the same year's own summer, so a mild or wet
      # July does not read as a winter-cycling boom.
      per_year <- lapply(split(d, d$year), function(g) {
        jul <- mean(g$cyc[g$mon == 7], na.rm = TRUE)
        win <- mean(g$cyc[g$mon %in% c(12, 1, 2)], na.rm = TRUE)
        if (!is.finite(jul) || !is.finite(win) || jul <= 0) return(NULL)
        data.frame(year = g$year[1], share = win / jul, winter = win, july = jul)
      })
      w <- do.call(rbind, Filter(Negate(is.null), per_year))
      validate(need(!is.null(w) && nrow(w) > 1, "Not enough years of data."))
      w <- w[w$year >= 2019, ]

      plot_ly(w, x = ~year, y = ~100 * share, type = "bar",
              marker = list(color = PAL$cat[1], line = list(color = PAL$surface, width = 2)),
              text = ~sprintf("%.0f%%", 100 * share), textposition = "outside",
              outsidetextfont = list(color = PAL$ink2, size = 11),
              hovertemplate = "<b>%{x}</b><br>Winter is %{y:.1f}% of the July rate<extra></extra>") |>
        plotly_base(ylab = "Winter as % of July", legend = FALSE, hovermode = "closest") |>
        layout(xaxis = list(dtick = 1, gridcolor = "rgba(0,0,0,0)",
                            linecolor = PAL$axis,
                            tickfont = list(size = 11, color = PAL$muted)))
    })
  })
}

`%||%` <- function(a, b) if (is.null(a)) b else a
