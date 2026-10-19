# ---------------------------------------------------------------------------
# Overview -- the answer to "what is going on right now", above the fold.
# ---------------------------------------------------------------------------

overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "page",
      div(class = "page-head",
        h2("Edmonton cycling at a glance"),
        p(class = "lede",
          "Counter readings, route status and construction, pulled live from the City of Edmonton open data portal.")
      ),
      uiOutput(ns("tiles")),
      div(class = "grid-2",
        card(full_screen = TRUE,
          card_header("Daily cyclists per counter, last 12 months"),
          withSpinner(plotlyOutput(ns("recent"), height = "300px"), type = 8, color = PAL$cat[1]),
          card_footer(class = "card-note",
            "Averaged across reporting counters, so the line tracks ridership rather than how many counters the city has installed.")
        ),
        card(full_screen = TRUE,
          card_header("Busiest counters, last 90 days"),
          withSpinner(plotlyOutput(ns("top"), height = "300px"), type = 8, color = PAL$cat[1])
        )
      ),
      div(class = "grid-2",
        card(full_screen = TRUE,
          card_header("Seasonality — how Edmonton rides through the year"),
          withSpinner(plotlyOutput(ns("season"), height = "290px"), type = 8, color = PAL$cat[1]),
          card_footer(class = "card-note",
            "Median cyclists per counter per day, by month, across all years of record. Winter riding is real but roughly a fifth of the July peak.")
        ),
        card(
          card_header("Bike routes disrupted by construction"),
          withSpinner(DTOutput(ns("disrupt")), type = 8, color = PAL$cat[1]),
          card_footer(class = "card-note",
            "Open permits where the city has flagged that bike facilities are affected.")
        )
      )
    )
  )
}

overview_server <- function(id, bundle) {
  moduleServer(id, function(input, output, session) {

    counts <- reactive(req(bundle())$counts)

    output$tiles <- renderUI({
      b <- req(bundle()); d <- counts()
      latest <- max(d$date, na.rm = TRUE)

      # Year to date against the same window last year, on a fixed panel of
      # counters so the comparison is not contaminated by fleet growth.
      ytd_from <- as.Date(format(latest, "%Y-01-01"))
      prev_from <- ytd_from - 365; prev_to <- latest - 365
      panel <- intersect(
        unique(d$sensor[d$date >= ytd_from & d$date <= latest & !is.na(d$cyc)]),
        unique(d$sensor[d$date >= prev_from & d$date <= prev_to & !is.na(d$cyc)])
      )
      ytd  <- sum(d$cyc[d$date >= ytd_from & d$date <= latest & d$sensor %in% panel], na.rm = TRUE)
      prev <- sum(d$cyc[d$date >= prev_from & d$date <= prev_to & d$sensor %in% panel], na.rm = TRUE)
      delta <- if (prev > 0) (ytd - prev) / prev else NA

      last_day <- sum(d$cyc[d$date == latest], na.rm = TRUE)
      n_active <- length(active_sensors(d))
      n_constr <- nrow(active_construction(b$construction))

      div(class = "tiles",
        stat_tile(fmt_int(last_day), "Cyclists counted",
                  sub = format(latest, "%A %d %B"), accent = PAL$cat[1]),
        stat_tile(fmt_compact(ytd), sprintf("Year to date (%s)", format(latest, "%Y")),
                  sub = sprintf("vs %s on %d shared counters", format(prev_from, "%Y"), length(panel)),
                  delta = delta, accent = PAL$cat[3]),
        stat_tile(n_active, "Counters reporting",
                  sub = "in the last 30 days", accent = PAL$cat[4]),
        stat_tile(fmt_int(n_constr), "Bike routes under construction",
                  sub = "open permits affecting bike facilities",
                  accent = PAL$status[["serious"]])
      )
    })

    output$recent <- renderPlotly({
      d <- counts()
      latest <- max(d$date, na.rm = TRUE)
      s <- daily_series(d[d$date > latest - 365, ], "cyc", "mean")
      validate(need(nrow(s) > 0, "No counts in the last year."))
      # A 7-day mean rides over the day-of-week sawtooth without hiding the
      # weather-driven swings, which are the interesting part.
      s$roll <- stats::filter(s$value, rep(1 / 7, 7), sides = 2)

      plot_ly(s) |>
        add_trace(x = ~date, y = ~value, type = "scatter", mode = "markers",
                  marker = list(size = 3, color = PAL$cat[1], opacity = 0.22),
                  name = "Daily", hoverinfo = "skip", showlegend = FALSE) |>
        add_trace(x = ~date, y = ~roll, type = "scatter", mode = "lines",
                  line = list(color = PAL$cat[1], width = 2), name = "7-day average",
                  hovertemplate = "%{x|%d %b %Y}<br><b>%{y:.0f}</b> cyclists per counter<extra></extra>") |>
        plotly_base(ylab = "Cyclists per counter per day", legend = FALSE)
    })

    output$top <- renderPlotly({
      d <- counts()
      latest <- max(d$date, na.rm = TRUE)
      s <- sensor_summary(d, from = latest - 90, to = latest)
      validate(need(nrow(s) > 0, "No recent counter data."))
      s <- utils::head(s[order(-s$cyc_mean), ], 12)
      s$sensor <- factor(s$sensor, levels = rev(s$sensor))

      plot_ly(s, y = ~sensor, x = ~cyc_mean, type = "bar", orientation = "h",
              marker = list(color = PAL$cat[1],
                            line = list(color = PAL$surface, width = 2)),
              text = ~fmt_int(cyc_mean), textposition = "auto",
              insidetextfont = list(color = "#fff", size = 11),
              outsidetextfont = list(color = PAL$ink2, size = 11),
              hovertemplate = "<b>%{y}</b><br>%{x:.0f} cyclists/day<extra></extra>") |>
        plotly_base(xlab = "Mean cyclists per day", legend = FALSE, hovermode = "closest") |>
        layout(yaxis = list(title = "", tickfont = list(size = 10.5, color = PAL$ink2),
                            gridcolor = "rgba(0,0,0,0)", linecolor = "rgba(0,0,0,0)"))
    })

    output$season <- renderPlotly({
      d <- counts()
      d <- d[!is.na(d$cyc), ]
      validate(need(nrow(d) > 0, "No data."))
      d$month <- as.integer(format(d$date, "%m"))
      agg <- stats::aggregate(cyc ~ month, data = d, FUN = stats::median)
      agg$label <- month.abb[agg$month]
      agg$label <- factor(agg$label, levels = month.abb)
      # Sequential single-hue fill: magnitude, so lightness carries the value.
      cols <- ramp_n(PAL$seq_blue, 100)
      idx <- pmax(1, ceiling(99 * agg$cyc / max(agg$cyc)))

      plot_ly(agg, x = ~label, y = ~cyc, type = "bar",
              marker = list(color = cols[idx],
                            line = list(color = PAL$surface, width = 2)),
              text = ~fmt_int(cyc), textposition = "outside",
              outsidetextfont = list(color = PAL$ink2, size = 10),
              hovertemplate = "<b>%{x}</b><br>%{y:.0f} cyclists per counter<extra></extra>") |>
        plotly_base(ylab = "Median cyclists per counter", legend = FALSE, hovermode = "closest")
    })

    output$disrupt <- renderDT({
      b <- req(bundle())
      cs <- active_construction(b$construction)
      validate(need(nrow(cs) > 0, "No open permits affecting bike facilities."))
      tab <- data.frame(
        Street = cs$street,
        Extent = substr(ifelse(is.na(cs$extent), "", cs$extent), 1, 60),
        Reason = tools::toTitleCase(tolower(ifelse(is.na(cs$reason), "", cs$reason))),
        Until  = format(cs$finish_date, "%d %b %Y"),
        stringsAsFactors = FALSE
      )
      tab <- tab[order(tab$Street), ]
      datatable(tab, rownames = FALSE, class = "compact stripe hover",
                options = list(pageLength = 6, dom = "ftp", scrollX = TRUE,
                               columnDefs = list(list(width = "34%", targets = 0))))
    })
  })
}
