# ---------------------------------------------------------------------------
# Counters -- the lower half of the Trends tab: per-sensor detail, including how much of the record is actually
# there. Coverage matters: almost no counter has a complete series, and a gap
# looks identical to a quiet day unless you show it.
# ---------------------------------------------------------------------------

counters_ui <- function(id) {
  ns <- NS(id)
  div(class = "page page-continued",
    div(class = "page-head",
      h2("Individual counters"),
      p(class = "lede", "Every permanent counter the city runs, and what each one has seen.")
    ),
    div(class = "control-row",
      selectInput(ns("sensor"), NULL, choices = NULL, width = "340px"),
      div(class = "control-spacer"),
      checkboxInput(ns("active_only"), "Reporting in the last 30 days", value = TRUE)
    ),
    uiOutput(ns("head")),
    div(class = "grid-2",
      card(full_screen = TRUE,
        card_header("Daily history"),
        withSpinner(plotlyOutput(ns("history"), height = "300px"), type = 8, color = PAL$cat[1]),
        card_footer(class = "card-note",
          "Grey bands are days with no reading — either the counter was flagged out of service or nothing was published.")
      ),
      card(full_screen = TRUE,
        card_header("Typical day"),
        withSpinner(plotlyOutput(ns("profile"), height = "300px"), type = 8, color = PAL$cat[1]),
        card_footer(class = "card-note",
          "Mean cyclists per 15 minutes over the last 12 months, weekday against weekend.")
      )
    ),
    card(full_screen = TRUE,
      card_header("All counters"),
      withSpinner(DTOutput(ns("table")), type = 8, color = PAL$cat[1])
    )
  )
}

counters_server <- function(id, bundle) {
  moduleServer(id, function(input, output, session) {

    counts <- reactive(req(bundle())$counts)

    choices <- reactive({
      d <- counts()
      s <- sensor_summary(d)
      if (isTRUE(input$active_only)) s <- s[s$sensor %in% active_sensors(d), ]
      s$sensor
    })

    observeEvent(choices(), {
      cur <- input$sensor
      ch <- choices()
      updateSelectInput(session, "sensor", choices = ch,
                        selected = if (!is.null(cur) && cur %in% ch) cur else ch[1])
    })

    sel <- reactive({
      s <- req(input$sensor)
      d <- counts()
      d[d$sensor == s, ]
    })

    output$head <- renderUI({
      b <- req(bundle())
      x <- sel(); req(nrow(x) > 0)
      s <- sensor_summary(x)
      m <- b$meta[tidy_sensor_name(b$meta$sensor_raw) == x$sensor[1], ]

      div(class = "counter-head",
        div(class = "tiles tiles-4",
          stat_tile(fmt_compact(s$cyc_total), "Cyclists all time", accent = PAL$cat[1]),
          stat_tile(fmt_int(s$cyc_mean), "Mean per day", accent = PAL$cat[3]),
          stat_tile(fmt_int(s$cyc_max), "Busiest day", accent = PAL$cat[4]),
          stat_tile(sprintf("%d%%", round(100 * min(1, s$coverage))), "Days with a reading",
                    sub = sprintf("%s to %s", format(s$first, "%b %Y"), format(s$last, "%b %Y")),
                    accent = PAL$cat[5])
        ),
        if (nrow(m) && !is.na(m$config[1]))
          div(class = "counter-meta",
              span(class = "chip", m$config[1]),
              if (!is.na(m$direction[1])) span(class = "chip", m$direction[1]),
              if (!is.na(m$image_url[1]) && nzchar(m$image_url[1]))
                tags$a(href = m$image_url[1], target = "_blank", rel = "noopener",
                       class = "chip chip-link", "Photo of this counter"))
      )
    })

    output$history <- renderPlotly({
      x <- sel(); req(nrow(x) > 0)
      x <- x[order(x$date), ]

      # Explicit gap bands: a missing day should not silently interpolate into
      # a line that implies a reading was taken.
      full <- data.frame(date = seq(min(x$date), max(x$date), by = "day"))
      x <- merge(full, x[, c("date", "cyc", "outage")], by = "date", all.x = TRUE)
      x$roll <- stats::filter(ifelse(is.na(x$cyc), NA, x$cyc), rep(1 / 14, 14), sides = 2)

      miss <- is.na(x$cyc)
      shapes <- list()
      if (any(miss)) {
        r <- rle(miss)
        ends <- cumsum(r$lengths); starts <- ends - r$lengths + 1
        for (i in which(r$values)) {
          if (r$lengths[i] < 3) next
          shapes[[length(shapes) + 1]] <- list(
            type = "rect", xref = "x", yref = "paper",
            x0 = x$date[starts[i]], x1 = x$date[ends[i]], y0 = 0, y1 = 1,
            fillcolor = "rgba(137,135,129,0.16)", line = list(width = 0), layer = "below")
        }
      }

      plot_ly(x) |>
        add_trace(x = ~date, y = ~cyc, type = "scatter", mode = "markers",
                  marker = list(size = 2.6, color = PAL$cat[1], opacity = 0.3),
                  name = "Daily", hoverinfo = "skip", showlegend = FALSE) |>
        add_trace(x = ~date, y = ~roll, type = "scatter", mode = "lines",
                  line = list(color = PAL$cat[1], width = 2), name = "14-day average",
                  connectgaps = FALSE,
                  hovertemplate = "%{x|%d %b %Y}<br><b>%{y:.0f}</b> cyclists<extra></extra>") |>
        plotly_base(ylab = "Cyclists per day", legend = FALSE) |>
        layout(shapes = shapes)
    })

    output$profile <- renderPlotly({
      s <- req(input$sensor)
      b <- req(bundle())
      latest <- max(b$counts$date, na.rm = TRUE)
      prof <- counter_profile(s, latest - 365, latest)
      validate(need(nrow(prof) > 0, "No 15-minute data for this counter."))

      wide <- reshape(prof[, c("slot", "kind", "v")], idvar = "slot",
                      timevar = "kind", direction = "wide")
      names(wide) <- sub("^v\\.", "", names(wide))
      wide$time <- as.POSIXct(wide$slot * 900, origin = "1970-01-01", tz = "UTC")

      p <- plot_ly()
      if ("Weekday" %in% names(wide))
        p <- add_trace(p, data = wide, x = ~time, y = ~Weekday, type = "scatter",
                       mode = "lines", name = "Weekday",
                       line = list(color = PAL$cat[1], width = 2.2),
                       hovertemplate = "%{x|%H:%M} weekday<br><b>%{y:.1f}</b><extra></extra>")
      if ("Weekend" %in% names(wide))
        p <- add_trace(p, data = wide, x = ~time, y = ~Weekend, type = "scatter",
                       mode = "lines", name = "Weekend",
                       line = list(color = PAL$cat[2], width = 2.2, dash = "dot"),
                       hovertemplate = "%{x|%H:%M} weekend<br><b>%{y:.1f}</b><extra></extra>")

      plotly_base(p, ylab = "Cyclists per 15 min", legend = TRUE) |>
        layout(xaxis = list(tickformat = "%H:%M", dtick = 3 * 3600 * 1000,
                            gridcolor = PAL$grid, linecolor = PAL$axis,
                            tickfont = list(size = 11, color = PAL$muted)))
    })

    output$table <- renderDT({
      d <- counts()
      s <- sensor_summary(d)
      act <- active_sensors(d)
      tab <- data.frame(
        Counter = s$sensor,
        Status  = ifelse(s$sensor %in% act, "Reporting", "Decommissioned"),
        `Cyclists (total)` = s$cyc_total,
        `Mean/day` = round(s$cyc_mean),
        `Busiest day` = s$cyc_max,
        `Uptime %` = round(100 * pmin(1, s$coverage)),
        `Outage days` = s$outages,
        From = format(s$first, "%b %Y"),
        To = format(s$last, "%b %Y"),
        check.names = FALSE, stringsAsFactors = FALSE
      )
      datatable(tab, rownames = FALSE, class = "compact stripe hover",
                options = list(pageLength = 12, dom = "ftip", scrollX = TRUE,
                               order = list(list(2, "desc")))) |>
        formatCurrency(c("Cyclists (total)", "Mean/day", "Busiest day"), "", digits = 0) |>
        formatStyle("Status", color = styleEqual(
          c("Reporting", "Decommissioned"), c(PAL$status[["good"]], PAL$muted)))
    })
  })
}

#' Mean cyclists per 15-minute slot for one counter, weekday vs weekend.
#'
#' Aggregated server side -- the underlying table is 10.4M rows.
counter_profile <- function(sensor, from, to, force = FALSE) {
  key <- sprintf("-cprof-%s-%s", substr(gsub("[^A-Za-z0-9]", "", sensor), 1, 24),
                 format(as.Date(to), "%Y%m"))
  cached_fetch("intraday", suffix = key, force = force, seed = function() data.frame(), query = function() {
    esc <- gsub("'", "''", sensor)
    df <- soql_fetch(
      DATASETS$intraday$id,
      select = paste(
        "date_extract_dow(log_timestamp) AS dow",
        "date_extract_hh(log_timestamp) AS hh",
        "avg(total_cyclist_count) AS v",
        sep = ", "
      ),
      # The portal may still carry the pre-rename label, so match on prefix.
      where = sprintf(
        "starts_with(counter_location_description, '%s') AND log_timestamp >= '%s' AND log_timestamp < '%s'",
        esc, as.Date(from), as.Date(to)),
      group = "date_extract_dow(log_timestamp), date_extract_hh(log_timestamp)"
    )
    if (!nrow(df)) return(data.frame())
    dow <- as.integer(parse_count(df$dow))
    data.frame(
      slot = as.integer(parse_count(df$hh)) * 4L,
      kind = ifelse(dow %in% c(0, 6), "Weekend", "Weekday"),
      v    = parse_count(df$v),
      stringsAsFactors = FALSE
    ) |> (function(z) stats::aggregate(v ~ slot + kind, data = z, FUN = mean))()
  })
}
