# ---------------------------------------------------------------------------
# Data quality -- every correction the app makes, on the record.
#
# The counter data has real defects: a sensor that reported 86,235 cyclists in
# a February, another that reported 477,654 pedestrians and no cyclists, a
# handful of duplicated days, and two counters that were renamed mid-series and
# so were being counted as four. Handling those quietly would make the app
# easier to trust and harder to check. This tab shows the rules, what each one
# caught, and what the numbers look like without them.
# ---------------------------------------------------------------------------

quality_ui <- function(id) {
  ns <- NS(id)
  div(class = "page",
    div(class = "page-head",
      h2("Data quality"),
      p(class = "lede",
        "Everything the app corrects in the City's counter data, and what it looks like uncorrected.")
    ),
    uiOutput(ns("tiles")),
    card(
      card_header("The rules"),
      withSpinner(DTOutput(ns("rules")), type = 8, color = PAL$cat[1]),
      card_footer(class = "card-note",
        "Rules marked ‘alters data’ replace a value with a gap. The rest annotate only. Use the ",
        tags$strong("Cleaned data"), " switch in the header to see every chart in the app without any of this applied.")
    ),
    div(class = "grid-2",
      card(full_screen = TRUE,
        card_header("Before and after"),
        withSpinner(plotlyOutput(ns("compare"), height = "320px"), type = 8, color = PAL$cat[1]),
        card_footer(class = "card-note",
          "City-wide daily cyclists per counter. The raw line's spikes are single malfunctioning sensors, not busy days.")
      ),
      card(full_screen = TRUE,
        card_header("Where the corrections land"),
        withSpinner(plotlyOutput(ns("bysensor"), height = "320px"), type = 8, color = PAL$cat[1]),
        card_footer(class = "card-note",
          "Corrections are concentrated in a handful of counters, which is what you would expect from hardware faults rather than a systematic bias.")
      )
    ),
    card(full_screen = TRUE,
      card_header("Every flagged reading"),
      div(class = "control-row",
        selectInput(ns("rule_filter"), NULL, choices = c("All rules" = ""), width = "320px"),
        downloadButton(ns("download"), "Download CSV", class = "btn btn-sm btn-ghost")
      ),
      withSpinner(DTOutput(ns("ledger")), type = 8, color = PAL$cat[1])
    ),
    card(
      card_header("Counters merged under one name"),
      withSpinner(DTOutput(ns("aliases")), type = 8, color = PAL$cat[1]),
      card_footer(class = "card-note",
        "The city renamed two counters mid-series without changing their coordinates. Treating the old and new labels as separate sites splits each history in two and double-counts the location in any per-counter ranking.")
    )
  )
}

quality_server <- function(id, bundle) {
  moduleServer(id, function(input, output, session) {

    led <- reactive(req(bundle())$ledger)

    observeEvent(led(), {
      l <- led()
      ch <- c("All rules" = "", stats::setNames(
        unique(l$rule), sprintf("%s — %s", unique(l$rule), RULE_LABELS[unique(l$rule)])))
      updateSelectInput(session, "rule_filter", choices = ch)
    }, once = TRUE)

    output$tiles <- renderUI({
      b <- req(bundle())
      s <- b$qa_summary
      n_rows <- nrow(b$counts)
      destructive <- sum(s$n[s$destructive])
      sensors_hit <- length(unique(b$ledger$sensor[b$ledger$destructive]))

      div(class = "tiles",
        stat_tile(fmt_int(n_rows), "Counter-days in the record", accent = PAL$cat[1]),
        stat_tile(fmt_int(destructive), "Readings replaced with a gap",
                  sub = sprintf("%.2f%% of the record", 100 * destructive / n_rows),
                  accent = PAL$status[["serious"]]),
        stat_tile(sensors_hit, "Counters affected",
                  sub = sprintf("of %d", length(unique(b$counts$sensor))),
                  accent = PAL$cat[4]),
        stat_tile(fmt_int(sum(s$n[s$rule == "R9"])), "Borderline, kept",
                  sub = "flagged for review, not removed", accent = PAL$cat[5])
      )
    })

    output$rules <- renderDT({
      b <- req(bundle())
      s <- b$qa_summary
      why <- c(
        R1 = "Two labels at identical coordinates with non-overlapping dates are the same counter, renamed.",
        R2 = "The same counter and day appearing twice would be summed twice. The larger reading is kept.",
        R3 = "The City flags counter outages in its 15-minute feed. A dead counter reads as a zero, which is worse than a gap.",
        R4 = "On hardware that watches both modes, one mode dwarfing the other by 20x is a fault, not a crowd.",
        R5 = "A reading far beyond that counter's own Tukey fence and its seasonal norm. Per-counter, because volumes differ a hundredfold.",
        R6 = "Blank in the source data.",
        R7 = "A zero with no outage flag. Could be a genuinely quiet day or a silent failure — tagged, never altered.",
        R9 = "Above a single fence but below the threshold to remove. Kept, and listed here so it can be argued with."
      )
      tab <- data.frame(
        Rule = s$rule,
        What = s$label,
        Readings = s$n,
        Effect = ifelse(s$destructive, "Alters data", "Annotates only"),
        Why = unname(why[s$rule]),
        check.names = FALSE, stringsAsFactors = FALSE
      )
      datatable(tab, rownames = FALSE, class = "compact stripe",
                options = list(dom = "t", pageLength = 20,
                               columnDefs = list(list(width = "42%", targets = 4)))) |>
        formatCurrency("Readings", "", digits = 0) |>
        formatStyle("Effect", fontWeight = styleEqual("Alters data", "600"),
                    color = styleEqual(c("Alters data", "Annotates only"),
                                       c(PAL$status[["serious"]], PAL$muted)))
    })

    output$compare <- renderPlotly({
      b <- req(bundle())
      raw   <- daily_series(counts_view(b$counts, FALSE), "cyc", "mean")
      clean <- daily_series(counts_view(b$counts, TRUE),  "cyc", "mean")

      plot_ly() |>
        add_trace(data = raw, x = ~date, y = ~value, type = "scatter", mode = "lines",
                  name = "Raw", line = list(color = PAL$cat[2], width = 1.4),
                  hovertemplate = "%{x|%d %b %Y}<br>Raw <b>%{y:.0f}</b><extra></extra>") |>
        add_trace(data = clean, x = ~date, y = ~value, type = "scatter", mode = "lines",
                  name = "Cleaned", line = list(color = PAL$cat[1], width = 1.6),
                  hovertemplate = "%{x|%d %b %Y}<br>Cleaned <b>%{y:.0f}</b><extra></extra>") |>
        plotly_base(ylab = "Cyclists per counter per day", legend = TRUE)
    })

    output$bysensor <- renderPlotly({
      l <- led()
      l <- l[l$destructive, ]
      validate(need(nrow(l) > 0, "No corrections applied."))
      tab <- as.data.frame(table(l$sensor), stringsAsFactors = FALSE)
      names(tab) <- c("sensor", "n")
      tab <- utils::head(tab[order(-tab$n), ], 12)
      tab$sensor <- factor(tab$sensor, levels = rev(tab$sensor))

      plot_ly(tab, y = ~sensor, x = ~n, type = "bar", orientation = "h",
              marker = list(color = PAL$status[["serious"]],
                            line = list(color = PAL$surface, width = 2)),
              text = ~n, textposition = "auto",
              insidetextfont = list(color = "#fff", size = 11),
              outsidetextfont = list(color = PAL$ink2, size = 11),
              hovertemplate = "<b>%{y}</b><br>%{x} readings corrected<extra></extra>") |>
        plotly_base(xlab = "Readings corrected", legend = FALSE, hovermode = "closest") |>
        layout(yaxis = list(title = "", tickfont = list(size = 10.5, color = PAL$ink2),
                            gridcolor = "rgba(0,0,0,0)", linecolor = "rgba(0,0,0,0)"))
    })

    filtered <- reactive({
      l <- led()
      f <- input$rule_filter
      if (!is.null(f) && nzchar(f)) l <- l[l$rule == f, ]
      l[!is.na(l$date), ]
    })

    output$ledger <- renderDT({
      l <- filtered()
      validate(need(nrow(l) > 0, "Nothing flagged under this rule."))
      tab <- data.frame(
        Rule = l$rule,
        Counter = l$sensor,
        Date = format(l$date, "%Y-%m-%d"),
        Measure = l$measure,
        `Raw value` = l$raw_value,
        Action = l$action,
        check.names = FALSE, stringsAsFactors = FALSE
      )
      tab <- tab[order(-ifelse(is.na(tab$`Raw value`), -1, tab$`Raw value`)), ]
      datatable(tab, rownames = FALSE, class = "compact stripe hover",
                options = list(pageLength = 12, dom = "ftip", scrollX = TRUE)) |>
        formatCurrency("Raw value", "", digits = 0)
    })

    output$download <- downloadHandler(
      filename = function() sprintf("yegbike-data-quality-%s.csv", Sys.Date()),
      content = function(file) utils::write.csv(filtered(), file, row.names = FALSE)
    )

    output$aliases <- renderDT({
      b <- req(bundle())
      a <- b$aliases
      a <- a[a$sensor_raw != a$sensor, ]
      validate(need(nrow(a) > 0, "No counters needed merging."))
      d <- b$counts
      rng <- do.call(rbind, lapply(seq_len(nrow(a)), function(i) {
        sub <- d[d$sensor == a$sensor[i], ]
        data.frame(from = min(sub$date), to = max(sub$date))
      }))
      datatable(
        data.frame(
          `Label in the source data` = a$sensor_raw,
          `Merged into` = a$sensor,
          Latitude = round(a$lat, 5),
          Longitude = round(a$lon, 5),
          `Combined record` = sprintf("%s to %s", format(rng$from, "%b %Y"), format(rng$to, "%b %Y")),
          check.names = FALSE, stringsAsFactors = FALSE
        ),
        rownames = FALSE, class = "compact stripe", options = list(dom = "t"))
    })
  })
}
