# ---------------------------------------------------------------------------
# Simulation -- a day of Edmonton cycling, animated.
#
# Two modes behind one toggle. "Measured" draws only what the counters saw.
# "Simulated" moves riders along real bike-route geometry at a rate set by
# those same counts. The distinction is the point of the tab, so it is stated
# in the UI rather than buried in a methodology note.
# ---------------------------------------------------------------------------

simulation_ui <- function(id) {
  ns <- NS(id)
  div(class = "page page-flush",
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        width = 310, title = "Day to animate",
        dateInput(ns("day"), NULL, value = Sys.Date() - 1,
                  min = as.Date("2018-08-01"), max = Sys.Date(),
                  format = "D d M yyyy", width = "100%"),
        div(class = "btn-row",
          actionButton(ns("busiest"), "Busiest day", class = "btn btn-sm btn-ghost"),
          actionButton(ns("yesterday"), "Yesterday", class = "btn btn-sm btn-ghost")
        ),
        hr(),
        uiOutput(ns("daystats")),
        hr(),
        div(class = "sidebar-note",
          tags$p(tags$strong("Measured"), " shows each counter pulsing with its own 15-minute reading. Nothing is inferred."),
          tags$p(tags$strong("Simulated"), " keeps those counts but renders them as movement: a rider is spawned for every few cyclists recorded and sent along real bike-route geometry in the direction the counter registered."),
          tags$p(class = "sidebar-caution",
            "The routes any individual rider takes are invented — the counters record a crossing, not a journey. Volumes, timing and direction are real.")
        )
      ),
      div(class = "sim-wrap",
        withSpinner(deckmapOutput(ns("deck"), height = "100%"), type = 8, color = PAL$cat[1]))
    )
  )
}

simulation_server <- function(id, bundle) {
  moduleServer(id, function(input, output, session) {

    observeEvent(bundle(), {
      b <- bundle(); req(b$counts)
      latest <- max(b$counts$date, na.rm = TRUE)
      updateDateInput(session, "day", value = latest, max = latest)
    }, once = TRUE)

    observeEvent(input$yesterday, {
      b <- req(bundle())
      updateDateInput(session, "day", value = max(b$counts$date, na.rm = TRUE))
    })

    observeEvent(input$busiest, {
      b <- req(bundle())
      bd <- busiest_day(b$counts)
      validate(need(!is.null(bd), "No counts available."))
      updateDateInput(session, "day", value = bd$date)
      showNotification(
        sprintf("Busiest day on record: %s — %s cyclists across %d counters.",
                format(bd$date, "%A %d %B %Y"), fmt_int(bd$total), bd$n_sensors),
        type = "message", duration = 6)
    })

    payload <- reactive({
      b <- req(bundle())
      day <- req(input$day)
      withProgress(message = "Building the day", value = 0.3, {
        setProgress(0.4, detail = "Fetching 15-minute counts")
        p <- build_sim_payload(day, b$paths, b$meta)
        setProgress(1)
        p
      })
    })

    output$deck <- renderDeckmap({
      p <- payload()
      validate(need(p$n_rows > 0,
                    "No 15-minute readings for that date. Try a date within the last few years."))
      p
    })

    output$daystats <- renderUI({
      p <- payload()
      if (p$n_rows == 0) return(div(class = "sidebar-note", "No readings for this date."))
      m <- p$measured
      total <- sum(vapply(m$counters, function(c) c$total, numeric(1)))
      busiest <- m$counters[[which.max(vapply(m$counters, function(c) c$total, numeric(1)))]]

      # Peak 15-minute slot across the whole city, as a clock time.
      bins <- Reduce(`+`, lapply(m$counters, function(c) c$cyc))
      pk <- which.max(bins)
      pk_time <- sprintf("%02d:%02d", ((pk - 1) * 15) %/% 60, ((pk - 1) * 15) %% 60)

      tagList(
        div(class = "sidebar-stat",
            span(class = "sidebar-stat-value", fmt_int(total)),
            span(class = "sidebar-stat-label", "cyclists counted city-wide")),
        div(class = "sidebar-stat",
            span(class = "sidebar-stat-value", pk_time),
            span(class = "sidebar-stat-label",
                 sprintf("peak quarter-hour (%s bikes)", fmt_int(max(bins))))),
        div(class = "sidebar-stat",
            span(class = "sidebar-stat-value", substr(busiest$name, 1, 26)),
            span(class = "sidebar-stat-label",
                 sprintf("busiest counter — %s", fmt_int(busiest$total))))
      )
    })
  })
}
