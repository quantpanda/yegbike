# ---------------------------------------------------------------------------
# YEG Bike -- Edmonton bike lane status and ridership
#
# Data is pulled from data.edmonton.ca at startup and re-pulled on demand from
# the Refresh control in the header. Everything on disk under cache/ is a
# convenience, never a source of truth.
#
# Run:  Rscript -e 'shiny::runApp(".", port = 8080)'
# ---------------------------------------------------------------------------

# Load libraries and helpers. Guarded because some launch paths source
# global.R for us and some do not; see the note in global.R.
if (!isTRUE(get0(".yegbike_loaded", envir = globalenv(), ifnotfound = FALSE))) {
  source("global.R")
}

ui <- page_navbar(
  title = span(class = "brand", span(class = "brand-mark", "●"), "YEG Bike"),
  id = "nav",
  window_title = "YEG Bike — Edmonton bike lane status",
  theme = bs_theme(
    version = 5,
    base_font = font_google("Inter", local = FALSE),
    "body-bg" = PAL$plane,
    "body-color" = PAL$ink,
    primary = PAL$cat[1],
    "border-color" = PAL$grid
  ),
  fillable = c("Simulation", "Map"),

  header = tagList(
    tags$head(
      tags$link(rel = "stylesheet", href = "app.css"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
    ),
    div(class = "app-status", uiOutput("status_bar"))
  ),

  nav_panel("Overview",    overview_ui("ov")),
  nav_panel("Map",         map_ui("map")),
  nav_panel("Simulation",  simulation_ui("sim")),
  nav_panel("Trends",      trends_ui("tr")),
  nav_panel("Counters",    counters_ui("ct")),
  nav_panel("Network",     network_ui("nw")),
  nav_panel("Data quality", quality_ui("qa")),

  nav_spacer(),
  nav_item(
    div(
      class = "nav-tools",
      div(class = "clean-switch",
          tags$label(`for` = "clean_mode", "Cleaned data"),
          tags$input(type = "checkbox", id = "clean_mode", class = "toggle", checked = NA)),
      actionButton("refresh", label = tagList(icon("rotate"), "Refresh"),
                   class = "btn btn-sm btn-refresh")
    )
  )
)

server <- function(input, output, session) {

  # -- data bundle --------------------------------------------------------
  bundle <- reactiveVal(NULL)

  load_bundle <- function(force) {
    withProgress(message = if (force) "Refreshing from data.edmonton.ca" else "Loading Edmonton open data",
                 value = 0, {
      b <- app_data(force = force, notify = function(label, frac) {
        setProgress(value = frac, detail = label)
      })
      bundle(b)
    })
  }

  # Pull on load, as required -- the disk cache only short-circuits the network
  # inside each dataset's TTL, and Refresh always bypasses it.
  load_bundle(force = FALSE)

  observeEvent(input$refresh, {
    load_bundle(force = TRUE)
    showNotification("Data refreshed from the City of Edmonton.",
                     type = "message", duration = 4)
  })

  clean_mode <- reactive(isTRUE(input$clean_mode))

  # -- header status ------------------------------------------------------
  output$status_bar <- renderUI({
    b <- bundle()
    if (is.null(b)) return(NULL)

    log <- b$fetch_log
    degraded <- vapply(log, function(x) x$status %in% c("stale", "seed"), logical(1))
    live_at <- b$fetched_at
    latest <- suppressWarnings(max(b$counts$date, na.rm = TRUE))

    bits <- tagList(
      span(class = "status-item",
           span(class = "status-dot", style = sprintf("background:%s",
                if (any(degraded)) PAL$status[["warning"]] else PAL$status[["good"]])),
           if (any(degraded)) "Showing cached data" else "Live from data.edmonton.ca"),
      span(class = "status-item status-muted",
           sprintf("Fetched %s", format(live_at, "%d %b %Y, %H:%M"))),
      span(class = "status-item status-muted",
           sprintf("Counts through %s", format(latest, "%d %b %Y")))
    )

    if (any(degraded)) {
      failed <- names(log)[degraded]
      bits <- tagList(bits, div(
        class = "status-warn",
        sprintf("Could not reach the portal for: %s. Showing the last good copy — press Refresh to retry.",
                paste(failed, collapse = ", "))
      ))
    }
    bits
  })

  # -- modules ------------------------------------------------------------
  overview_server("ov", bundle, clean_mode, session)
  map_server("map", bundle, clean_mode)
  simulation_server("sim", bundle)
  trends_server("tr", bundle, clean_mode)
  counters_server("ct", bundle, clean_mode)
  network_server("nw", bundle)
  quality_server("qa", bundle)
}

shinyApp(ui, server)
