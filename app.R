# ---------------------------------------------------------------------------
# YEG Bike -- Edmonton bike lane status and ridership
#
# Data is pulled from data.edmonton.ca once per server process and shared by
# every session; the first visitor after it goes an hour stale triggers an
# incremental reload. Everything on disk under cache/ is a convenience, never
# a source of truth.
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
  fillable = "Map",

  header = tagList(
    tags$head(
      tags$link(rel = "stylesheet", href = "app.css"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
    ),
    div(class = "app-status", uiOutput("status_bar"))
  ),

  nav_panel("Overview", overview_ui("ov")),
  nav_panel("Map",      map_ui("map")),
  nav_panel("Trends",   trends_ui("tr"), counters_ui("ct")),
  nav_panel("About",    about_ui("ab"))
)

server <- function(input, output, session) {

  # -- data bundle --------------------------------------------------------
  # Shared across sessions (see shared_bundle() in R/data.R). Only the visitor
  # who finds it missing or stale waits, and they get a progress bar.
  bundle <- reactiveVal(
    if (shared_bundle_fresh()) shared_bundle()
    else withProgress(message = "Loading Edmonton open data", value = 0, {
      shared_bundle(notify = function(label, frac) setProgress(value = frac, detail = label))
    })
  )

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
        sprintf("Could not reach the portal for: %s. Showing the last good copy.",
                paste(failed, collapse = ", "))
      ))
    }
    bits
  })

  # -- modules ------------------------------------------------------------
  overview_server("ov", bundle)
  map_server("map", bundle)
  trends_server("tr", bundle)
  counters_server("ct", bundle)
  about_server("ab", bundle)
}

shinyApp(ui, server)
