# ---------------------------------------------------------------------------
# global.R -- libraries and source files. No side effects.
#
# Shiny sources this automatically before app.R. The exploratory analysis this
# file used to contain has moved to notes/eda.R, which is deliberately not
# sourced: it ran two base plots and five ggplot builds over 97k rows at every
# startup, twice, because app.R also sourced this file explicitly.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(leaflet)
  library(plotly)
  library(DT)
  library(sf)
  library(htmltools)
  library(shinycssloaders)
})

# jsonlite, httr2, readr and igraph are deliberately NOT attached -- every call
# to them in this project is already namespaced, and attaching them masks
# functions we rely on. jsonlite::validate in particular shadows
# shiny::validate and takes a character argument, which turns every
# validate(need(...)) guard in the app into "is.character(txt) is not TRUE".
requireNamespace("jsonlite", quietly = TRUE)
requireNamespace("httr2",    quietly = TRUE)
requireNamespace("readr",    quietly = TRUE)
requireNamespace("igraph",   quietly = TRUE)

# Belt and braces: pin the two guards to Shiny's versions so a future
# library() call cannot silently re-break them.
validate <- shiny::validate
need     <- shiny::need

for (f in c("socrata", "clean", "network", "sim", "theme", "data",
            "deckmap_widget",
            "mod_overview", "mod_map", "mod_simulation", "mod_trends",
            "mod_counters", "mod_network", "mod_quality")) {
  source(file.path("R", paste0(f, ".R")), local = FALSE)
}

# Larger uploads/downloads over the websocket -- the simulation payload for a
# busy summer day is a couple of megabytes before compression.
options(shiny.maxRequestSize = 64 * 1024^2)

# Whether Shiny auto-sources global.R before app.R turns out to depend on how
# the app is launched -- under `Rscript -e 'shiny::runApp(".")'` it does not.
# app.R therefore sources this file itself, and this flag keeps that from
# running twice where Shiny does source it. (The original app.R sourced
# global.R unconditionally at the bottom, which ran the whole file twice.)
.yegbike_loaded <- TRUE

#' Choose raw or cleaned count columns.
#'
#' Every tab reads counts through this, so the Data Quality tab's raw/cleaned
#' switch propagates everywhere without each module knowing about it.
counts_view <- function(counts, clean = TRUE) {
  if (isTRUE(clean)) return(counts)
  counts$cyc <- counts$cyc_raw
  counts$ped <- counts$ped_raw
  counts
}
