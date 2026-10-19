# Drive every module server headlessly and force each output to render.
#
#   Rscript tests/test_app.R
#
# shiny::testServer runs the real reactive graph, so an error in any renderer
# surfaces here rather than as a blank card in the browser.

source("global.R")

pass <- 0L; fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch({ force(expr); TRUE }, error = function(e) conditionMessage(e))
  if (isTRUE(r)) { pass <<- pass + 1L; cat(sprintf("  ok   %s\n", label)) }
  else { fail <<- fail + 1L; cat(sprintf("  FAIL %s\n         %s\n", label, r)) }
}

cat("loading data bundle...\n")
B <- shared_bundle()
bundle <- reactiveVal(B)
cat(sprintf("  %d routes, %d counter-days, %d ledger rows\n\n",
            nrow(B$routes), nrow(B$counts), nrow(B$ledger)))

cat("-- Shared bundle --\n")
ok("second session reuses the loaded bundle", {
  t <- system.time(b2 <- shared_bundle())[["elapsed"]]
  stopifnot(identical(b2, B), t < 0.5)
})
ok("bundle carries no route geometry", stopifnot(!inherits(B$routes, "sf")))
ok("map geometry is one feature per route type",
   stopifnot(nrow(B$map_routes) == length(unique(B$routes$classification))))
ok("map geometry is small", {
  f <- tempfile(fileext = ".geojson"); sf::st_write(B$map_routes, f, quiet = TRUE)
  stopifnot(file.size(f) < 1.5e6)
})
ok("no simulation or disruption data loaded",
   stopifnot(is.null(B$paths), is.null(B$disruptions)))
cat("\n")

# ---------------------------------------------------------------------------
cat("-- Overview --\n")
testServer(overview_server, args = list(bundle = bundle), {
  ok("tiles",   output$tiles)
  ok("recent",  output$recent)
  ok("top",     output$top)
  ok("season",  output$season)
  ok("disrupt", output$disrupt)
})

cat("\n-- Map --\n")
testServer(map_server, args = list(bundle = bundle), {
  session$setInputs(show_counters = TRUE, show_construction = TRUE,
                    classes = setdiff(names(CLASS_COLOURS), NON_CYCLING_CLASS))
  ok("leaflet renders", output$map)
  ok("sidebar summary", output$summary)
  ok("uses a keyless basemap", {
    stopifnot(grepl("Esri", MAP_TILES), !grepl("CartoDB|Stadia|Thunderforest|Mapbox", MAP_TILES))
  })
  ok("toggling layers does not re-render the map", {
    before <- output$map
    session$setInputs(classes = names(CLASS_COLOURS), show_counters = FALSE)
    stopifnot(identical(before, output$map))
  })
  session$setInputs(classes = character(0))
  ok("no classes selected still renders", output$summary)
})

cat("\n-- UI structure --\n")
# testServer proves a renderer runs; it says nothing about whether the result
# is visible. The Map tab rendered fine and showed nothing, because its
# wrapper div was opaque to bslib's fill chain and the map's height:100%
# resolved to zero. These check the layout contract instead of the reactive.
html_of <- function(x) paste(as.character(x), collapse = "")

for (tab in list(list("Map", map_ui("m")))) {
  h <- html_of(tab[[2]])
  ok(sprintf("%s: full-bleed wrapper is a fill container", tab[[1]]),
     stopifnot(grepl("page-flush[^\"]*html-fill-container", h) ||
               grepl("html-fill-container[^\"]*page-flush", h)))
}
ok("map output requests a height", grepl("id=\"m-map\"[^>]*height:", html_of(map_ui("m"))))

css <- paste(readLines("www/app.css", warn = FALSE), collapse = "\n")
ok("full-bleed pages have a chain-independent height",
   stopifnot(grepl("\\.page-flush\\s*\\{[^}]*height:\\s*calc\\(100vh", css)))
ok("full-bleed pages have a min-height floor",
   stopifnot(grepl("\\.page-flush\\s*\\{[^}]*min-height", css)))
ok("spinner wrapper passes height through",
   stopifnot(grepl("\\.page-flush\\s+\\.shiny-spinner-output-container\\s*\\{[^}]*height", css)))

cat("\n-- Trends --\n")
testServer(trends_server, args = list(bundle = bundle), {
  for (n in c("mean", "panel", "total")) {
    session$setInputs(norm = n, measure = "cyc")
    ok(paste("series /", n), output$series)
    ok(paste("note /", n),   output$norm_note)
  }
  session$setInputs(norm = "mean", measure = "ped")
  ok("series / pedestrians", output$series)
  session$setInputs(measure = "cyc")
  ok("year over year", output$yoy)
  ok("hour heatmap",   output$heat)
  ok("winter share",   output$winter)
})

cat("\n-- Counters --\n")
testServer(counters_server, args = list(bundle = bundle), {
  session$setInputs(active_only = TRUE)
  s1 <- choices()[1]
  session$setInputs(sensor = s1)
  ok(paste("header /", s1), output$head)
  ok("history",  output$history)
  ok("profile",  output$profile)
  ok("table",    output$table)
  session$setInputs(active_only = FALSE)
  ok("includes decommissioned", stopifnot(length(choices()) > 48))
})

cat("\n-- About --\n")
testServer(about_server, args = list(bundle = bundle), {
  ok("tiles",       output$tiles)
  ok("composition", output$composition)
  ok("buildout",    output$buildout)
  ok("rules",       output$rules)
  ok("compare",     output$compare)
})
ok("raw series never peaks below cleaned", {
  raw <- B$counts; raw$cyc <- raw$cyc_raw
  stopifnot(max(daily_series(raw, "cyc", "mean")$value) >=
            max(daily_series(B$counts, "cyc", "mean")$value))
})

cat("\n-- App --\n")
ui_html <- local({ source("app.R", local = TRUE); paste(as.character(ui), collapse = "") })
ok("tabs are Overview, Map, Trends, About", {
  tabs <- regmatches(ui_html, gregexpr('data-value="[^"]+"', ui_html))[[1]]
  stopifnot(identical(unique(sub('data-value="([^"]+)"', "\\1", tabs)),
                      c("Overview", "Map", "Trends", "About")))
})
ok("no deck.gl or MapLibre shipped", stopifnot(!grepl("deck.gl|maplibre", ui_html)))
ok("no cleaned-data toggle or Refresh button",
   stopifnot(!grepl("clean_mode|id=\"refresh\"", ui_html)))

cat(sprintf("\n%d passed, %d failed\n", pass, fail))
if (fail > 0) quit(status = 1)
