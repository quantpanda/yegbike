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
B <- app_data(force = FALSE)
bundle <- reactiveVal(B)
clean_on <- reactive(TRUE)
cat(sprintf("  %d routes, %d counter-days, %d ledger rows\n\n",
            nrow(B$routes), nrow(B$counts), nrow(B$ledger)))

# ---------------------------------------------------------------------------
cat("-- Overview --\n")
testServer(overview_server, args = list(bundle = bundle, clean_mode = clean_on), {
  ok("tiles",   output$tiles)
  ok("recent",  output$recent)
  ok("top",     output$top)
  ok("season",  output$season)
  ok("disrupt", output$disrupt)
})

cat("\n-- Map --\n")
testServer(map_server, args = list(bundle = bundle, clean_mode = clean_on), {
  session$setInputs(cycling_only = TRUE, show_counters = TRUE,
                    show_construction = TRUE, show_disruptions = TRUE,
                    classes = setdiff(names(CLASS_COLOURS), NON_CYCLING_CLASS))
  ok("leaflet base", output$map)
  ok("sidebar summary", output$summary)
  ok("shown_routes non-empty", stopifnot(nrow(shown_routes()) > 1000))
  session$setInputs(cycling_only = FALSE, classes = names(CLASS_COLOURS))
  ok("all classes render", output$summary)
})

cat("\n-- Simulation --\n")
testServer(simulation_server, args = list(bundle = bundle), {
  session$setInputs(day = max(B$counts$date, na.rm = TRUE))
  ok("payload builds", {
    p <- payload()
    stopifnot(p$n_rows > 0, length(p$measured$counters) > 10)
  })
  ok("measured payload shape", {
    p <- payload()
    stopifnot(all(vapply(p$measured$counters, function(c) length(c$cyc) == 96, logical(1))))
  })
  ok("simulated agents exist", {
    p <- payload()
    stopifnot(p$simulated$shown > 100, length(p$simulated$paths) > 10)
  })
  ok("agents reference valid paths", {
    p <- payload()
    stopifnot(min(p$simulated$agents$p) >= 0,
              max(p$simulated$agents$p) < length(p$simulated$paths))
  })
  ok("agent times inside the day", {
    p <- payload()
    stopifnot(min(p$simulated$agents$t) >= 0, max(p$simulated$agents$t) <= 1440)
  })
  ok("deck output", output$deck)
  ok("day stats", output$daystats)

  # The button itself can only be checked for not erroring: updateDateInput has
  # no client to round-trip through under testServer, so input$day never
  # changes here. The selection logic is tested directly below instead.
  session$setInputs(busiest = 1)
  ok("busiest-day button runs", TRUE)

  # Animate the actual busiest day and confirm it is worth animating.
  session$setInputs(day = busiest_day(B$counts)$date)
  ok("busiest day animates with a full agent set", {
    p <- payload()
    stopifnot(p$n_rows > 0, p$simulated$shown > 1000, p$simulated$total > 10000)
  })
})

cat("\n-- busiest day selection --\n")
bd <- busiest_day(B$counts)
ok("returns the global maximum by total riders", {
  s <- daily_series(B$counts, "cyc", "total")
  stopifnot(bd$date == s$date[which.max(s$value)], bd$total == max(s$value))
})
ok("busiest day has near-full counter coverage", {
  n <- length(unique(B$counts$sensor[B$counts$date == bd$date & !is.na(B$counts$cyc)]))
  stopifnot(n >= 40)
})
# The regression this replaces: the per-counter mean picked a five-counter day.
ok("beats the per-counter mean, which picks a near-empty day", {
  m <- daily_series(B$counts, "cyc", "mean")
  mean_pick <- m$n_sensors[which.max(m$value)]
  stopifnot(mean_pick < 10, bd$n_sensors > 4 * mean_pick)
})
cat(sprintf("  ..   busiest: %s, %s riders, %d counters\n",
            format(bd$date, "%a %d %b %Y"), fmt_int(bd$total), bd$n_sensors))

cat("\n-- Trends --\n")
testServer(trends_server, args = list(bundle = bundle, clean_mode = clean_on), {
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
testServer(counters_server, args = list(bundle = bundle, clean_mode = clean_on), {
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

cat("\n-- Network --\n")
testServer(network_server, args = list(bundle = bundle), {
  ok("tiles",       output$tiles)
  ok("composition", output$composition)
  ok("buildout",    output$buildout)
  ok("table",       output$table)
})

cat("\n-- Data quality --\n")
testServer(quality_server, args = list(bundle = bundle), {
  session$setInputs(rule_filter = "")
  ok("tiles",    output$tiles)
  ok("rules",    output$rules)
  ok("compare",  output$compare)
  ok("bysensor", output$bysensor)
  ok("ledger",   output$ledger)
  ok("aliases",  output$aliases)
  session$setInputs(rule_filter = "R5")
  ok("ledger filtered to R5", stopifnot(all(filtered()$rule == "R5")))
})

cat("\n-- raw/cleaned toggle propagates --\n")
clean_off <- reactive(FALSE)
testServer(trends_server, args = list(bundle = bundle, clean_mode = clean_off), {
  session$setInputs(norm = "mean", measure = "cyc")
  ok("renders with cleaning disabled", output$series)
  ok("raw series peaks higher than cleaned", {
    raw   <- daily_series(counts_view(B$counts, FALSE), "cyc", "mean")
    clean <- daily_series(counts_view(B$counts, TRUE),  "cyc", "mean")
    stopifnot(max(raw$value) > max(clean$value) * 1.5)
  })
})

cat(sprintf("\n%d passed, %d failed\n", pass, fail))
if (fail > 0) quit(status = 1)
