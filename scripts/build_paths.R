# Rebuild the routable bike graph and per-counter agent paths.
#
#   Rscript scripts/build_paths.R
#
# The app builds this automatically on first run, so you only need this script
# to force a rebuild after the city publishes new route geometry, or to inspect
# the diagnostics below.

suppressPackageStartupMessages({
  library(sf); library(igraph)
})
for (f in c("socrata", "clean", "network", "sim")) source(file.path("R", paste0(f, ".R")))

t0 <- Sys.time()
cat("loading route geometry...\n")
routes <- load_routes()
cat(sprintf("  %d features, %d after dropping non-cycling classes\n",
            nrow(routes), sum(!routes$classification %in% NON_CYCLING_CLASS)))

cat("loading counters...\n")
cleaned <- clean_counts(load_daily(), load_outages(), load_counter_meta())
counters <- recent_counters(cleaned$data)
cat(sprintf("  %d counters active in the last 90 days\n", nrow(counters)))

cat("building...\n")
cache <- build_path_cache(routes, counters, log = function(m) cat("  ", m, "\n"))

# --- diagnostics ---------------------------------------------------------
flat <- unlist(unname(cache$paths), recursive = FALSE) |> unlist(recursive = FALSE)
lens <- vapply(flat, seg_length_m, numeric(1))
cat(sprintf("  path length: median %.0f m, range %.0f-%.0f m\n",
            stats::median(lens), min(lens), max(lens)))
cat(sprintf("  vertices per path: median %.0f\n", stats::median(vapply(flat, nrow, integer(1)))))

# Grid snapping could in principle join two lines that merely pass near each
# other, so check that no path contains an implausible jump and that everything
# stays inside Edmonton.
maxhop <- vapply(flat, function(m) {
  if (nrow(m) < 2) return(0)
  lat <- m[, 2] * pi / 180
  dx <- diff(m[, 1]) * 111320 * cos((lat[-1] + lat[-length(lat)]) / 2)
  dy <- diff(m[, 2]) * 110540
  max(sqrt(dx^2 + dy^2))
}, numeric(1))
lons <- range(vapply(flat, function(m) m[1, 1], numeric(1)))
lats <- range(vapply(flat, function(m) m[1, 2], numeric(1)))

cat(sprintf("  largest hop within a path: %.0f m (grid %d m)\n", max(maxhop), cache$grid_m))
cat(sprintf("  extent: lon %.3f..%.3f, lat %.3f..%.3f\n", lons[1], lons[2], lats[1], lats[2]))
cat(sprintf("  counters with >=2 km of usable path: %d of %d\n",
            sum(vapply(cache$paths, function(p) {
              l <- unlist(lapply(p, function(w) vapply(w, seg_length_m, numeric(1))))
              length(l) > 0 && stats::median(l) >= 2000
            }, logical(1))), nrow(counters)))

stopifnot(max(maxhop) < 4 * cache$grid_m)
stopifnot(lons[1] > -114.0, lons[2] < -113.2, lats[1] > 53.3, lats[2] < 53.8)

cat(sprintf("\nwrote cache/paths.rds (%.1f MB) in %.0fs\n",
            file.size("cache/paths.rds") / 1e6,
            as.numeric(difftime(Sys.time(), t0, units = "secs"))))
