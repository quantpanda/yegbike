# Generate the artifacts shipped with a deployment.
#
#   Rscript scripts/build_deploy_data.R
#
# Writes data/:
#   routes_by_class.rds  merged, simplified route geometry for the Map tab
#   routes.rds           last-resort seed if the portal is unreachable at cold start
#   daily.rds            ditto
#
# About 1.4 MB in total, against 13.4 MB for the equivalent CSV exports. Re-run
# after the city publishes new route geometry, and occasionally to keep the
# seed from drifting too far from current data.

suppressPackageStartupMessages(library(sf))
for (f in c("socrata", "clean", "data")) source(file.path("R", paste0(f, ".R")))

dir.create("data", showWarnings = FALSE)

cat("fetching (full history, ~30s)...\n")
routes  <- load_routes(force = TRUE)
both    <- load_counts_and_outages(force = TRUE)
daily   <- both$counts
outages <- both$outages
meta    <- load_counter_meta(force = TRUE)
cleaned <- clean_counts(daily, outages, meta)
cat(sprintf("  %s counter-days through %s, %d counters\n",
            format(nrow(daily), big.mark = ","), max(daily$date, na.rm = TRUE),
            length(unique(cleaned$data$sensor))))

cat("writing baselines...\n")
# These are starting points, not just outage fallbacks: at runtime the app
# loads them and asks the portal only for rows newer than their last date.
# Refetching 112k settled rows on every cold start cost about 64 seconds.
#
# xz is slow to write and small to ship -- the right trade for an artifact
# built once and downloaded on every deploy.
saveRDS(routes,  "data/routes.rds",  compress = "xz")
saveRDS(daily,   "data/daily.rds",   compress = "xz")
saveRDS(outages, "data/outages.rds", compress = "xz")

# Map geometry: one simplified multi-line per route type. Sending 9,860
# separate segments was 5.4 MB of GeoJSON per visitor; merged, it is under 1 MB.
cat("merging route geometry for the map...\n")
saveRDS(merge_routes_by_class(routes), "data/routes_by_class.rds", compress = "xz")

for (f in list.files("data", full.names = TRUE)) {
  cat(sprintf("  %-22s %5.1f MB\n", f, file.size(f) / 1e6))
}
cat(sprintf("\ntotal %.1f MB\n", sum(file.size(list.files("data", full.names = TRUE))) / 1e6))
