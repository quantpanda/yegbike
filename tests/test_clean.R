# Assertions for the data quality rules in R/clean.R.
#
#   Rscript tests/test_clean.R
#
# Two halves, deliberately:
#
#   1. Fixtures -- synthetic data with known defects injected. These pin rule
#      BEHAVIOUR and never drift.
#   2. Live data -- structural invariants only (no duplicates survive, removal
#      stays under a rate, the panel is a subset). No exact counts.
#
# The split exists because the first version of this file pinned real values
# and broke when the city corrected its own data: MacKinnon Ravine used to
# report 86,235 cyclists on 2022-02-02 and now reports 40. A test suite that
# fails when upstream data improves is testing the wrong thing.

suppressPackageStartupMessages({ library(sf) })
source("R/socrata.R")
source("R/clean.R")

pass <- 0L; fail <- 0L
ok <- function(label, cond, detail = "") {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat(sprintf("  ok   %s\n", label)) }
  else { fail <<- fail + 1L; cat(sprintf("  FAIL %s %s\n", label, detail)) }
}

# ---------------------------------------------------------------------------
# Fixture: a year of plausible counts for three counters, with defects planted.
# ---------------------------------------------------------------------------

make_fixture <- function() {
  set.seed(7)
  days <- seq(as.Date("2022-01-01"), as.Date("2022-12-31"), by = "day")
  # Seasonal shape so the per-month rules have something realistic to work on.
  seasonal <- function(d, peak) peak * (0.25 + 0.75 * sin(pi * (as.integer(format(d, "%j")) / 365)))

  mk <- function(name, lat, lon, peak, ped_ratio) {
    cyc <- pmax(0, round(seasonal(days, peak) * runif(length(days), 0.6, 1.4)))
    data.frame(sensor_raw = name, lat = lat, lon = lon, date = days,
               cyc = cyc, ped = round(cyc * ped_ratio * runif(length(days), 0.7, 1.3)),
               stringsAsFactors = FALSE)
  }

  d <- rbind(
    mk("Busy Bridge",  53.53, -113.51, 900, 1.2),   # dual-mode, high volume
    mk("Quiet Path",   53.55, -113.45, 120, 0.8),   # dual-mode, low volume
    mk("Lane Counter", 53.50, -113.49, 400, 0)      # cyclist-only: ped always 0
  )

  meta <- data.frame(
    sensor_raw = c("Busy Bridge", "Quiet Path", "Lane Counter"),
    config = c("Cyclist and Pedestrian", "Cyclist and Pedestrian", "Cyclist Only"),
    stringsAsFactors = FALSE
  )

  list(data = d, meta = meta, days = days)
}

put <- function(d, sensor, date, col, value) {
  d[[col]][d$sensor_raw == sensor & d$date == as.Date(date)] <- value
  d
}
get1 <- function(d, sensor, date, col) {
  v <- d[[col]][d$sensor == sensor & d$date == as.Date(date)]
  if (!length(v)) NA_real_ else v[1]
}

fx <- make_fixture()

cat("-- R4 cross-modal implausibility --\n")
d <- fx$data
d <- put(d, "Busy Bridge", "2022-02-02", "cyc", 86235)   # the classic signature
d <- put(d, "Busy Bridge", "2022-02-02", "ped", 20)
res <- clean_counts(d, NULL, fx$meta)
ok("a huge cyclist count beside a tiny pedestrian count is removed",
   is.na(get1(res$data, "Busy Bridge", "2022-02-02", "cyc")))
ok("the raw value survives for audit",
   isTRUE(get1(res$data, "Busy Bridge", "2022-02-02", "cyc_raw") == 86235))
ok("it is logged against a rule",
   any(res$ledger$rule %in% c("R4", "R5") & res$ledger$date == as.Date("2022-02-02")))

d <- fx$data
d <- put(d, "Quiet Path", "2022-05-28", "ped", 477654)
d <- put(d, "Quiet Path", "2022-05-28", "cyc", 0)
res <- clean_counts(d, NULL, fx$meta)
ok("a huge pedestrian count beside zero cyclists is removed",
   is.na(get1(res$data, "Quiet Path", "2022-05-28", "ped")))

cat("\n-- R4 must not fire on cyclist-only hardware --\n")
# The regression that mattered: 20 of the city's counters report a structural
# zero for pedestrians, and a naive ratio test condemned every busy day.
res <- clean_counts(fx$data, NULL, fx$meta)
lane <- res$data[res$data$sensor == "Lane Counter", ]
ok("no ordinary day on a cyclist-only counter is removed",
   sum(is.na(lane$cyc)) == 0,
   sprintf("(%d of %d removed)", sum(is.na(lane$cyc)), nrow(lane)))
ok("its real peak survives",
   isTRUE(max(lane$cyc, na.rm = TRUE) == max(fx$data$cyc[fx$data$sensor_raw == "Lane Counter"])))

cat("\n-- R5 robust spike --\n")
# A week-long malfunction: the case where a percentile reference fails, because
# the bad values inflate the percentile enough to exempt themselves.
d <- fx$data
run <- seq(as.Date("2022-05-22"), as.Date("2022-05-28"), by = "day")
for (dt in as.character(run)) d <- put(d, "Quiet Path", dt, "ped", 50000)
res <- clean_counts(d, NULL, fx$meta)
kept <- sum(!is.na(res$data$ped[res$data$sensor == "Quiet Path" & res$data$date %in% run]))
ok("an entire malfunctioning run is removed, not merely its peak",
   kept == 0, sprintf("(%d of 7 survived)", kept))

cat("\n-- guard against over-cleaning --\n")
d <- fx$data
# A real seasonal peak: high, but in July and proportionate on both modes.
d <- put(d, "Busy Bridge", "2022-07-15", "cyc", 2228)
d <- put(d, "Busy Bridge", "2022-07-15", "ped", 1400)
res <- clean_counts(d, NULL, fx$meta)
ok("a large but plausible summer peak survives",
   isTRUE(get1(res$data, "Busy Bridge", "2022-07-15", "cyc") == 2228),
   sprintf("(got %s)", get1(res$data, "Busy Bridge", "2022-07-15", "cyc")))
res <- clean_counts(fx$data, NULL, fx$meta)
n_rm <- sum(res$ledger$destructive)
ok("clean input is left almost entirely alone",
   n_rm / nrow(fx$data) < 0.01,
   sprintf("(%d of %d removed)", n_rm, nrow(fx$data)))

cat("\n-- R3 outage --\n")
d <- fx$data
oo <- data.frame(
  sensor_raw = "Busy Bridge",
  date = as.Date(c("2022-03-01", "2022-03-02")),
  n_intervals = 96L, n_oos = 96L, stringsAsFactors = FALSE
)
res <- clean_counts(d, oo, fx$meta)
ok("a flagged outage becomes a gap, not a zero",
   is.na(get1(res$data, "Busy Bridge", "2022-03-01", "cyc")) &&
   is.na(get1(res$data, "Busy Bridge", "2022-03-01", "ped")))
ok("a partial outage is left alone", {
  oo2 <- oo; oo2$n_oos <- 10L
  r2 <- clean_counts(d, oo2, fx$meta)
  !is.na(get1(r2$data, "Busy Bridge", "2022-03-01", "cyc"))
})

cat("\n-- R1 sensor identity --\n")
# A rename: old label stops, new label starts the next day, same coordinates.
d <- fx$data
old <- d[d$sensor_raw == "Busy Bridge" & d$date < as.Date("2022-07-01"), ]
new <- d[d$sensor_raw == "Busy Bridge" & d$date >= as.Date("2022-07-01"), ]
old$sensor_raw <- "Busy Bridge (EBD into town/WBD out of town)"
res <- clean_counts(rbind(old, new, d[d$sensor_raw != "Busy Bridge", ]), NULL, NULL)
ok("a renamed counter is merged back into one series",
   sum(grepl("^Busy Bridge", unique(res$data$sensor))) == 1,
   sprintf("(got %d)", sum(grepl("^Busy Bridge", unique(res$data$sensor)))))
ok("the merged series spans both labels' ranges", {
   b <- res$data[grepl("^Busy Bridge", res$data$sensor), ]
   min(b$date) == min(d$date) && max(b$date) == max(d$date)
})

# The counterpart, and the bug this rule actually had: two DIFFERENT counters
# at one site, one per direction, publishing at identical coordinates with
# fully overlapping dates. Merging them makes R2 discard one direction.
d <- fx$data
east <- d[d$sensor_raw == "Quiet Path", ]; east$sensor_raw <- "106 Street S of 61 Ave E"
west <- d[d$sensor_raw == "Quiet Path", ]; west$sensor_raw <- "106 Street S of 61 Avenue W"
res <- clean_counts(rbind(east, west, d[d$sensor_raw != "Quiet Path", ]), NULL, NULL)
ok("two counters at one site with overlapping dates stay separate",
   length(unique(res$data$sensor[grepl("61 Ave", res$data$sensor)])) == 2,
   sprintf("(got %d)", length(unique(res$data$sensor[grepl("61 Ave", res$data$sensor)]))))
ok("neither direction is discarded as a duplicate",
   sum(res$ledger$rule == "R2") == 0,
   sprintf("(%d logged)", sum(res$ledger$rule == "R2")))

cat("\n-- R2 duplicates --\n")
d <- fx$data
dup <- d[d$sensor_raw == "Busy Bridge" & d$date == as.Date("2022-04-01"), ]
dup$cyc <- dup$cyc * 2
res <- clean_counts(rbind(d, dup), NULL, fx$meta)
ok("no duplicate sensor-day survives",
   !any(duplicated(paste(res$data$sensor, res$data$date))))
ok("the larger reading is the one kept",
   isTRUE(get1(res$data, "Busy Bridge", "2022-04-01", "cyc_raw") == dup$cyc[1]))

# ---------------------------------------------------------------------------
# Live data: structural invariants only.
# ---------------------------------------------------------------------------

cat("\n-- live data invariants --\n")
daily   <- load_daily()
outages <- load_outages()
meta    <- load_counter_meta()
res     <- clean_counts(daily, outages, meta)
d       <- res$data
cat(sprintf("  ..   %s counter-days, %d counters, through %s\n",
            format(nrow(d), big.mark = ","), length(unique(d$sensor)),
            format(max(d$date, na.rm = TRUE))))

ok("labels collapse to no more sensors than labels",
   length(unique(d$sensor)) <= length(unique(daily$sensor_raw)))
ok("no duplicate (sensor, date) remains",
   !any(duplicated(paste(d$sensor, d$date))),
   sprintf("(%d)", sum(duplicated(paste(d$sensor, d$date)))))
ok("every merge is between counters at one location", {
  a <- res$aliases
  m <- a[a$sensor_raw != a$sensor, ]
  nrow(m) == 0 || all(vapply(split(a, a$sensor), function(g)
    nrow(unique(g[, c("lat", "lon")])) == 1, logical(1)))
})
ok("removal rules stay under 0.5% of the record", {
  n <- sum(res$ledger$rule %in% c("R4", "R5"))
  n / nrow(d) < 0.005
}, sprintf("(%.2f%%)", 100 * sum(res$ledger$rule %in% c("R4","R5")) / nrow(d)))
ok("cleaning only ever removes, never invents",
   all(is.na(d$cyc) | d$cyc == d$cyc_raw, na.rm = TRUE))
ok("the cleaned maximum is at or below the raw maximum",
   max(d$cyc, na.rm = TRUE) <= max(d$cyc_raw, na.rm = TRUE))
ok("outage days carry no counts",
   all(is.na(d$cyc[d$outage])))
ok("a fixed panel is a proper subset of the fleet", {
  p <- fixed_panel(d, "2020-01-01", "2024-12-31")
  length(p) > 3 && length(p) < length(unique(d$sensor))
})
ok("the mean series spans the record",
   nrow(daily_series(d, "cyc", "mean")) > 2500)

# Whatever the city has or has not corrected upstream, an implausible reading
# must not reach a chart. Stated as a rate, not a value.
ok("no cleaned cyclist day exceeds 3x the 99.9th percentile", {
  q <- stats::quantile(d$cyc, 0.999, na.rm = TRUE, names = FALSE)
  max(d$cyc, na.rm = TRUE) <= 3 * q
}, sprintf("(max %s, p99.9 %s)", max(d$cyc, na.rm = TRUE),
           round(stats::quantile(d$cyc, 0.999, na.rm = TRUE, names = FALSE))))

cat("\n-- ledger --\n")
print(res$summary)
cat(sprintf("\n%d passed, %d failed\n", pass, fail))
if (fail > 0) quit(status = 1)
