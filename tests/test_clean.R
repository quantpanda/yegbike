# Assertions for the data quality rules in R/clean.R.
# Run:  Rscript tests/test_clean.R
#
# These pin the specific defects found during exploration, and -- just as
# importantly -- pin a real peak that must survive, so the rules can't drift
# into over-cleaning.

suppressPackageStartupMessages({
  library(httr2); library(jsonlite); library(readr); library(sf); library(dplyr)
})
source("R/socrata.R")
source("R/clean.R")

pass <- 0L; fail <- 0L
ok <- function(label, cond, detail = "") {
  if (isTRUE(cond)) {
    pass <<- pass + 1L; cat(sprintf("  ok   %s\n", label))
  } else {
    fail <<- fail + 1L; cat(sprintf("  FAIL %s %s\n", label, detail))
  }
}

cat("loading...\n")
daily   <- load_daily()
outages <- load_outages()
meta    <- load_counter_meta()
res     <- clean_counts(daily, outages, meta)
d       <- res$data

cat("\n-- R1 sensor identity --\n")
ok("57 raw labels collapse to 55 sensors",
   length(unique(d$sensor)) == 55,
   sprintf("(got %d)", length(unique(d$sensor))))
ok("Keillor Point is one sensor",
   sum(grepl("^Keillor Point", unique(d$sensor))) == 1)
ok("Woodward Trail is one sensor",
   sum(grepl("^Woodward Trail", unique(d$sensor))) == 1)
ok("merged Keillor series spans 2019 to 2023",
   { k <- d[d$sensor == "Keillor Point", ]
     min(k$date) < as.Date("2020-01-01") && max(k$date) > as.Date("2023-01-01") })

cat("\n-- R2 duplicates --\n")
ok("no duplicate (sensor, date) remains",
   !any(duplicated(paste(d$sensor, d$date))),
   sprintf("(got %d)", sum(duplicated(paste(d$sensor, d$date)))))
ok("14 duplicate rows were logged",
   sum(res$ledger$rule == "R2") == 14,
   sprintf("(got %d)", sum(res$ledger$rule == "R2")))

cat("\n-- R3/R4/R5 outliers cleaned --\n")
val <- function(sens, dt, col) {
  r <- d[d$sensor == sens & d$date == as.Date(dt), ]
  if (!nrow(r)) return(NA_real_)
  r[[col]][1]
}
ok("MacKinnon Ravine 2022-02-02 cyclist spike removed",
   is.na(val("MacKinnon Ravine", "2022-02-02", "cyc")),
   sprintf("(got %s, raw %s)", val("MacKinnon Ravine","2022-02-02","cyc"),
           val("MacKinnon Ravine","2022-02-02","cyc_raw")))
ok("MacKinnon Ravine raw value preserved for audit",
   isTRUE(val("MacKinnon Ravine", "2022-02-02", "cyc_raw") > 80000))
ok("Keillor Point 2022-05-28 pedestrian spike (477,654) removed",
   is.na(val("Keillor Point", "2022-05-28", "ped")),
   sprintf("(got %s)", val("Keillor Point","2022-05-28","ped")))
ok("the whole late-May-2022 Keillor run is removed, not just its peak",
   { k <- d[d$sensor == "Keillor Point" &
            d$date >= as.Date("2022-05-22") & d$date <= as.Date("2022-05-31"), ]
     all(is.na(k$ped)) },
   sprintf("(%d of %d still present)",
           sum(!is.na(d$ped[d$sensor=="Keillor Point" &
                            d$date >= as.Date("2022-05-22") &
                            d$date <= as.Date("2022-05-31")])), 10))
ok("no cleaned pedestrian value above 20000",
   max(d$ped, na.rm = TRUE) < 20000,
   sprintf("(max %s)", max(d$ped, na.rm = TRUE)))

cat("\n-- R9 borderline values are surfaced, not hidden --\n")
# 4,712 at MacKinnon Ravine in Sept 2022 has a plausible pedestrian ratio and
# sits under the nulling fence, so it stays in the data -- but it must not stay
# invisible.
ok("MacKinnon 2022-09-14 (4712) retained",
   isTRUE(val("MacKinnon Ravine", "2022-09-14", "cyc") == 4712))
ok("MacKinnon 2022-09-14 logged as borderline",
   any(res$ledger$rule == "R9" & res$ledger$sensor == "MacKinnon Ravine" &
       res$ledger$date == as.Date("2022-09-14")))
ok("borderline tier is annotation-only",
   all(!res$ledger$destructive[res$ledger$rule == "R9"]))

cat("\n-- guard against over-cleaning --\n")
# These are the peaks the rules must NOT eat. High Level Bridge West is the
# busiest cycling count in the city; Buena Vista's 2,111 is a smooth spring
# tail, not a break.
ok("High Level Bridge West 2228 cyclist peak survives",
   isTRUE(max(d$cyc[d$sensor == "High Level Bridge West"], na.rm = TRUE) >= 2228),
   sprintf("(max %s)", max(d$cyc[d$sensor == "High Level Bridge West"], na.rm = TRUE)))
ok("Buena Vista Bridge 2111 cyclist peak survives",
   isTRUE(max(d$cyc[d$sensor == "Buena Vista Bridge"], na.rm = TRUE) >= 2111),
   sprintf("(max %s)", max(d$cyc[d$sensor == "Buena Vista Bridge"], na.rm = TRUE)))

# Outage nulling is a correction, not over-cleaning -- a dead counter reporting
# zero is worse than a gap. Judge the outlier rules on their own.
n_outlier <- sum(res$ledger$rule %in% c("R4", "R5"))
ok("outlier rules touch under 0.5% of rows",
   n_outlier / nrow(d) < 0.005,
   sprintf("(%d of %d = %.2f%%)", n_outlier, nrow(d), 100 * n_outlier / nrow(d)))
cat(sprintf("  ..   R3 outage nulls: %d sensor-days (%.1f%%) -- corrections, not outliers\n",
            sum(res$ledger$rule == "R3"), 100 * sum(res$ledger$rule == "R3") / nrow(d)))

cat("\n-- R8 comparability --\n")
p <- fixed_panel(d, "2019-01-01", "2025-12-31")
ok("fixed panel is non-empty and smaller than the fleet",
   length(p) > 5 && length(p) < length(unique(d$sensor)),
   sprintf("(%d sensors)", length(p)))
s_mean <- daily_series(d, "cyc", "mean")
ok("mean series covers the full date range", nrow(s_mean) > 2500)

cat("\n-- ledger --\n")
print(res$summary)
cat(sprintf("\n%d passed, %d failed\n", pass, fail))
if (fail > 0) quit(status = 1)
