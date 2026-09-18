# ---------------------------------------------------------------------------
# clean.R -- data quality rules for the counter series.
#
# Design principle: nothing is silently dropped. Every value this file changes
# is written to a ledger (sensor, date, rule, measure, raw_value, action) that
# the Data Quality tab renders, and every consumer can ask for the raw series
# instead. The rules are auditable and arguable, which is the point -- the
# previous approach was two unexplained global thresholds.
#
# Rules:
#   R1 renamed sensor      merge labels that share coordinates
#   R2 duplicate rows      collapse duplicate (sensor, date)
#   R3 sensor outage       use the city's own out_of_service flag; 0 -> NA
#   R4 cross-modal         one mode implausibly dwarfs the other
#   R5 robust spike        per-sensor, per-month MAD outlier
#   R6 missing             blank counts
#   R7 ambiguous zero      tagged, not altered
#   R8 fleet growth        handled in aggregation, not here (see active_panel)
# ---------------------------------------------------------------------------

# Absolute floors below which we never call something a spike. Edmonton's
# busiest counter peaks around 2,200 cyclists/day, so a genuine malfunction is
# always far above these; the floors stop the robust rule from firing on the
# ordinary variance of a quiet sensor.
SPIKE_FLOOR_CYC <- 500
SPIKE_FLOOR_PED <- 2000

# A day is "out of service" when at least this fraction of its 15-minute
# intervals carry the city's flag. In practice the flag is set for whole days,
# so this threshold is not sensitive.
OUTAGE_FRACTION <- 0.5

RULE_LABELS <- c(
  R1 = "Renamed sensor merged",
  R2 = "Duplicate sensor-day",
  R3 = "Sensor out of service",
  R4 = "Cross-modal implausibility",
  R5 = "Robust spike (per-sensor MAD)",
  R6 = "Missing value",
  R7 = "Ambiguous zero",
  R9 = "Borderline - retained for review"
)

# Rules that null a value. R7 and R9 annotate without altering, so the Data
# Quality tab can style them differently.
RULES_DESTRUCTIVE <- c("R3", "R4", "R5")

# ---------------------------------------------------------------------------
# R1 -- sensor identity
# ---------------------------------------------------------------------------

#' Strip the parenthetical direction annotation and tidy whitespace.
#'
#' "Keillor Point (EBD into Keillor Point/WBD out of Keillor Point)" and
#' "Keillor Point" are the same physical counter; the city renamed it in 2023.
tidy_sensor_name <- function(x) {
  x <- sub("\\s*\\(.*\\)\\s*$", "", as.character(x))
  trimws(gsub("\\s+", " ", x))
}

#' Build a map from raw sensor label to canonical sensor name.
#'
#' Two labels are the same counter when they sit at the same coordinates (to 5
#' decimal places, about a metre) AND their date ranges do not overlap. Both
#' conditions are required, and the second is not a formality: the city runs
#' pairs of counters at a single site, one per direction, and publishes them at
#' identical coordinates. `106 Street S of 61 Ave E` and `106 Street S of 61
#' Avenue W` are two devices, not one renamed device, and merging them would
#' hand R2 a pile of same-day collisions to "resolve" by throwing one
#' direction away.
#'
#' A rename, by contrast, leaves a clean handover: the old label stops on one
#' date and the new label starts after it.
#'
#' Deriving this from the data rather than a hard-coded alias list means future
#' renames are handled without a code change -- which is the point, since this
#' rule was written against 57 labels and the city has since published 62.
sensor_alias_map <- function(daily) {
  by_label <- stats::aggregate(
    cbind(lat, lon) ~ sensor_raw, data = daily, FUN = function(v) round(stats::median(v), 5)
  )
  rng <- stats::aggregate(date ~ sensor_raw, data = daily,
                          FUN = function(v) c(min(v), max(v)))
  by_label$first <- rng$date[match(by_label$sensor_raw, rng$sensor_raw), 1]
  by_label$last  <- rng$date[match(by_label$sensor_raw, rng$sensor_raw), 2]

  by_label$key  <- paste(by_label$lat, by_label$lon)
  by_label$tidy <- tidy_sensor_name(by_label$sensor_raw)
  by_label$sensor <- by_label$tidy

  for (k in unique(by_label$key)) {
    idx <- which(by_label$key == k)
    if (length(idx) < 2) next
    g <- by_label[idx, ]
    # Chronological order makes "does this one start after the last one ended"
    # a single pass.
    o <- order(g$first)
    g <- g[o, ]; idx <- idx[o]

    # Greedily chain labels whose ranges do not overlap. Anything that overlaps
    # an existing chain starts a chain of its own, and so keeps its own name.
    chain <- rep(NA_integer_, nrow(g))
    chain[1] <- 1L
    for (i in seq_len(nrow(g))[-1]) {
      prev <- which(chain == chain[i - 1L])
      if (g$first[i] > max(g$last[prev])) chain[i] <- chain[i - 1L]
      else chain[i] <- max(chain, na.rm = TRUE) + 1L
    }
    for (cc in unique(chain)) {
      members <- which(chain == cc)
      if (length(members) < 2) next
      nm <- unique(g$tidy[members])
      by_label$sensor[idx[members]] <- nm[order(nchar(nm), nm)][1]
    }
  }
  by_label[, c("sensor_raw", "sensor", "lat", "lon")]
}

# ---------------------------------------------------------------------------
# R5 -- robust spike detection
# ---------------------------------------------------------------------------

#' Flag values that are extreme relative to the same sensor in the same month.
#'
#' Sensors differ by two orders of magnitude in volume and ridership is
#' strongly seasonal, so neither a global cutoff nor a global percentile works.
#' A value must clear three independent bars:
#'
#'   1. median + k*MAD within (sensor, calendar month) -- seasonally aware
#'   2. `fence_mult` times a Tukey fence (Q3 + 5*IQR) for that sensor
#'   3. an absolute floor
#'
#' Condition 2 deliberately uses a Tukey fence rather than a high percentile.
#' An earlier version used 3 * p99, which fails on exactly the cases that
#' matter: when a sensor breaks for a week, the broken values themselves lift
#' p99 far enough that they stop qualifying as outliers. Keillor Point's p99 is
#' 31,222 purely because of its own malfunction, so 3*p99 = 93,665 waved
#' through a 51,545-pedestrian day. Q3 and IQR are computed from the bulk of
#' the distribution and stay put: Keillor's fence is 937.
#'
#' The multiplier is what preserves genuine peaks. Buena Vista Bridge topped
#' out at 2,111 cyclists in May 2021 against a fence of 1,714 -- a real spring
#' peak, and only 1.2x the fence. MacKinnon Ravine's malfunction sits at 14x
#' its fence. Doubling the fence cleanly separates the two.
#' @return logical vector when `advisory = FALSE`; when TRUE, returns the
#'   weaker single-fence test instead, used to surface borderline values that
#'   are kept but worth a human look.
spike_flags <- function(sensor, month, value, floor_abs, k = 8, fence_mult = 2) {
  out <- rep(FALSE, length(value))
  ok <- !is.na(value)
  if (!any(ok)) return(out)

  fence <- tapply(value[ok], sensor[ok], function(v) {
    q <- stats::quantile(v, c(0.25, 0.75), names = FALSE)
    q[2] + 5 * (q[2] - q[1])
  })

  grp  <- paste(sensor, month)
  cent <- tapply(value[ok], grp[ok], stats::median)
  disp <- tapply(value[ok], grp[ok], stats::mad)

  m <- cent[grp]; s <- disp[grp]; f <- fence[sensor]
  # MAD collapses to zero for sensors that are mostly zero (winter, or a quiet
  # path). Fall back to the group median so the test stays finite; conditions 2
  # and 3 still gate the result.
  s[is.na(s) | s == 0] <- pmax(m[is.na(s) | s == 0], 1)

  out[ok] <- value[ok] > (m[ok] + k * s[ok]) &
             value[ok] > fence_mult * pmax(f[ok], 1) &
             value[ok] >= floor_abs
  out
}

# ---------------------------------------------------------------------------
# Hardware capability
# ---------------------------------------------------------------------------

#' Which rows come from a counter that records both cyclists and pedestrians.
#'
#' Prefers the city's published `counter_configuration`; where that is missing,
#' infers from the series itself -- a counter that has never once recorded a
#' pedestrian across thousands of days is not a counter that failed to see any,
#' it is a counter that cannot see them.
is_dual_mode <- function(d, meta = NULL) {
  ever_ped <- tapply(d$ped, d$sensor, function(v) any(!is.na(v) & v > 0))
  ever_cyc <- tapply(d$cyc, d$sensor, function(v) any(!is.na(v) & v > 0))
  inferred <- ever_ped & ever_cyc
  dual <- unname(inferred[d$sensor])

  if (!is.null(meta) && nrow(meta)) {
    cfg <- stats::setNames(tolower(meta$config), tidy_sensor_name(meta$sensor_raw))
    known <- cfg[d$sensor]
    stated_dual <- grepl("cyclist", known) & grepl("pedestrian", known)
    dual[!is.na(known)] <- stated_dual[!is.na(known)]
  }
  dual[is.na(dual)] <- FALSE
  dual
}

# ---------------------------------------------------------------------------
# Main entry point
# ---------------------------------------------------------------------------

#' Apply R1-R7 to the daily counter series.
#'
#' @param daily   normalised output of load_daily()
#' @param outages normalised output of load_outages()
#' @param meta    normalised output of load_counter_meta(); used to tell
#'                cyclist-only hardware from dual-mode, which R4 depends on
#' @return list(data, ledger, aliases, summary)
#'   `data` carries both raw and cleaned columns (cyc_raw/cyc, ped_raw/ped) so
#'   the UI's raw/clean toggle is a column choice, not a re-run.
clean_counts <- function(daily, outages = NULL, meta = NULL) {
  stopifnot(is.data.frame(daily))
  ledger <- list()
  add <- function(rule, measure, sensor, date, raw, action) {
    if (!length(sensor)) return(invisible(NULL))
    ledger[[length(ledger) + 1L]] <<- data.frame(
      rule = rule, measure = measure, sensor = sensor, date = date,
      raw_value = raw, action = action, stringsAsFactors = FALSE
    )
  }

  # --- R1: canonical sensor identity -------------------------------------
  # Drop any pre-existing canonical column so a second pass over already
  # -canonicalised data doesn't collide into sensor.x / sensor.y on the merge.
  daily <- daily[, setdiff(names(daily), "sensor"), drop = FALSE]
  aliases <- sensor_alias_map(daily)
  # match() rather than merge(): this is a lookup, and merge() sorts both
  # frames to do it. Over 112k rows that sort was a measurable share of
  # startup.
  d <- daily
  d$sensor <- aliases$sensor[match(d$sensor_raw, aliases$sensor_raw)]
  miss <- is.na(d$sensor)
  if (any(miss)) d$sensor[miss] <- tidy_sensor_name(d$sensor_raw[miss])

  renamed <- aliases[aliases$sensor_raw != aliases$sensor, ]
  if (nrow(renamed)) {
    add("R1", "identity", renamed$sensor_raw, as.Date(NA), NA_real_,
        paste("merged into", renamed$sensor))
  }

  # --- R2: duplicate (sensor, date) --------------------------------------
  # 14 pairs, all Feb-Mar 2019. They double-count in every sum. Keep the larger
  # reading: a duplicate row usually means a partial write followed by a full
  # one, so the maximum is the complete day.
  key <- paste(d$sensor, d$date, sep = "\r")
  dup_key <- if (anyDuplicated(key)) unique(key[duplicated(key)]) else character(0)
  if (length(dup_key)) {
    # Log one entry per affected sensor-day, not per row, so the ledger count
    # matches the number of days corrected.
    dd <- d[match(dup_key, key), ]
    add("R2", "both", dd$sensor, dd$date, dd$cyc, "collapsed to max of duplicates")
    # max() of an all-NA group warns and returns -Inf; keep NA instead.
    max_or_na <- function(v) if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE)
    agg <- stats::aggregate(cbind(cyc, ped, lat, lon) ~ sensor + date, data = d,
                            FUN = max_or_na, na.action = stats::na.pass)
    keep <- d[!duplicated(key), setdiff(names(d), c("cyc", "ped", "lat", "lon"))]
    d <- merge(keep, agg, by = c("sensor", "date"))
  }

  d$cyc_raw <- d$cyc
  d$ped_raw <- d$ped

  # --- R3: sensor outage --------------------------------------------------
  # The single most useful rule. Without it a dead counter reads as a day when
  # nobody cycled, which drags every average down and is indistinguishable
  # from a real winter zero. The city publishes the flag; we just use it.
  d$outage <- FALSE
  if (!is.null(outages) && nrow(outages)) {
    oa <- outages
    oa$sensor <- tidy_sensor_name(oa$sensor_raw)
    is_out <- oa$n_intervals > 0 & (oa$n_oos / oa$n_intervals) >= OUTAGE_FRACTION
    # Composite-key membership instead of a two-column merge. The merge sorted
    # both 112k-row frames to answer a yes/no question.
    if (any(is_out)) {
      out_key <- paste(oa$sensor[is_out], oa$date[is_out], sep = "\r")
      d$outage <- paste(d$sensor, d$date, sep = "\r") %in% out_key
    }
  }
  hit <- which(d$outage & (!is.na(d$cyc) | !is.na(d$ped)))
  if (length(hit)) {
    add("R3", "both", d$sensor[hit], d$date[hit], d$cyc[hit], "counter offline; set to NA")
    d$cyc[hit] <- NA_real_
    d$ped[hit] <- NA_real_
  }

  # --- R4: cross-modal implausibility -------------------------------------
  # A counter that watches both modes sees them in some rough proportion. When
  # one explodes while the other stays in the teens, the sensor is miscounting,
  # not witnessing a cycling festival. This is the signature of both known bad
  # sensors: MacKinnon Ravine (86,235 bikes / 20 peds, February) and Keillor
  # Point (477,654 peds / 0 bikes).
  #
  # Critically, this test is only meaningful on dual-mode hardware. Twenty of
  # the city's counters are "Cyclist Only" and report a structural zero for
  # pedestrians, so applying the ratio test to them would condemn every busy
  # day they ever had. Those sensors are left to R5 instead.
  d$dual_mode <- is_dual_mode(d, meta)
  ratio_ok <- d$dual_mode & !is.na(d$cyc) & !is.na(d$ped)

  bad_cyc <- which(ratio_ok & d$cyc >= 1000 & d$cyc > 20 * pmax(d$ped, 1))
  if (length(bad_cyc)) {
    add("R4", "cyclist", d$sensor[bad_cyc], d$date[bad_cyc], d$cyc[bad_cyc],
        "implausible vs pedestrian count; set to NA")
    d$cyc[bad_cyc] <- NA_real_
  }
  bad_ped <- which(ratio_ok & d$ped >= 5000 & d$ped > 20 * pmax(d$cyc, 1))
  if (length(bad_ped)) {
    add("R4", "pedestrian", d$sensor[bad_ped], d$date[bad_ped], d$ped[bad_ped],
        "implausible vs cyclist count; set to NA")
    d$ped[bad_ped] <- NA_real_
  }

  # --- R5: robust per-sensor spike ----------------------------------------
  mth <- as.integer(format(d$date, "%m"))
  sc <- spike_flags(d$sensor, mth, d$cyc, SPIKE_FLOOR_CYC)
  if (any(sc)) {
    add("R5", "cyclist", d$sensor[sc], d$date[sc], d$cyc[sc], "MAD outlier; set to NA")
    d$cyc[sc] <- NA_real_
  }
  sp <- spike_flags(d$sensor, mth, d$ped, SPIKE_FLOOR_PED)
  if (any(sp)) {
    add("R5", "pedestrian", d$sensor[sp], d$date[sp], d$ped[sp], "MAD outlier; set to NA")
    d$ped[sp] <- NA_real_
  }

  # --- R9: borderline, retained -------------------------------------------
  # Values that clear a single Tukey fence but not the doubled one used to
  # null. These survive into every chart; logging them means a borderline
  # reading like MacKinnon Ravine's 4,712 on 2022-09-14 is visible and
  # arguable rather than quietly either deleted or accepted. Chasing each one
  # with a tighter threshold would just overfit to the cases we happened to
  # look at.
  bl_c <- which(spike_flags(d$sensor, mth, d$cyc, SPIKE_FLOOR_CYC, fence_mult = 1) & !is.na(d$cyc))
  if (length(bl_c)) {
    add("R9", "cyclist", d$sensor[bl_c], d$date[bl_c], d$cyc[bl_c], "kept; above single fence")
  }
  bl_p <- which(spike_flags(d$sensor, mth, d$ped, SPIKE_FLOOR_PED, fence_mult = 1) & !is.na(d$ped))
  if (length(bl_p)) {
    add("R9", "pedestrian", d$sensor[bl_p], d$date[bl_p], d$ped[bl_p], "kept; above single fence")
  }

  # --- R6: missing --------------------------------------------------------
  miss <- which(is.na(d$cyc_raw) & !d$outage)
  if (length(miss)) {
    add("R6", "cyclist", d$sensor[miss], d$date[miss], NA_real_, "blank in source")
  }

  # --- R7: ambiguous zero -------------------------------------------------
  # Not altered -- just tagged, so trend charts can choose to treat a zero as a
  # real observation or as unknown. About 46% of rows have a zero in one mode,
  # which mixes genuine winter days with silent sensor failures the city never
  # flagged.
  d$zero_cyc <- !is.na(d$cyc) & d$cyc == 0 & !d$outage

  ledger_df <- if (length(ledger)) do.call(rbind, ledger) else
    data.frame(rule = character(), measure = character(), sensor = character(),
               date = as.Date(character()), raw_value = numeric(),
               action = character(), stringsAsFactors = FALSE)
  ledger_df$rule_label   <- unname(RULE_LABELS[ledger_df$rule])
  ledger_df$destructive  <- ledger_df$rule %in% RULES_DESTRUCTIVE

  d <- d[order(d$sensor, d$date), ]
  rownames(d) <- NULL

  counts <- as.integer(table(factor(ledger_df$rule, levels = names(RULE_LABELS))))
  names(counts) <- names(RULE_LABELS)
  # R7 tags rows in place rather than writing ledger entries -- there are tens
  # of thousands of ambiguous zeros and listing them individually would bury
  # the rules that actually changed something.
  counts[["R7"]] <- sum(d$zero_cyc, na.rm = TRUE)

  list(
    data    = d,
    ledger  = ledger_df,
    aliases = aliases,
    summary = data.frame(
      rule        = names(RULE_LABELS),
      label       = unname(RULE_LABELS),
      n           = unname(counts),
      destructive = names(RULE_LABELS) %in% RULES_DESTRUCTIVE,
      stringsAsFactors = FALSE
    )
  )
}

# ---------------------------------------------------------------------------
# R8 -- comparability over time
# ---------------------------------------------------------------------------

#' Sensors reporting in every year of a range -- a fixed panel.
#'
#' The counter fleet grew from 14 sensors in 2018 to 48 today, so a raw
#' city-wide daily total mostly measures how many counters the city has
#' installed. Restricting to sensors present throughout gives a series where a
#' change means a change in ridership.
fixed_panel <- function(d, from = NULL, to = NULL) {
  from <- if (is.null(from)) min(d$date, na.rm = TRUE) else as.Date(from)
  to   <- if (is.null(to)) max(d$date, na.rm = TRUE) else as.Date(to)
  sub  <- d[d$date >= from & d$date <= to & !is.na(d$cyc), ]
  if (!nrow(sub)) return(character())
  yrs  <- unique(format(sub$date, "%Y"))
  tab  <- table(sub$sensor, format(sub$date, "%Y"))
  rownames(tab)[rowSums(tab > 0) == length(yrs)]
}

#' City-wide daily series, with the normalisation choice made explicit.
#'
#' @param mode "total"  raw sum across whatever counters reported (biased by
#'                      fleet growth -- offered because it is what people
#'                      expect to see, and the tab labels the caveat)
#'             "mean"   average per reporting counter (fleet-size neutral)
#'             "panel"  sum over a fixed panel of counters present throughout
daily_series <- function(d, measure = c("cyc", "ped"), mode = c("mean", "total", "panel"),
                         panel = NULL) {
  measure <- match.arg(measure); mode <- match.arg(mode)
  x <- d[!is.na(d[[measure]]), ]
  if (mode == "panel") {
    if (is.null(panel)) panel <- fixed_panel(d)
    x <- x[x$sensor %in% panel, ]
  }
  if (!nrow(x)) return(data.frame(date = as.Date(character()), value = numeric(), n_sensors = integer()))
  agg <- stats::aggregate(x[[measure]], by = list(date = x$date), FUN = function(v) c(sum(v), length(v)))
  out <- data.frame(date = agg$date, sum = agg$x[, 1], n_sensors = as.integer(agg$x[, 2]))
  out$value <- if (mode == "mean") out$sum / out$n_sensors else out$sum
  out[order(out$date), c("date", "value", "sum", "n_sensors")]
}
