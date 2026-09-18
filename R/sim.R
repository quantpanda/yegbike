# ---------------------------------------------------------------------------
# sim.R -- turn one day of 15-minute counter readings into payloads the
# deck.gl widget can animate.
#
# Two payloads, matching the two modes the UI offers:
#
#   measured   what the counters actually recorded. One point per counter, a
#              96-slot array of 15-minute values. No inference at all.
#
#   simulated  the same counts, rendered as movement along real bike-route
#              geometry. Every agent's existence is justified by a recorded
#              cyclist, but the specific route any individual took is invented.
#              The UI labels this; see the note in network.R.
# ---------------------------------------------------------------------------

BINS_PER_DAY <- 96L        # 15-minute bins
CYCLIST_SPEED_MS <- 4.2    # ~15 km/h
MAX_AGENTS <- 4000L        # keeps the browser at 60fps and the payload small

#' Measured-mode payload: per-counter 15-minute series.
#'
#' @param intraday normalised load_intraday() output
#' @param meta     optional counter metadata for labels
build_measured <- function(intraday, meta = NULL) {
  if (!nrow(intraday)) return(list(counters = list(), peak = 0))
  x <- intraday
  x$sensor <- tidy_sensor_name(x$sensor_raw)
  x$bin <- pmin(BINS_PER_DAY - 1L, pmax(0L, x$minute %/% 15L))

  sensors <- sort(unique(x$sensor))
  out <- vector("list", length(sensors))
  peak <- 0

  for (i in seq_along(sensors)) {
    s <- sensors[i]
    xs <- x[x$sensor == s, ]
    cyc <- numeric(BINS_PER_DAY)
    ped <- numeric(BINS_PER_DAY)
    # A sensor can appear more than once per bin when the portal carries both
    # directions as separate rows; sum rather than overwrite.
    cyc_agg <- tapply(xs$cyc, xs$bin, sum)
    ped_agg <- tapply(xs$ped, xs$bin, sum)
    cyc[as.integer(names(cyc_agg)) + 1L] <- as.numeric(cyc_agg)
    ped[as.integer(names(ped_agg)) + 1L] <- as.numeric(ped_agg)
    peak <- max(peak, max(cyc))

    out[[i]] <- list(
      name = s,
      lon  = round(stats::median(xs$lon), 6),
      lat  = round(stats::median(xs$lat), 6),
      cyc  = as.integer(cyc),
      ped  = as.integer(ped),
      total = as.integer(sum(cyc)),
      oos  = as.logical(any(xs$oos))
    )
  }
  list(counters = out, peak = as.integer(peak))
}

#' Simulated-mode payload: a path library plus lightweight agents.
#'
#' The paths are sent once and referenced by index; each agent is just three
#' numbers. Sending whole geometries per agent would be tens of megabytes.
#'
#' Agents are scaled so the browser never has to draw more than MAX_AGENTS
#' trips. `scale` is reported back to the UI so the legend can say "1 rider
#' shown = N counted" rather than implying a one-to-one rendering.
build_simulated <- function(intraday, path_cache, seed = 1) {
  empty <- list(paths = list(), agents = list(), scale = 1, total = 0, shown = 0)
  if (!nrow(intraday) || is.null(path_cache)) return(empty)
  set.seed(seed)

  paths_by_sensor <- path_cache$paths
  x <- intraday
  x$sensor <- tidy_sensor_name(x$sensor_raw)
  x <- x[x$sensor %in% names(paths_by_sensor), ]
  if (!nrow(x)) return(empty)

  # Flatten the path library to a single indexed list, remembering which
  # (sensor, direction) each entry belongs to.
  lib <- list(); lookup <- list(); n <- 0L
  for (s in names(paths_by_sensor)) {
    for (dn in names(paths_by_sensor[[s]])) {
      ids <- integer(0)
      for (p in paths_by_sensor[[s]][[dn]]) {
        n <- n + 1L
        lib[[n]] <- p
        ids <- c(ids, n)
      }
      lookup[[paste(s, dn)]] <- ids
    }
  }
  if (!n) return(empty)

  total <- sum(x$cyc)
  if (total <= 0) return(c(empty, list(total = 0)))
  scale <- max(1L, as.integer(ceiling(total / MAX_AGENTS)))

  dirs <- c("ebd", "wbd", "nbd", "sbd")
  agent_path <- integer(0); agent_start <- numeric(0); agent_dur <- numeric(0)

  for (i in seq_len(nrow(x))) {
    row <- x[i, ]
    if (row$cyc <= 0 || row$oos) next
    counts <- c(row$ebd, row$wbd, row$nbd, row$sbd)
    # Some counters report a total without a directional split. Spread those
    # across whichever directions we have paths for rather than dropping them.
    if (sum(counts) == 0) {
      avail <- dirs[paste(row$sensor, dirs) %in% names(lookup)]
      if (!length(avail)) next
      counts <- rep(row$cyc / length(avail), 4)
      counts[!dirs %in% avail] <- 0
    }

    for (k in seq_along(dirs)) {
      if (counts[k] <= 0) next
      ids <- lookup[[paste(row$sensor, dirs[k])]]
      if (is.null(ids) || !length(ids)) next
      # Probabilistic rounding so directions quieter than one agent per bin
      # still show up over the course of a day instead of vanishing.
      exact <- counts[k] / scale
      n_ag <- floor(exact) + (stats::runif(1) < (exact - floor(exact)))
      if (n_ag < 1) next

      pick <- ids[sample.int(length(ids), n_ag, replace = TRUE)]
      lens <- vapply(lib[pick], seg_length_m, numeric(1))
      speed <- CYCLIST_SPEED_MS * stats::runif(n_ag, 0.75, 1.3)
      agent_path  <- c(agent_path, pick)
      # Spread starts uniformly through the 15-minute bin the count came from.
      agent_start <- c(agent_start, row$minute + stats::runif(n_ag, 0, 15))
      agent_dur   <- c(agent_dur, (lens / speed) / 60)  # minutes
    }
  }

  if (!length(agent_path)) return(empty)

  # Only ship the paths that at least one agent uses.
  used <- sort(unique(agent_path))
  remap <- stats::setNames(seq_along(used) - 1L, used)  # zero-based for JS

  list(
    paths  = lapply(lib[used], function(m) unname(round(m, 5))),
    agents = list(
      p = unname(remap[as.character(agent_path)]),
      t = round(agent_start, 2),
      d = round(agent_dur, 2)
    ),
    scale = scale,
    total = as.integer(total),
    shown = length(agent_path)
  )
}

#' Load the precomputed path library, or NULL if there isn't one.
#'
#' Looks in `data/` first: that is a deploy artifact shipped with the app, so a
#' hosted instance starts with the route network already built instead of
#' spending ten seconds rebuilding it on every cold start. `cache/` is the
#' local development fallback, written by scripts/build_paths.R.
load_path_cache <- function(paths = c("data/paths.rds", cache_path("paths"))) {
  for (p in paths) {
    if (is.na(p) || !file.exists(p)) next
    obj <- tryCatch(readRDS(p), error = function(e) NULL)
    if (!is.null(obj)) return(obj)
  }
  NULL
}

#' Everything the Simulation tab needs for one date.
build_sim_payload <- function(day, path_cache = NULL, meta = NULL, force = FALSE) {
  intraday <- load_intraday(day, force = force)
  measured <- build_measured(intraday, meta)
  simulated <- build_simulated(intraday, path_cache)
  list(
    date = format(as.Date(day)),
    measured = measured,
    simulated = simulated,
    has_paths = !is.null(path_cache),
    n_rows = nrow(intraday)
  )
}
