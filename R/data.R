# ---------------------------------------------------------------------------
# data.R -- assemble everything the app needs into one bundle.
#
# Called once at startup and again whenever the user hits Refresh. Every fetch
# degrades independently, so one dead endpoint costs one layer rather than the
# whole app.
# ---------------------------------------------------------------------------

# Classifications that are not cycling infrastructure. "Walkway / Breezeway"
# alone is 3,031 of the 9,766 features -- a third of what a naive render of
# this dataset shows as "the bike network" is sidewalk.
NON_CYCLING_CLASS <- c("Walkway / Breezeway", "Maintenance Access")

# Douglas-Peucker tolerance, in metres, for the Map tab's route geometry. 5 m
# is under a pixel at any zoom that fits a neighbourhood on screen.
MAP_SIMPLIFY_M <- 5

# How old the shared bundle may get before the next visitor triggers a reload.
# A reload is incremental (only rows newer than the cache), so this is cheap.
DATA_MAX_AGE_S <- 3600

#' Load and clean every dataset the app uses.
#'
#' @param force  bypass the disk cache and re-fetch from data.edmonton.ca
#' @param notify optional function(step_label, fraction) for a progress bar
app_data <- function(force = FALSE, notify = NULL) {
  step <- function(label, frac) if (is.function(notify)) notify(label, frac)

  step("Bike routes", 0.05)
  routes <- try_layer(load_routes(force))

  # The Map tab draws one merged shape per route type rather than 9,860
  # segments: 5.4 MB of GeoJSON becomes under 1 MB, and Leaflet draws 11
  # layers instead of 9,860. Prefer the copy built at deploy time.
  map_routes <- seed_rds("routes_by_class")
  if (is.null(map_routes) && !is.null(routes)) {
    step("Preparing map geometry", 0.15)
    map_routes <- try_layer(merge_routes_by_class(routes))
  }

  # Counts and outage flags come from one rollup query, so they are fetched
  # together rather than through load_daily()/load_outages() individually.
  step("Counter readings", 0.30)
  both    <- try_layer(load_counts_and_outages(force), fallback = list())
  daily   <- both$counts
  outages <- both$outages %||% data.frame()
  if (is.null(daily)) stop("no counter data available")

  step("Counter details", 0.65)
  meta <- try_layer(load_counter_meta(force), fallback = data.frame())

  step("Construction", 0.80)
  construction <- try_layer(load_construction(force), fallback = data.frame())

  step("Applying data quality rules", 0.90)
  cleaned <- clean_counts(daily, outages, meta)

  step("Ready", 1)
  list(
    # Segment attributes only. Geometry is needed once, for lengths, and
    # carrying it in a bundle every session shares would cost RAM for nothing.
    routes       = route_table(routes),
    map_routes   = map_routes,
    counts       = cleaned$data,
    ledger       = cleaned$ledger,
    qa_summary   = cleaned$summary,
    aliases      = cleaned$aliases,
    meta         = meta,
    construction = construction,
    construction_map = try_layer(construction_map(construction)),
    fetched_at   = Sys.time(),
    fetch_log    = fetch_log()
  )
}

# ---------------------------------------------------------------------------
# One bundle per R process, shared by every session.
#
# Loading used to happen inside server(), so every visitor paid for the full
# fetch-and-clean (about 16 s) and held their own ~36 MB copy. Now the first
# visitor after a cold start or after DATA_MAX_AGE_S pays for it once and
# everyone else gets it instantly. R is single-threaded, so two sessions
# cannot race to reload.
# ---------------------------------------------------------------------------

.shared <- new.env(parent = emptyenv())

#' TRUE when the shared bundle exists and is younger than DATA_MAX_AGE_S.
shared_bundle_fresh <- function() {
  b <- .shared$bundle
  !is.null(b) &&
    as.numeric(difftime(Sys.time(), b$fetched_at, units = "secs")) < DATA_MAX_AGE_S
}

#' The shared bundle, reloading it first if stale.
#'
#' A failed reload keeps serving the previous bundle rather than taking the
#' app down; only a failure with nothing to fall back on is an error.
shared_bundle <- function(notify = NULL) {
  if (!shared_bundle_fresh()) {
    b <- tryCatch(app_data(force = FALSE, notify = notify), error = function(e) {
      if (is.null(.shared$bundle)) stop(e)
      warning("data reload failed, serving previous bundle: ", conditionMessage(e))
      NULL
    })
    if (!is.null(b)) .shared$bundle <- b
  }
  .shared$bundle
}

#' Run a loader, downgrading a hard failure to a usable empty value.
try_layer <- function(expr, fallback = NULL) {
  tryCatch(expr, error = function(e) {
    warning(conditionMessage(e))
    fallback
  })
}

# ---------------------------------------------------------------------------
# Derived views, shared across tabs
# ---------------------------------------------------------------------------

#' Per-sensor summary over an optional date window.
sensor_summary <- function(counts, from = NULL, to = NULL) {
  x <- counts
  if (!is.null(from)) x <- x[x$date >= as.Date(from), ]
  if (!is.null(to))   x <- x[x$date <= as.Date(to), ]
  if (!nrow(x)) return(data.frame())

  agg <- do.call(rbind, lapply(split(x, x$sensor), function(g) {
    data.frame(
      sensor    = g$sensor[1],
      lat       = stats::median(g$lat, na.rm = TRUE),
      lon       = stats::median(g$lon, na.rm = TRUE),
      days      = nrow(g),
      reporting = sum(!is.na(g$cyc)),
      outages   = sum(g$outage),
      cyc_total = sum(g$cyc, na.rm = TRUE),
      cyc_mean  = mean(g$cyc, na.rm = TRUE),
      cyc_max   = suppressWarnings(max(g$cyc, na.rm = TRUE)),
      ped_total = sum(g$ped, na.rm = TRUE),
      first     = min(g$date),
      last      = max(g$date),
      stringsAsFactors = FALSE
    )
  }))
  agg$cyc_max[!is.finite(agg$cyc_max)] <- NA
  agg$coverage <- agg$reporting / pmax(1, as.numeric(agg$last - agg$first) + 1)
  agg[order(-agg$cyc_total), ]
}

#' Counters that have reported within `days` of the latest date in the data.
#'
#' Seven counters have been decommissioned over the years. Including them in a
#' "current status" view would show dead sensors as though they were live.
active_sensors <- function(counts, days = 30) {
  latest <- max(counts$date, na.rm = TRUE)
  recent <- counts[counts$date >= latest - days & !is.na(counts$cyc), ]
  unique(recent$sensor)
}

#' Construction permits in force on `on_date`.
active_construction <- function(construction, on_date = Sys.Date()) {
  if (is.null(construction) || !nrow(construction)) return(construction)
  on_date <- as.Date(on_date)
  keep <- (is.na(construction$start_date) | construction$start_date <= on_date) &
          (is.na(construction$finish_date) | construction$finish_date >= on_date)
  construction[keep, ]
}

#' Segment attributes with a length in km, geometry dropped.
route_table <- function(routes) {
  if (is.null(routes) || !nrow(routes)) return(NULL)
  km <- as.numeric(sf::st_length(sf::st_transform(routes, 3402))) / 1000
  data.frame(
    classification    = routes$classification,
    construction_year = suppressWarnings(as.integer(routes$construction_year)),
    km                = km,
    stringsAsFactors  = FALSE
  )
}

#' Map-ready construction layer for the permits open today.
#'
#' Built once per bundle. The permit outlines arrive at survey precision, and
#' sent as-is they were 2.5 MB of the Map tab -- more than the whole route
#' network. Simplified to MAP_SIMPLIFY_M and rounded like the routes.
#'
#' @return list(lines = sf or NULL, points = data.frame or NULL)
construction_map <- function(construction, tol_m = MAP_SIMPLIFY_M) {
  cs <- active_construction(construction)
  if (is.null(cs) || !nrow(cs)) return(NULL)
  reason <- tools::toTitleCase(tolower(ifelse(is.na(cs$reason), "", cs$reason)))

  lines <- NULL
  has_geom <- which(lengths(cs$rings) > 0)
  if (length(has_geom)) {
    geoms <- lapply(cs$rings[has_geom], function(rings) {
      sf::st_multilinestring(lapply(rings, function(r) r[, 1:2, drop = FALSE]))
    })
    lines <- sf::st_sf(
      label = sprintf("%s — %s (until %s)", cs$street[has_geom], reason[has_geom],
                      format(cs$finish_date[has_geom], "%d %b %Y")),
      geometry = sf::st_sfc(geoms, crs = 4326)
    )
    lines <- sf::st_transform(sf::st_simplify(sf::st_transform(lines, 3402),
                                              dTolerance = tol_m), 4326)
    lines <- lines[!sf::st_is_empty(lines), ]
    sf::st_geometry(lines) <- sf::st_sfc(lapply(sf::st_geometry(lines), function(g) {
      sf::st_multilinestring(lapply(unclass(sf::st_cast(g, "MULTILINESTRING")),
                                    function(l) round(l, 5)))
    }), crs = 4326)
  }

  pts <- cs[lengths(cs$rings) == 0 & is.finite(cs$lat), ]
  points <- if (nrow(pts)) data.frame(
    lon = pts$lon, lat = pts$lat,
    label = sprintf("%s — %s", pts$street, reason[lengths(cs$rings) == 0 & is.finite(cs$lat)]),
    stringsAsFactors = FALSE
  )
  list(lines = lines, points = points)
}

#' Total network length in km by classification, from route_table().
route_lengths <- function(routes) {
  if (is.null(routes) || !nrow(routes)) return(data.frame())
  agg <- stats::aggregate(routes$km, by = list(classification = routes$classification), FUN = sum)
  names(agg)[2] <- "km"
  agg$cycling <- !agg$classification %in% NON_CYCLING_CLASS
  agg[order(-agg$km), ]
}

#' One simplified MULTILINESTRING per classification, for the Map tab.
#'
#' Projects to Alberta 10-TM (EPSG:3402) so the tolerance is in real metres,
#' and rounds coordinates to 5 decimals (about a metre) because Leaflet ships
#' every digit it is given.
merge_routes_by_class <- function(routes, tol_m = MAP_SIMPLIFY_M) {
  r <- sf::st_transform(routes, 3402)
  r <- sf::st_simplify(r, dTolerance = tol_m)
  r <- sf::st_transform(r, 4326)
  r <- suppressWarnings(sf::st_cast(sf::st_cast(r, "MULTILINESTRING"), "LINESTRING"))
  r <- r[!sf::st_is_empty(r), ]
  parts <- split(sf::st_geometry(r), r$classification)
  geoms <- lapply(parts, function(g) {
    sf::st_multilinestring(lapply(g, function(l) round(unclass(l), 5)))
  })
  sf::st_sf(classification = names(parts),
            geometry = sf::st_sfc(geoms, crs = 4326),
            stringsAsFactors = FALSE)
}

#' Day-of-week x hour-of-day mean cyclist volume from the 15-minute table.
#'
#' Pushed to the server: grouping 10.4M rows into a 7x24 grid takes about a
#' second there and would take minutes if the rows came over the wire.
hour_profile <- function(from, to, force = FALSE) {
  key <- sprintf("-profile-%s-%s", format(as.Date(from), "%Y%m%d"), format(as.Date(to), "%Y%m%d"))
  cached_fetch("intraday", suffix = key, force = force, seed = function() data.frame(), query = function() {
    df <- soql_fetch(
      DATASETS$intraday$id,
      select = paste(
        "date_extract_dow(log_timestamp) AS dow",
        "date_extract_hh(log_timestamp) AS hh",
        "sum(total_cyclist_count) AS cyc",
        "sum(total_pedestrian_count) AS ped",
        "count(*) AS n",
        sep = ", "
      ),
      where = sprintf("log_timestamp >= '%s' AND log_timestamp < '%s'",
                      as.Date(from), as.Date(to)),
      group = "date_extract_dow(log_timestamp), date_extract_hh(log_timestamp)"
    )
    if (!nrow(df)) return(data.frame())
    data.frame(
      dow = as.integer(parse_count(df$dow)),
      hour = as.integer(parse_count(df$hh)),
      cyc = parse_count(df$cyc),
      ped = parse_count(df$ped),
      n   = parse_count(df$n),
      stringsAsFactors = FALSE
    )
  })
}
