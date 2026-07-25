# ---------------------------------------------------------------------------
# data.R -- assemble everything the app needs into one bundle.
#
# Called once at startup and again whenever the user hits Refresh. Every fetch
# degrades independently, so one dead endpoint costs one layer rather than the
# whole app.
# ---------------------------------------------------------------------------

#' Load and clean every dataset the app uses.
#'
#' @param force  bypass the disk cache and re-fetch from data.edmonton.ca
#' @param notify optional function(step_label, fraction) for a progress bar
app_data <- function(force = FALSE, notify = NULL) {
  step <- function(label, frac) if (is.function(notify)) notify(label, frac)

  step("Bike routes", 0.05)
  routes <- try_layer(load_routes(force))

  step("Daily counts", 0.25)
  daily <- try_layer(load_daily(force))

  step("Sensor outages", 0.50)
  outages <- try_layer(load_outages(force), fallback = data.frame())

  step("Counter details", 0.65)
  meta <- try_layer(load_counter_meta(force), fallback = data.frame())

  step("Construction", 0.75)
  construction <- try_layer(load_construction(force), fallback = data.frame())

  step("Disruptions", 0.85)
  disruptions <- try_layer(load_disruptions(force), fallback = data.frame())

  step("Applying data quality rules", 0.90)
  cleaned <- clean_counts(daily, outages, meta)

  # The animation's route library is derived from geometry that changes a few
  # times a year, so it is precomputed. Build it on first run rather than
  # making the user remember a setup script; ten seconds once, then cached.
  paths <- load_path_cache()
  if (is.null(paths) && !is.null(routes) && nrow(routes)) {
    step("Building the route network (first run only)", 0.94)
    paths <- try_layer(build_path_cache(routes, recent_counters(cleaned$data)))
  }

  step("Ready", 1)
  list(
    routes       = routes,
    counts       = cleaned$data,
    ledger       = cleaned$ledger,
    qa_summary   = cleaned$summary,
    aliases      = cleaned$aliases,
    meta         = meta,
    construction = construction,
    disruptions  = disruptions,
    paths        = paths,
    fetched_at   = Sys.time(),
    fetch_log    = fetch_log()
  )
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

#' The day with the most cyclists counted city-wide.
#'
#' A raw total, deliberately, unlike the per-counter mean the trend charts use.
#' The mean is right for trends and wrong for "busiest": on 2021-05-30 only
#' five counters reported, and 4,048 riders across five sensors averages higher
#' than a full 48-counter day carrying four times the traffic -- which made the
#' Simulation tab's "Busiest day" button open a near-empty map.
#'
#' @return list(date, total, n_sensors)
busiest_day <- function(counts) {
  s <- daily_series(counts, "cyc", "total")
  if (!nrow(s)) return(NULL)
  i <- which.max(s$value)
  list(date = s$date[i], total = s$value[i], n_sensors = s$n_sensors[i])
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

#' Total network length in km by classification.
route_lengths <- function(routes) {
  if (is.null(routes) || !nrow(routes)) return(data.frame())
  len_m <- as.numeric(sf::st_length(sf::st_transform(routes, 3402)))
  agg <- stats::aggregate(len_m, by = list(classification = routes$classification), FUN = sum)
  names(agg)[2] <- "metres"
  agg$km <- agg$metres / 1000
  agg$cycling <- !agg$classification %in% NON_CYCLING_CLASS
  agg[order(-agg$km), ]
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
