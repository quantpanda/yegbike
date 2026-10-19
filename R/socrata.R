# ---------------------------------------------------------------------------
# socrata.R -- paged, cached, retrying access to the City of Edmonton open
# data portal (a Socrata instance at data.edmonton.ca).
#
# Two things here are load-bearing:
#
#   1. Socrata's default page size is 1000 rows. A query without an explicit
#      $limit silently truncates -- it does not error. Every fetch in this
#      file pages until it sees a short page.
#   2. The 15-minute counts table has 10.4M rows, far too many to pull. But
#      Socrata will do GROUP BY server side in well under a second, so we push
#      the aggregation down and only ever pull what we render.
# ---------------------------------------------------------------------------

SOCRATA_HOST <- "https://data.edmonton.ca"

# Dataset identifiers, with cache lifetimes chosen by how often each actually
# changes. Route geometry moves a few times a year; construction permits move hourly.
DATASETS <- list(
  routes      = list(id = "vd4b-a4iv", ttl = 7 * 24 * 3600, label = "Bike Routes"),
  daily       = list(id = "sw7k-ptx8", ttl = 6 * 3600,      label = "Daily Pedestrian and Bike Counts"),
  intraday    = list(id = "tq23-qn4m", ttl = 6 * 3600,      label = "Bike and Pedestrian Counts (Eco Counter)"),
  construction= list(id = "7wiq-4rgy", ttl = 1 * 3600,      label = "On-street Construction and Maintenance"),
  counters    = list(id = "py7x-4d39", ttl = 7 * 24 * 3600, label = "Bike Counter Locations (Eco Counter)")
)

PAGE_SIZE <- 50000L

# Where fetched data is cached between runs.
#
# Resolved at load time rather than hard-coded, because a hosted deployment
# (shinyapps.io, Connect, a container) may hand us a read-only app directory.
# When that happens we fall back to the session's temp directory: the cache
# then lasts only as long as the process, which costs one refetch per instance
# and is very much better than failing to start.
#
# Override with YEGBIKE_CACHE_DIR to point at a persistent volume.
resolve_cache_dir <- function() {
  env <- Sys.getenv("YEGBIKE_CACHE_DIR", "")
  candidates <- c(if (nzchar(env)) env, "cache", file.path(tempdir(), "yegbike-cache"))
  for (d in candidates) {
    ok <- tryCatch({
      dir.create(d, showWarnings = FALSE, recursive = TRUE)
      f <- file.path(d, ".write-test")
      writeLines("x", f); unlink(f)
      TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (isTRUE(ok)) return(d)
  }
  # Nothing is writable: run cache-less. Reads will miss, writes will no-op.
  NA_character_
}

CACHE_DIR <- resolve_cache_dir()

# Populated by fetch_soql() so the UI can report what came from the network and
# what came from disk after a failure.
.fetch_state <- new.env(parent = emptyenv())
.fetch_state$log <- list()

cache_path <- function(key) {
  if (is.na(CACHE_DIR)) return(NA_character_)
  file.path(CACHE_DIR, paste0(key, ".rds"))
}

#' Write to the cache, treating failure as a miss rather than an error.
#'
#' A cache is an optimisation. Losing it must never take the app down, so a
#' full disk or a read-only mount degrades to "slower" instead of "broken".
cache_write <- function(obj, path) {
  if (is.na(path)) return(invisible(FALSE))
  ok <- tryCatch({
    dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
    saveRDS(obj, path)
    TRUE
  }, error = function(e) {
    warning(sprintf("could not write cache to %s: %s", path, conditionMessage(e)))
    FALSE
  }, warning = function(w) FALSE)
  invisible(ok)
}

cache_exists <- function(path) !is.na(path) && file.exists(path)

#' Note the outcome of one dataset fetch for the UI banner.
record_fetch <- function(key, status, time = Sys.time(), rows = NA_integer_, msg = NULL) {
  .fetch_state$log[[key]] <- list(
    key = key, status = status, time = time, rows = rows, msg = msg
  )
  invisible(NULL)
}

fetch_log <- function() .fetch_state$log

#' TRUE if any dataset in this session fell back to cache or seed data.
fetch_degraded <- function() {
  any(vapply(.fetch_state$log, function(x) x$status != "live", logical(1)))
}

#' Most recent successful live fetch across all datasets.
last_fetch_time <- function() {
  ts <- vapply(.fetch_state$log, function(x) as.numeric(x$time), numeric(1))
  if (!length(ts)) return(NA)
  as.POSIXct(max(ts), origin = "1970-01-01")
}

# ---------------------------------------------------------------------------
# Query construction
# ---------------------------------------------------------------------------

#' Build a Socrata resource URL from SoQL clauses.
#'
#' Clause values are passed through as written; callers are responsible for
#' valid SoQL. URL-encoding happens here so callers write readable queries.
soql_url <- function(id, select = NULL, where = NULL, group = NULL,
                     order = NULL, limit = NULL, offset = NULL,
                     format = c("json", "csv")) {
  format <- match.arg(format)
  clauses <- list(
    `$select` = select, `$where` = where, `$group` = group,
    `$order` = order, `$limit` = limit, `$offset` = offset
  )
  clauses <- clauses[!vapply(clauses, is.null, logical(1))]
  if (!length(clauses)) return(sprintf("%s/resource/%s.%s", SOCRATA_HOST, id, format))

  # as.character(500000) is "5e+05", which Socrata rejects with a parse error.
  # Integers are safe but doubles are not, so force plain notation for both.
  as_clause <- function(v) {
    if (is.numeric(v)) format(v, scientific = FALSE, trim = TRUE) else as.character(v)
  }
  qs <- paste0(
    utils::URLencode(names(clauses), reserved = TRUE), "=",
    vapply(clauses, function(v) utils::URLencode(as_clause(v), reserved = TRUE), character(1)),
    collapse = "&"
  )
  sprintf("%s/resource/%s.%s?%s", SOCRATA_HOST, id, format, qs)
}

#' One HTTP request, with retry on transient failures.
soql_request <- function(url) {
  httr2::request(url) |>
    httr2::req_user_agent("yegbike (https://github.com/yegbike)") |>
    httr2::req_timeout(120) |>
    httr2::req_retry(max_tries = 3, backoff = function(i) 2^i) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON(flatten = TRUE)
}

#' Fetch a SoQL result set over the CSV endpoint.
#'
#' Roughly three times faster than the JSON endpoint for the bulk tables --
#' 8 seconds against 23 for the 109k-row daily series -- because the payload is
#' smaller and readr parses it far faster than jsonlite builds 109k records.
#'
#' Every column comes back as character and is converted by the caller's
#' normaliser. That is deliberate: Socrata's CSV writes values over 999 with a
#' thousands separator ("1,178"), and letting a type-guesser near that risks a
#' column silently becoming character, or worse, a naive integer cast turning
#' 1,178 into 1. `parse_count()` strips the separator explicitly.
soql_fetch_csv <- function(id, select = NULL, where = NULL, group = NULL,
                           order = NULL, limit = 500000) {
  url <- soql_url(id, select, where, group, order, limit = limit, format = "csv")
  txt <- httr2::request(url) |>
    httr2::req_user_agent("yegbike (https://github.com/yegbike)") |>
    httr2::req_timeout(300) |>
    httr2::req_retry(max_tries = 3, backoff = function(i) 2^i) |>
    httr2::req_perform() |>
    httr2::resp_body_string()
  if (!nzchar(txt)) return(data.frame())
  readr::read_csv(txt, col_types = readr::cols(.default = readr::col_character()),
                  progress = FALSE)
}

#' Fetch a full SoQL result set, paging until exhausted.
#'
#' Socrata caps a single response; we page with $limit/$offset and stop on the
#' first short page. `max_rows` is a safety valve so a mis-specified query
#' can't page forever.
soql_fetch <- function(id, select = NULL, where = NULL, group = NULL,
                       order = NULL, max_rows = 2e6, progress = NULL) {
  pages <- list()
  offset <- 0L
  repeat {
    url <- soql_url(id, select, where, group, order, limit = PAGE_SIZE, offset = offset)
    page <- soql_request(url)
    if (!is.data.frame(page) || nrow(page) == 0L) break
    pages[[length(pages) + 1L]] <- page
    if (is.function(progress)) progress(offset + nrow(page))
    if (nrow(page) < PAGE_SIZE) break
    offset <- offset + PAGE_SIZE
    if (offset >= max_rows) {
      warning(sprintf("soql_fetch(%s) hit max_rows=%s; result may be truncated", id, max_rows))
      break
    }
  }
  if (!length(pages)) return(data.frame())
  do.call(rbind, pages)
}

# ---------------------------------------------------------------------------
# Cached fetch with graceful degradation
# ---------------------------------------------------------------------------

#' Fetch a dataset, preferring cache within its TTL.
#'
#' Degradation ladder, in order: fresh network -> cached rds (any age) ->
#' `seed()` (a local CSV shipped with the repo) -> error. The app stays usable
#' on a train with no wifi; it just says so in the header.
#'
#' @param key   name in DATASETS, also the cache key stem
#' @param query zero-arg function performing the actual fetch
#' @param force skip the TTL check and go to the network
#' @param seed  optional zero-arg fallback when there is no cache at all
cached_fetch <- function(key, query, force = FALSE, seed = NULL, suffix = "") {
  spec <- DATASETS[[key]]
  ttl  <- if (is.null(spec)) 3600 else spec$ttl
  path <- cache_path(paste0(key, suffix))

  if (!force && cache_exists(path)) {
    age <- as.numeric(difftime(Sys.time(), file.mtime(path), units = "secs"))
    if (age < ttl) {
      obj <- tryCatch(readRDS(path), error = function(e) NULL)
      if (!is.null(obj)) {
        record_fetch(key, "cache", file.mtime(path), nrow_safe(obj))
        return(obj)
      }
    }
  }

  out <- tryCatch(query(), error = function(e) e)

  if (!inherits(out, "error")) {
    cache_write(out, path)
    record_fetch(key, "live", Sys.time(), nrow_safe(out))
    return(out)
  }

  # Network failed. Fall back to whatever we have, loudly.
  if (cache_exists(path)) {
    obj <- readRDS(path)
    record_fetch(key, "stale", file.mtime(path), nrow_safe(obj), conditionMessage(out))
    warning(sprintf("[%s] fetch failed (%s); using cache from %s",
                    key, conditionMessage(out), format(file.mtime(path))))
    return(obj)
  }
  if (is.function(seed)) {
    obj <- tryCatch(seed(), error = function(e) NULL)
    if (!is.null(obj)) {
      record_fetch(key, "seed", NA, nrow_safe(obj), conditionMessage(out))
      warning(sprintf("[%s] fetch failed (%s); using bundled seed data",
                      key, conditionMessage(out)))
      return(obj)
    }
  }
  stop(sprintf("[%s] fetch failed and no cache or seed available: %s",
               key, conditionMessage(out)))
}

nrow_safe <- function(x) if (is.data.frame(x)) nrow(x) else NA_integer_

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---------------------------------------------------------------------------
# Bundled seed data
#
# The last resort when the portal is unreachable and there is no cache -- which
# on a hosted deployment is precisely the cold-start-during-an-outage case.
# Shipped as compressed .rds under data/ rather than the original CSV exports:
# 1.4 MB against 13.4 MB, and no re-parsing at startup. Generate with
# scripts/build_deploy_data.R.
# ---------------------------------------------------------------------------

seed_rds <- function(name, dir = "data") {
  f <- file.path(dir, paste0(name, ".rds"))
  if (!file.exists(f)) return(NULL)
  tryCatch(readRDS(f), error = function(e) NULL)
}

seed_csv <- function(file, transform) {
  if (!file.exists(file)) return(NULL)
  tryCatch(
    transform(readr::read_csv(file, show_col_types = FALSE, progress = FALSE)),
    error = function(e) NULL
  )
}

# ---------------------------------------------------------------------------
# Dataset-specific loaders
# ---------------------------------------------------------------------------

#' Bike route geometry as an sf object.
#'
#' We take the CSV representation rather than JSON: Socrata emits WKT there,
#' which sf reads directly, whereas the JSON form nests coordinate arrays that
#' need hand-assembly into MULTILINESTRINGs.
load_routes <- function(force = FALSE) {
  query <- function() {
    url <- sprintf(
      "%s/resource/%s.csv?$query=%s", SOCRATA_HOST, DATASETS$routes$id,
      utils::URLencode(
        "SELECT * WHERE route_coming_soon = false LIMIT 50000",
        reserved = TRUE
      )
    )
    df <- httr2::request(url) |>
      httr2::req_user_agent("yegbike") |>
      httr2::req_timeout(180) |>
      httr2::req_retry(max_tries = 3) |>
      httr2::req_perform() |>
      httr2::resp_body_string() |>
      readr::read_csv(show_col_types = FALSE, progress = FALSE)
    routes_to_sf(df)
  }
  cached_fetch("routes", query, force, seed = function() {
    seed_rds("routes") %||%
      seed_csv("Bike_Routes_20251013.csv", routes_to_sf)
  })
}

routes_to_sf <- function(df) {
  df <- as.data.frame(df)
  df <- df[!is.na(df$geometry_line) & nzchar(df$geometry_line), , drop = FALSE]
  sf::st_as_sf(df, wkt = "geometry_line", crs = 4326)
}

DAILY_SELECT <- paste(
  "counter_location_description", "latitude", "longitude",
  "log_timstamp", "total_pedestrian_count", "total_cyclist_count",
  sep = ", "
)

# One query against the 15-minute table that yields everything the daily view
# needs: counts, position, and the outage flag, rolled up per sensor-day.
#
# The obvious source for daily counts is the daily dataset (sw7k-ptx8), and
# that is what this used to use. But that endpoint is slow out of proportion to
# its size -- a bare `select max(log_timstamp)` over 112k rows takes ten
# seconds, and any filtered read is a full scan. The 15-minute table is 10.4M
# rows and answers the same question in under two seconds, because Socrata
# indexes it properly.
#
# The two agree exactly: 5,352 overlapping sensor-days compared, zero
# mismatches on either measure. tq23 is in fact the cleaner of the two, being
# the source the daily table is derived from -- it does not carry the 14
# duplicated sensor-days that the rollup does.
ROLLUP_SELECT <- paste(
  "counter_location_description",
  "date_trunc_ymd(log_timestamp) AS d",
  "sum(total_cyclist_count) AS cyc",
  "sum(total_pedestrian_count) AS ped",
  "max(latitude) AS lat",
  "max(longitude) AS lon",
  "count(*) AS n_intervals",
  # Only 'Out of Service' means the counter is dead. Since mid-2025 the city
  # also writes 'Outage 1' on ~12 counters a day, and those intervals carry
  # normal, even above-average, counts -- treating any non-null label as an
  # outage blanked 30-40% of summer 2025.
  "sum(case(out_of_service = 'Out of Service', 1, true, 0)) AS n_oos",
  sep = ", "
)
ROLLUP_GROUP <- "counter_location_description, date_trunc_ymd(log_timestamp)"

#' Counts and outages together, fetching the shared rollup only once.
#'
#' load_daily() and load_outages() each derive from the same query. Called
#' independently they would run it twice; app_data() uses this instead.
load_counts_and_outages <- function(force = FALSE) {
  base_c <- baseline_daily()
  base_o <- seed_rds("outages")
  p_c <- cache_path("daily"); p_o <- cache_path("outages")

  # Honour a warm cache before going near the network.
  if (!force && cache_exists(p_c) && cache_exists(p_o)) {
    ttl <- DATASETS$daily$ttl
    fresh <- function(p) as.numeric(difftime(Sys.time(), file.mtime(p), units = "secs")) < ttl
    if (fresh(p_c) && fresh(p_o)) {
      cc <- tryCatch(readRDS(p_c), error = function(e) NULL)
      oo <- tryCatch(readRDS(p_o), error = function(e) NULL)
      if (!is.null(cc) && !is.null(oo)) {
        record_fetch("daily", "cache", file.mtime(p_c), nrow(cc))
        record_fetch("outages", "cache", file.mtime(p_o), nrow(oo))
        return(list(counts = cc, outages = oo))
      }
    }
  }

  since <- if (force || is.null(base_c) || !nrow(base_c)) NULL else max(base_c$date, na.rm = TRUE)
  got <- tryCatch(fetch_rollup(since), error = function(e) e)

  if (inherits(got, "error") || is.null(got)) {
    msg <- if (inherits(got, "error")) conditionMessage(got) else "no rows returned"
    counts  <- if (cache_exists(p_c)) readRDS(p_c) else base_c
    outages <- if (cache_exists(p_o)) readRDS(p_o) else base_o
    if (is.null(counts)) stop(sprintf("could not fetch counts: %s", msg))
    record_fetch("daily", if (cache_exists(p_c)) "stale" else "seed", NA, nrow(counts), msg)
    record_fetch("outages", if (cache_exists(p_o)) "stale" else "seed", NA, nrow_safe(outages), msg)
    return(list(counts = counts, outages = outages %||% base_o))
  }

  counts  <- merge_by_key(base_c, got$counts,  c("sensor_raw", "date"))
  outages <- merge_by_key(base_o, got$outages, c("sensor_raw", "date"))
  cache_write(counts, p_c); cache_write(outages, p_o)
  record_fetch("daily", "live", Sys.time(), nrow(counts))
  record_fetch("outages", "live", Sys.time(), nrow_safe(outages))
  list(counts = counts, outages = outages)
}

#' Fetch the per-sensor-day rollup, optionally only after a date.
fetch_rollup <- function(since = NULL) {
  where <- if (is.null(since)) NULL else
    sprintf("log_timestamp > '%sT00:00:00.000'", format(as.Date(since)))
  df <- soql_fetch_csv(DATASETS$intraday$id, select = ROLLUP_SELECT,
                       group = ROLLUP_GROUP, where = where)
  if (!nrow(df)) return(NULL)
  counts <- data.frame(
    sensor_raw = as.character(df$counter_location_description),
    lat = parse_count(df$lat), lon = parse_count(df$lon),
    date = parse_ts(df$d),
    ped = parse_count(df$ped), cyc = parse_count(df$cyc),
    stringsAsFactors = FALSE
  )
  outages <- data.frame(
    sensor_raw  = counts$sensor_raw,
    date        = counts$date,
    n_intervals = as.integer(parse_count(df$n_intervals)),
    n_oos       = as.integer(parse_count(df$n_oos)),
    stringsAsFactors = FALSE
  )
  list(counts = counts, outages = outages)
}

#' Daily pedestrian and cyclist counts, one row per sensor-day.
#'
#' Loads incrementally where it can. The series runs to 112,000 rows and takes
#' the better part of a minute to pull in full, but all of it except the last
#' few days is settled history -- so we ship a baseline as a deploy artifact
#' and ask the portal only for what came after it. That turns a 38-second cold
#' start into a request for a couple of hundred rows.
#'
#' `force = TRUE` (the Refresh button) always pulls the whole series. That is
#' the escape hatch for the case incremental loading cannot see: the city
#' occasionally revises history rather than only appending to it. They
#' corrected a malfunctioning counter's 2022 readings this way.
load_daily <- function(force = FALSE) {
  query <- function() {
    base <- if (force) NULL else baseline_daily()
    since <- if (is.null(base) || !nrow(base)) NULL else max(base$date, na.rm = TRUE)
    got <- tryCatch(fetch_rollup(since), error = function(e) NULL)
    if (is.null(got)) {
      if (!is.null(base) && nrow(base)) return(base)
      stop("could not fetch counts and no baseline is available")
    }
    merge_by_key(base, got$counts, c("sensor_raw", "date"))
  }
  cached_fetch("daily", query, force, seed = baseline_daily)
}

#' The bundled starting point for the daily series.
baseline_daily <- function() {
  seed_rds("daily") %||%
    seed_csv("Daily_Pedestrian_and_Bike_Counts_20251102.csv", function(df) {
      normalise_daily(data.frame(
        counter_location_description = df[["Counter Location Description"]],
        latitude                     = df$Latitude,
        longitude                    = df$Longitude,
        # The portal CSV export writes "2025 Nov 01 12:00:00 AM"; the API
        # returns ISO. normalise_daily() handles both.
        log_timstamp                 = df$log_timstamp,
        total_pedestrian_count       = df[["Total Pedestrian Count"]],
        total_cyclist_count          = df[["Total Cyclist Count"]],
        stringsAsFactors             = FALSE
      ))
    })
}

#' Append `new` to `old`, letting `new` win on shared keys.
merge_by_key <- function(old, new, keys) {
  if (is.null(new) || !nrow(new)) return(old)
  if (is.null(old) || !nrow(old)) return(new)
  k_old <- do.call(paste, c(old[keys], sep = "\r"))
  k_new <- do.call(paste, c(new[keys], sep = "\r"))
  rbind(old[!k_old %in% k_new, , drop = FALSE], new)
}

#' Coerce a count column to numeric.
#'
#' The CSV export writes values >= 1000 with thousands separators inside the
#' quotes ("1,178"). as.numeric() gives NA and as.integer() gives 1 -- the
#' latter silently, which would make the data look plausible while being wrong.
#' parse_number() strips the separator.
parse_count <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  suppressWarnings(readr::parse_number(as.character(x)))
}

#' Parse both the ISO timestamps the API returns and the "%Y %b %d %I:%M:%S %p"
#' form the portal's CSV export uses.
parse_ts <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  x <- as.character(x)
  d <- suppressWarnings(as.Date(substr(x, 1, 10), format = "%Y-%m-%d"))
  if (anyNA(d)) {
    alt <- suppressWarnings(as.Date(as.POSIXct(x[is.na(d)], format = "%Y %b %d %I:%M:%S %p", tz = "UTC")))
    d[is.na(d)] <- alt
  }
  d
}

normalise_daily <- function(df) {
  if (!nrow(df)) return(df)
  data.frame(
    sensor_raw = as.character(df$counter_location_description),
    lat        = parse_count(df$latitude),
    lon        = parse_count(df$longitude),
    date       = parse_ts(df$log_timstamp),
    ped        = parse_count(df$total_pedestrian_count),
    cyc        = parse_count(df$total_cyclist_count),
    stringsAsFactors = FALSE
  )
}

#' Per-sensor-day outage summary derived from the 15-minute table.
#'
#' This is the single most valuable thing the 15-minute dataset gives us. The
#' daily table cannot distinguish "nobody cycled" from "the counter was dead",
#' but the 15-minute table carries the city's own out_of_service flag. We push
#' the aggregation server side -- counting 10.4M rows into ~110k sensor-days
#' takes about a second and transfers a couple of MB.
OUTAGE_SELECT <- paste(
  "counter_location_description",
  "date_trunc_ymd(log_timestamp) AS d",
  "count(*) AS n_intervals",
  # Only 'Out of Service' means the counter is dead. Since mid-2025 the city
  # also writes 'Outage 1' on ~12 counters a day, and those intervals carry
  # normal, even above-average, counts -- treating any non-null label as an
  # outage blanked 30-40% of summer 2025.
  "sum(case(out_of_service = 'Out of Service', 1, true, 0)) AS n_oos",
  sep = ", "
)
OUTAGE_GROUP <- "counter_location_description, date_trunc_ymd(log_timestamp)"

#' Per-sensor-day outage summary derived from the 15-minute table.
#'
#' Incremental for the same reason as load_daily: this aggregates 10.4M rows
#' into 112k, which costs 26 seconds to transfer in full and is almost entirely
#' unchanging history.
load_outages <- function(force = FALSE) {
  query <- function() {
    base <- if (force) NULL else seed_rds("outages")
    since <- if (is.null(base) || !nrow(base)) NULL else max(base$date, na.rm = TRUE)
    got <- tryCatch(fetch_rollup(since), error = function(e) NULL)
    if (is.null(got)) {
      if (!is.null(base) && nrow(base)) return(base)
      stop("could not fetch outage flags")
    }
    merge_by_key(base, got$outages, c("sensor_raw", "date"))
  }

  # Without any of this the app still runs; it just cannot tell a real zero
  # from a dead counter, so R3 goes quiet. An empty frame degrades cleanly.
  cached_fetch("outages", query, force, seed = function() {
    seed_rds("outages") %||%
      data.frame(sensor_raw = character(), date = as.Date(character()),
                 n_intervals = integer(), n_oos = integer())
  })
}

normalise_outages <- function(df) {
  if (!nrow(df)) {
    return(data.frame(sensor_raw = character(), date = as.Date(character()),
                      n_intervals = integer(), n_oos = integer()))
  }
  data.frame(
    sensor_raw  = as.character(df$counter_location_description),
    date        = parse_ts(df$d),
    n_intervals = as.integer(parse_count(df$n_intervals)),
    n_oos       = as.integer(parse_count(df$n_oos)),
    stringsAsFactors = FALSE
  )
}

#' 15-minute directional counts for a single day. Drives the animation.
#'
#' About 4,600 rows for a recent day -- small enough to fetch on demand each
#' time the user picks a date, so this is cached per-date rather than in bulk.
load_intraday <- function(day, force = FALSE) {
  day <- as.Date(day)
  nxt <- day + 1
  key_suffix <- paste0("-", format(day, "%Y%m%d"))
  query <- function() {
    soql_fetch(
      DATASETS$intraday$id,
      select = paste(
        "counter_location_description", "log_timestamp", "latitude", "longitude",
        "total_cyclist_count", "total_pedestrian_count",
        "cyclist_count_ebd", "cyclist_count_wbd", "cyclist_count_nbd", "cyclist_count_sbd",
        "out_of_service",
        sep = ", "
      ),
      where = sprintf("log_timestamp >= '%s' AND log_timestamp < '%s'", day, nxt),
      order = "log_timestamp"
    ) |> normalise_intraday()
  }
  cached_fetch("intraday", query, force, suffix = key_suffix, seed = function() {
    data.frame()
  })
}

normalise_intraday <- function(df) {
  if (!nrow(df)) return(data.frame())
  # Socrata omits a column entirely from the response when no row in the page
  # has a value for it -- out_of_service is usually absent for a clean day.
  # Every accessor here has to tolerate that.
  num <- function(col) if (is.null(df[[col]])) rep(0, nrow(df)) else {
    v <- parse_count(df[[col]]); v[is.na(v)] <- 0; v
  }
  chr <- function(col) if (is.null(df[[col]])) rep(NA_character_, nrow(df)) else as.character(df[[col]])
  ts <- as.POSIXct(sub("T", " ", as.character(df$log_timestamp)), tz = "UTC")
  data.frame(
    sensor_raw = as.character(df$counter_location_description),
    ts         = ts,
    # Minutes since local midnight -- the animation's time axis.
    minute     = as.integer(difftime(ts, as.POSIXct(paste0(format(ts, "%Y-%m-%d"), " 00:00:00"), tz = "UTC"),
                                     units = "mins")),
    lat = num("latitude"), lon = num("longitude"),
    cyc = num("total_cyclist_count"), ped = num("total_pedestrian_count"),
    ebd = num("cyclist_count_ebd"), wbd = num("cyclist_count_wbd"),
    nbd = num("cyclist_count_nbd"), sbd = num("cyclist_count_sbd"),
    oos = { v <- chr("out_of_service"); !is.na(v) & nzchar(v) },
    stringsAsFactors = FALSE
  )
}

#' Construction permits that currently affect bike facilities.
#'
#' This is the live "is my route open" layer. The dataset the portal advertises
#' for this (pwsx-cf7d) is a map-canvas view that returns empty objects over the
#' API, and the old bike-specific disruptions table (mmza-a4ee) has been dead
#' since 2021 with two rows in it -- so we filter the general OSCAM permits
#' table on its own bike_facilities_affected column instead.
load_construction <- function(force = FALSE) {
  query <- function() {
    soql_fetch(
      DATASETS$construction$id,
      select = paste(
        "file_number", "start_date", "finish_date", "work_reason",
        "street_full_name", "road_segment_intersections", "latitude", "longitude",
        "bike_facilities_affected", "pedestrian_affected", "geometry_multiline",
        sep = ", "
      ),
      where = "bike_facilities_affected = 'Yes'"
    ) |> normalise_construction()
  }
  cached_fetch("construction", query, force, seed = function() data.frame())
}

normalise_construction <- function(df) {
  if (!nrow(df)) return(data.frame())
  chr <- function(col) if (is.null(df[[col]])) rep(NA_character_, nrow(df)) else as.character(df[[col]])
  out <- data.frame(
    file_number = chr("file_number"),
    start_date  = parse_ts(df$start_date),
    finish_date = parse_ts(df$finish_date),
    reason      = chr("work_reason"),
    street      = chr("street_full_name"),
    extent      = chr("road_segment_intersections"),
    lat         = parse_count(df$latitude),
    lon         = parse_count(df$longitude),
    ped_affected= chr("pedestrian_affected"),
    stringsAsFactors = FALSE
  )
  # Keep the MultiLineString rings as a list-column so the map can draw the
  # affected extent, not just a point. Shape varies (nested lists vs matrices)
  # depending on how jsonlite simplified the page, so normalise to a list of
  # two-column matrices per feature.
  raw <- df[["geometry_multiline.coordinates"]]
  out$rings <- if (is.null(raw)) vector("list", nrow(df)) else lapply(raw, as_ring_list)
  out
}

#' Normalise one GeoJSON MultiLineString coordinate blob to a list of Nx2
#' lon/lat matrices.
as_ring_list <- function(x) {
  if (is.null(x) || !length(x)) return(list())
  if (is.matrix(x)) return(list(x[, 1:2, drop = FALSE]))
  if (is.array(x) && length(dim(x)) == 3) {
    return(lapply(seq_len(dim(x)[1]), function(i) matrix(x[i, , 1:2], ncol = 2)))
  }
  if (is.list(x)) {
    return(Filter(Negate(is.null), lapply(x, function(r) {
      m <- if (is.matrix(r)) r else tryCatch(matrix(as.numeric(unlist(r)), ncol = 2, byrow = TRUE),
                                             error = function(e) NULL)
      if (is.null(m) || nrow(m) < 2) NULL else m[, 1:2, drop = FALSE]
    })))
  }
  list()
}

#' Counter metadata: hardware configuration, orientation, and photos.
load_counter_meta <- function(force = FALSE) {
  query <- function() {
    df <- soql_fetch(
      DATASETS$counters$id,
      select = "description, counter_configuration, direction_of_travel, latitude, longitude, image_url"
    )
    if (!nrow(df)) return(data.frame())
    data.frame(
      sensor_raw = as.character(df$description),
      config     = as.character(df$counter_configuration),
      direction  = as.character(df$direction_of_travel),
      lat        = parse_count(df$latitude),
      lon        = parse_count(df$longitude),
      image_url  = as.character(df$image_url),
      stringsAsFactors = FALSE
    )
  }
  cached_fetch("counters", query, force, seed = function() data.frame())
}
