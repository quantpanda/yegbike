# ---------------------------------------------------------------------------
# network.R -- turn the bike route geometry into a routable graph, and grow
# plausible cycling paths outward from each counter.
#
# This exists to serve the animation. A counter tells us how many cyclists
# passed a point and in which direction; it says nothing about where they came
# from or went. To draw movement we need paths, and the only honest source for
# those is the city's own route geometry -- a cyclist counted heading east at
# 102 Avenue is somewhere on the eastbound bike network near 102 Avenue.
#
# The output is therefore explicitly *synthetic paths carrying real counts*,
# and the UI says so. Nothing here claims to be a GPS trace.
# ---------------------------------------------------------------------------

# Classifications that are not cycling infrastructure. "Walkway / Breezeway"
# alone is 3,031 of the 9,766 features -- a third of what a naive render of
# this dataset shows as "the bike network" is sidewalk.
NON_CYCLING_CLASS <- c("Walkway / Breezeway", "Maintenance Access")

# Spacing, in metres, used to both densify and snap the network.
#
# The route dataset is a collection of independent line features, not a noded
# network: only 3% of segment endpoints have another endpoint within a metre,
# and the median nearest endpoint is 140 m away. Snapping endpoints to each
# other therefore produces a graph in 3,501 fragments whose largest component
# holds 1% of the network -- useless for routing.
#
# The reason is that where segments do meet, 40% of the time an endpoint lands
# in the *middle* of another segment (a T-junction), which shares no vertex
# with it. Rather than run a full geometric noding pass, we densify every line
# so it has a vertex at least this often, then snap all vertices to a grid of
# the same size. A T-junction then lands in the same grid cell as the segment
# it touches, and the network connects.
#
# 20 m, chosen by measurement rather than taste. Across 12/20/30 m it puts the
# most counters (35 of 47) on a large connected component and yields the
# largest single component, while staying tight enough to limit false
# junctions -- grid snapping will happily connect a bike overpass to the road
# beneath it if the cells are too coarse.
#
# Note the residual fragmentation is real, not an artifact: even at 30 m the
# network has over a thousand components. Edmonton genuinely has many
# disconnected trail systems, so full connectivity was never available.
GRID_M <- 20

#' Split MULTILINESTRING features into a flat list of coordinate matrices.
explode_lines <- function(routes) {
  geom <- sf::st_geometry(routes)
  out <- vector("list", length(geom))
  for (i in seq_along(geom)) {
    g <- geom[[i]]
    parts <- if (inherits(g, "MULTILINESTRING")) unclass(g) else list(unclass(g))
    out[[i]] <- lapply(parts, function(m) {
      m <- m[, 1:2, drop = FALSE]
      if (nrow(m) >= 2) m else NULL
    })
  }
  feature_id <- rep(seq_along(out), vapply(out, length, integer(1)))
  lines <- unlist(out, recursive = FALSE)
  keep <- !vapply(lines, is.null, logical(1))
  list(lines = lines[keep], feature = feature_id[keep])
}

#' Great-circle distance in metres between consecutive coordinates.
seg_length_m <- function(m) {
  if (nrow(m) < 2) return(0)
  lat <- m[, 2] * pi / 180
  dx <- diff(m[, 1]) * 111320 * cos((lat[-1] + lat[-length(lat)]) / 2)
  dy <- diff(m[, 2]) * 110540
  sum(sqrt(dx^2 + dy^2))
}

#' Compass bearing in degrees from the first to the last vertex of a path.
path_bearing <- function(m) {
  if (nrow(m) < 2) return(NA_real_)
  lat1 <- m[1, 2] * pi / 180; lat2 <- m[nrow(m), 2] * pi / 180
  dlon <- (m[nrow(m), 1] - m[1, 1]) * pi / 180
  y <- sin(dlon) * cos(lat2)
  x <- cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(dlon)
  (atan2(y, x) * 180 / pi) %% 360
}

#' Insert vertices along a coordinate matrix so no gap exceeds `step` metres.
densify <- function(m, step_lon, step_lat) {
  n <- nrow(m)
  if (n < 2) return(m)
  dx <- diff(m[, 1]) / step_lon
  dy <- diff(m[, 2]) / step_lat
  reps <- pmax(1L, ceiling(sqrt(dx^2 + dy^2)))
  if (all(reps == 1L)) return(m)

  total <- sum(reps) + 1L
  out <- matrix(NA_real_, nrow = total, ncol = 2)
  pos <- 1L
  for (i in seq_len(n - 1L)) {
    k <- reps[i]
    t <- seq.int(0, k - 1L) / k
    out[pos:(pos + k - 1L), 1] <- m[i, 1] + t * (m[i + 1L, 1] - m[i, 1])
    out[pos:(pos + k - 1L), 2] <- m[i, 2] + t * (m[i + 1L, 2] - m[i, 2])
    pos <- pos + k
  }
  out[total, ] <- m[n, ]
  out
}

#' Build an undirected routable graph from route geometry.
#'
#' Nodes are grid cells of GRID_M metres; edges join cells that are adjacent
#' along a densified route line. See the GRID_M comment for why this is done by
#' snapping rather than by matching endpoints.
build_graph <- function(routes, cycling_only = TRUE, grid_m = GRID_M) {
  if (cycling_only) {
    routes <- routes[!routes$classification %in% NON_CYCLING_CLASS, ]
  }
  ex <- explode_lines(routes)
  lines <- ex$lines
  if (!length(lines)) stop("no usable line geometry")

  # Degrees per grid step. Longitude degrees shrink with latitude, so use the
  # city's own latitude rather than the equator.
  lat0 <- stats::median(vapply(lines, function(m) m[1, 2], numeric(1)))
  step_lat <- grid_m / 110540
  step_lon <- grid_m / (111320 * cos(lat0 * pi / 180))

  dens <- lapply(lines, densify, step_lon = step_lon, step_lat = step_lat)
  npt  <- vapply(dens, nrow, integer(1))
  all_xy <- do.call(rbind, dens)

  # Snap to the grid and index cells.
  gx <- round(all_xy[, 1] / step_lon)
  gy <- round(all_xy[, 2] / step_lat)
  key <- paste(gx, gy)
  node_key <- unique(key)
  idx <- stats::setNames(seq_along(node_key), node_key)
  node_id <- unname(idx[key])

  # Node coordinates: the cell centre, so co-located vertices collapse exactly.
  kk <- do.call(rbind, strsplit(node_key, " ", fixed = TRUE))
  node_xy <- cbind(as.numeric(kk[, 1]) * step_lon, as.numeric(kk[, 2]) * step_lat)

  # Consecutive vertices within a line become edges; boundaries between lines
  # must not be joined.
  line_end <- cumsum(npt)
  is_last  <- logical(length(node_id))
  is_last[line_end] <- TRUE
  from <- node_id[!is_last]
  to   <- node_id[c(!is_last[-1], FALSE)]
  # `to` must be the *next* vertex, so rebuild by shifting within the flat run.
  to <- node_id[which(!is_last) + 1L]

  keep <- from != to
  from <- from[keep]; to <- to[keep]
  # Undirected: canonicalise orientation before de-duplicating.
  a <- pmin(from, to); b <- pmax(from, to)
  dup <- duplicated(paste(a, b))
  a <- a[!dup]; b <- b[!dup]

  len <- sqrt(((node_xy[a, 1] - node_xy[b, 1]) * 111320 * cos(lat0 * pi / 180))^2 +
              ((node_xy[a, 2] - node_xy[b, 2]) * 110540)^2)

  edges <- data.frame(from = a, to = b, len = len, stringsAsFactors = FALSE)
  g <- igraph::graph_from_data_frame(
    edges[, c("from", "to")], directed = FALSE,
    vertices = data.frame(name = seq_along(node_key))
  )
  inc <- igraph::incident_edges(g, igraph::V(g), mode = "all")

  structure(list(
    graph = g, edges = edges, node_xy = node_xy,
    incident = lapply(inc, as.integer), grid_m = grid_m
  ), class = "bike_graph")
}

#' Share of edges in the largest connected component.
#'
#' The plan's decision rule: if endpoint snapping leaves the network badly
#' fragmented, fall back to splitting segments where they cross. Reported so
#' the choice is evidence-based rather than assumed.
component_report <- function(bg) {
  comp <- igraph::components(bg$graph)
  big  <- which.max(comp$csize)
  in_big <- comp$membership[bg$edges$from] == big
  list(
    n_components = comp$no,
    largest_share = mean(in_big),
    largest_nodes = comp$csize[big],
    membership = comp$membership
  )
}

#' Index of the graph node nearest a lon/lat, within `max_m` metres.
nearest_node <- function(bg, lon, lat, max_m = 400) {
  dx <- (bg$node_xy[, 1] - lon) * 111320 * cos(lat * pi / 180)
  dy <- (bg$node_xy[, 2] - lat) * 110540
  d2 <- dx^2 + dy^2
  i <- which.min(d2)
  if (sqrt(d2[i]) > max_m) return(NA_integer_)
  as.integer(i)
}

#' Grow one path outward from a node, biased toward a compass bearing.
#'
#' At each junction we score the available edges by how well they continue in
#' the desired direction and pick among them with a softmax, so the walks
#' spread across parallel corridors instead of every agent tracing the same
#' shortest path. Immediate backtracking is suppressed.
#'
#' @param target_deg desired bearing, or NA for an unbiased walk
#' @return list(coords = Nx2 matrix, length_m = numeric) or NULL
random_walk <- function(bg, start, target_deg, target_m = 3000, bias = 3,
                        max_steps = 1200) {
  node <- start
  prev_node <- -1L
  path <- integer(max_steps + 1L)
  path[1] <- node
  n <- 1L
  total <- 0
  xy <- bg$node_xy
  lat_scale <- 111320 * cos(xy[start, 2] * pi / 180)

  for (step in seq_len(max_steps)) {
    cand_e <- bg$incident[[node]]
    if (!length(cand_e)) break
    nxt <- ifelse(bg$edges$from[cand_e] == node, bg$edges$to[cand_e], bg$edges$from[cand_e])
    # Don't immediately double back, unless that is the only way out.
    keep <- nxt != prev_node
    if (any(keep)) { cand_e <- cand_e[keep]; nxt <- nxt[keep] }

    if (is.na(target_deg) || length(nxt) == 1L) {
      pick <- if (length(nxt) == 1L) 1L else sample.int(length(nxt), 1L)
    } else {
      # Bearing of each candidate hop, scored by alignment with the direction
      # the counter recorded. Softmax rather than argmax so the walks fan out
      # across parallel corridors instead of all tracing one line.
      dx <- (xy[nxt, 1] - xy[node, 1]) * lat_scale
      dy <- (xy[nxt, 2] - xy[node, 2]) * 110540
      brg <- (atan2(dx, dy) * 180 / pi) %% 360
      w <- exp(bias * cos((brg - target_deg) * pi / 180))
      w[!is.finite(w)] <- .Machine$double.eps
      pick <- sample.int(length(nxt), 1L, prob = w / sum(w))
    }

    total <- total + bg$edges$len[cand_e[pick]]
    prev_node <- node
    node <- nxt[pick]
    n <- n + 1L
    path[n] <- node
    if (total >= target_m) break
  }

  if (n < 3L || total < target_m * 0.25) return(NULL)
  list(coords = xy[path[seq_len(n)], , drop = FALSE], length_m = total)
}

# Directions a counter can report, as compass bearings.
DIRECTION_BEARINGS <- c(ebd = 90, wbd = 270, nbd = 0, sbd = 180)

#' Precompute agent paths radiating from every counter, per direction.
#'
#' @return named list: paths[[sensor]][[direction]] = list of coordinate matrices
build_counter_paths <- function(bg, counters, n_paths = 24, target_m = 3000, seed = 42) {
  set.seed(seed)
  out <- list()
  for (i in seq_len(nrow(counters))) {
    s <- counters$sensor[i]
    node <- nearest_node(bg, counters$lon[i], counters$lat[i])
    if (is.na(node)) {
      out[[s]] <- NULL
      next
    }
    per_dir <- list()
    for (dn in names(DIRECTION_BEARINGS)) {
      walks <- vector("list", n_paths)
      for (k in seq_len(n_paths)) {
        w <- random_walk(bg, node, DIRECTION_BEARINGS[[dn]],
                         target_m = stats::runif(1, target_m * 0.6, target_m * 1.4))
        walks[[k]] <- if (is.null(w)) NULL else simplify_path(w$coords)
      }
      walks <- Filter(Negate(is.null), walks)
      if (length(walks)) per_dir[[dn]] <- walks
    }
    if (length(per_dir)) out[[s]] <- per_dir
  }
  out
}

#' Build the graph and per-counter path library, and cache it.
#'
#' Route geometry changes a few times a year while counts change daily, so this
#' is precomputed rather than rebuilt per request. Takes about ten seconds.
#'
#' @param log function(msg) for progress, or NULL
build_path_cache <- function(routes, counters, path = "cache/paths.rds", log = NULL) {
  say <- function(...) if (is.function(log)) log(sprintf(...))

  bg <- build_graph(routes)
  rep <- component_report(bg)
  say("graph: %d nodes, %d edges, largest component %.1f%%",
      nrow(bg$node_xy), nrow(bg$edges), 100 * rep$largest_share)

  paths <- build_counter_paths(bg, counters)
  say("grew %d paths across %d counters",
      sum(vapply(paths, function(p) sum(lengths(p)), integer(1))), length(paths))

  out <- list(paths = paths, counters = counters, built_at = Sys.time(),
              component_share = rep$largest_share, grid_m = bg$grid_m)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  saveRDS(out, path)
  out
}

#' Counters that have reported recently, with a representative position.
recent_counters <- function(counts, days = 90) {
  recent <- counts[counts$date >= max(counts$date, na.rm = TRUE) - days, ]
  recent <- recent[is.finite(recent$lat) & is.finite(recent$lon), ]
  stats::aggregate(cbind(lat, lon) ~ sensor, data = recent, FUN = stats::median)
}

#' Douglas-Peucker-lite: drop vertices that barely deviate from the line
#' between their neighbours.
#'
#' The tolerance is deliberately close to the grid spacing. Densifying to 20 m
#' leaves long collinear runs on straight roads, and collapsing them cuts the
#' path library -- the bulk of the animation payload -- by more than half with
#' no visible change at city zoom.
simplify_path <- function(m, tol_deg = 1.2e-4) {
  if (nrow(m) <= 3) return(m)
  keep <- rep(TRUE, nrow(m))
  for (i in 2:(nrow(m) - 1)) {
    if (!keep[i - 1]) next
    a <- m[i - 1, ]; b <- m[i, ]; c <- m[i + 1, ]
    num <- abs((c[1] - a[1]) * (a[2] - b[2]) - (a[1] - b[1]) * (c[2] - a[2]))
    den <- sqrt((c[1] - a[1])^2 + (c[2] - a[2])^2)
    if (den > 0 && num / den < tol_deg) keep[i] <- FALSE
  }
  m[keep, , drop = FALSE]
}
