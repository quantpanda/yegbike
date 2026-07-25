# ---------------------------------------------------------------------------
# theme.R -- one palette and one plot theme, shared by every chart.
#
# The categorical hues and their ORDER are validated, not chosen by eye: the
# sequence below clears the colourblind-separation and normal-vision gates on
# adjacent pairs in both light and dark. Re-order it and those guarantees are
# void. Three of the light hues sit under 3:1 against the surface, which is why
# every chart here carries either direct labels or a table view.
# ---------------------------------------------------------------------------

PAL <- list(
  # Categorical: assign in fixed order, never cycled. Past 8 series, fold the
  # tail into "Other" rather than inventing a ninth hue.
  cat = c("#2a78d6", "#eb6834", "#1baf7a", "#eda100",
          "#e87ba4", "#008300", "#4a3aa7", "#e34948"),

  # Sequential, single hue, light -> dark. For continuous magnitude only.
  seq_blue = c("#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5",
               "#256abf", "#184f95", "#0d366b"),

  # Diverging: two poles that read as opposite, neutral grey midpoint.
  div = c("#184f95", "#6da7ec", "#f0efec", "#ec835a", "#d03b3b"),

  status = c(good = "#0ca30c", warning = "#fab219",
             serious = "#ec835a", critical = "#d03b3b"),

  surface   = "#fcfcfb",
  plane     = "#f9f9f7",
  ink       = "#0b0b0b",
  ink2      = "#52514e",
  muted     = "#898781",
  grid      = "#e1e0d9",
  axis      = "#c3c2b7",
  border    = "rgba(11,11,11,0.10)"
)

# Cyclists and pedestrians appear together constantly; pinning them to fixed
# slots means the colour never moves when one is filtered out.
MODE_COLOURS <- c(Cyclists = PAL$cat[1], Pedestrians = PAL$cat[2])

#' Shared plotly layout: recessive chrome, readable ink, no chart junk.
plotly_base <- function(p, title = NULL, xlab = "", ylab = "", legend = TRUE,
                        hovermode = "x unified") {
  plotly::layout(
    p,
    title = if (is.null(title)) NULL else list(
      text = title, x = 0, xanchor = "left", font = list(size = 14, color = PAL$ink)
    ),
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    font = list(family = "system-ui, -apple-system, 'Segoe UI', sans-serif",
                size = 12, color = PAL$ink2),
    xaxis = list(title = list(text = xlab, font = list(size = 11, color = PAL$muted)),
                 gridcolor = PAL$grid, zeroline = FALSE,
                 linecolor = PAL$axis, tickfont = list(size = 11, color = PAL$muted)),
    yaxis = list(title = list(text = ylab, font = list(size = 11, color = PAL$muted)),
                 gridcolor = PAL$grid, zeroline = FALSE,
                 linecolor = PAL$axis, tickfont = list(size = 11, color = PAL$muted)),
    hovermode = hovermode,
    hoverlabel = list(bgcolor = "#ffffff", bordercolor = PAL$axis,
                      font = list(size = 12, color = PAL$ink)),
    showlegend = legend,
    legend = list(orientation = "h", x = 0, y = 1.06, xanchor = "left",
                  font = list(size = 11, color = PAL$ink2), bgcolor = "rgba(0,0,0,0)"),
    margin = list(l = 56, r = 18, t = if (is.null(title)) 34 else 52, b = 44)
  ) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
}

#' Interpolate a hex ramp to n steps.
ramp_n <- function(cols, n) grDevices::colorRampPalette(cols)(n)

#' Human-readable integer with thousands separators.
fmt_int <- function(x) formatC(round(x), format = "d", big.mark = ",")

#' Compact number for stat tiles (12.4k, 1.2M).
fmt_compact <- function(x) {
  ifelse(is.na(x), "—",
    ifelse(abs(x) >= 1e6, sprintf("%.1fM", x / 1e6),
      ifelse(abs(x) >= 1e4, sprintf("%.0fk", x / 1e3),
             formatC(round(x), format = "d", big.mark = ","))))
}

#' A stat tile: big number, label, optional delta with direction.
#'
#' The delta carries an arrow glyph as well as colour, so the direction is
#' never encoded by colour alone.
stat_tile <- function(value, label, sub = NULL, delta = NULL, accent = PAL$cat[1]) {
  d <- NULL
  if (!is.null(delta) && !is.na(delta)) {
    up <- delta >= 0
    d <- htmltools::span(
      class = paste("tile-delta", if (up) "is-up" else "is-down"),
      sprintf("%s %+.1f%%", if (up) "↑" else "↓", 100 * delta)
    )
  }
  htmltools::div(
    class = "stat-tile",
    style = sprintf("--tile-accent:%s;", accent),
    htmltools::div(class = "tile-label", label),
    htmltools::div(class = "tile-value", value),
    htmltools::div(class = "tile-foot", d,
                   if (!is.null(sub)) htmltools::span(class = "tile-sub", sub))
  )
}
