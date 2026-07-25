# YEG Bike

A Shiny app for keeping up with Edmonton's bike lanes: what's open, what's
closed, how busy it is, and how that has changed. Data is pulled from the
[City of Edmonton open data portal](https://data.edmonton.ca) when the app
starts, and re-pulled whenever you press **Refresh**.

```
Rscript -e 'shiny::runApp(".", port = 8080)'
```

First run takes about 30 seconds — it fetches roughly 220,000 rows and builds
the route network for the animation. After that, startup is around 3 seconds.

## What's in it

| Tab | What it answers |
|---|---|
| **Overview** | How many people rode yesterday, how the year compares to last, what's under construction |
| **Map** | Where the routes are, how busy each counter is, which bike facilities are disrupted right now |
| **Simulation** | A day of city-wide cycling, animated over a 24-hour clock |
| **Trends** | Daily, seasonal and year-over-year volumes; when in the week people ride; winter cycling |
| **Counters** | Per-counter history, typical day, and how complete each record actually is |
| **Network** | Kilometres by route type and the build-out over time |
| **Data quality** | Every correction the app makes, and the data without them |

## The simulation

Two modes behind one toggle, because they are epistemically different things:

- **Measured** — each counter pulses with its own 15-minute reading. Nothing
  is inferred.
- **Simulated** — the same counts rendered as movement. A rider is spawned for
  every few cyclists recorded and sent along real bike-route geometry in the
  direction the counter registered.

The volumes, timing and direction in simulated mode are real. **The route any
individual rider takes is invented** — a counter records a crossing, not a
journey. The UI says so on screen rather than in a footnote.

Paths come from a routable graph built from the city's route geometry. That
geometry is not topologically noded: only 3% of segment endpoints have another
endpoint within a metre, and where segments do meet, 40% of the time an
endpoint lands mid-way along another segment. Snapping endpoints together
therefore yields a graph in 3,501 fragments whose largest component holds 1% of
the network. Instead every line is densified and snapped to a 20 m grid, which
closes T-junctions and lifts the largest component to 24%. The residual
fragmentation is real — Edmonton has many disconnected trail systems — but 47
of 48 active counters still sit somewhere they can generate 2 km of path.

## Data sources

| Purpose | Dataset | ID |
|---|---|---|
| Route geometry | Bike Routes | `vd4b-a4iv` |
| Daily counts | Daily Pedestrian and Bike Counts | `sw7k-ptx8` |
| 15-minute directional counts | Bike and Pedestrian Counts (Eco Counter) | `tq23-qn4m` |
| Construction affecting bikes | On-street Construction and Maintenance | `7wiq-4rgy` |
| Road closures | Traffic Disruptions | `k4tx-5k8p` |
| Counter hardware and photos | Bike Counter Locations (Eco Counter) | `py7x-4d39` |

`tq23-qn4m` is the important one and is easy to miss: 10.4 million rows at
15-minute resolution with per-direction counts *and* the city's own
`out_of_service` flag. It is queried with server-side `GROUP BY`, so the app
never downloads the bulk of it.

Two datasets that look right but aren't: `mmza-a4ee` ("Non-TDN Bike Route
Disruptions") has two rows and stopped updating in 2021, and `pwsx-cf7d`
("...Permits Affecting Bike Facilities") is a map-canvas view that returns
empty objects over the API. Both are superseded by `7wiq-4rgy` filtered on its
`bike_facilities_affected` column.

### If the portal is unreachable

Each dataset degrades on its own, and independently — one dead endpoint costs
one layer rather than the whole app. The ladder is:

1. **Live** fetch from the portal.
2. **Cache** — the last good copy in `cache/`, at any age.
3. **Seed** — the CSV extracts `Bike_Routes_20251013.csv` and
   `Daily_Pedestrian_and_Bike_Counts_20251102.csv`, *if they are present in the
   working directory*. They are gitignored, so a fresh clone does not have
   them; they are a convenience for the original working copy, not a shipped
   fallback. A clone needs one successful fetch to populate the cache.

The header says which tier is in use and turns amber whenever anything is not
live.

## Data quality

The counter data has real defects. Corrections are applied by default, listed
individually on the **Data quality** tab, and can be switched off entirely with
the **Cleaned data** toggle in the header.

| Rule | What it catches | Effect |
|---|---|---|
| R1 | Counters renamed mid-series (same coordinates, non-overlapping dates) | Merges 57 labels into 55 counters |
| R2 | Duplicate counter-days | 14 rows collapsed to their maximum |
| R3 | Counter outages, from the city's `out_of_service` flag | 2,011 sensor-days set to a gap, not zero |
| R4 | One mode dwarfing the other on dual-mode hardware | 202 readings removed |
| R5 | Readings beyond a per-counter Tukey fence and seasonal norm | 156 readings removed |
| R6 | Blanks in the source | Logged |
| R7 | Zeros with no outage flag | Tagged, never altered |
| R9 | Borderline — above one fence, below the removal threshold | Kept and listed |

Together the removal rules touch 0.34% of the record.

Three decisions worth knowing about:

**R3 is the one that matters most.** The daily table cannot tell "nobody
cycled" from "the counter was dead", and both look like a zero. The 15-minute
table carries the city's own outage flag, so the app joins it on and writes a
gap instead of a zero.

**R5 uses a Tukey fence, not a percentile.** An earlier version used `3 × p99`,
which fails on exactly the cases that matter: when a counter breaks for a week,
the broken values lift p99 enough that they stop qualifying as outliers.
Keillor Point's p99 is 31,222 purely because of its own malfunction, so
`3 × p99` waved through a 51,545-pedestrian day. Q3 and IQR come from the bulk
of the distribution and stay put — Keillor's fence is 937.

**R4 only applies to dual-mode counters.** Twenty of the city's counters are
cyclist-only and report a structural zero for pedestrians. Applying a
cross-modal ratio test to them condemns every busy day they ever had — it
flagged 1,181 perfectly good readings before this was fixed.

The rules are also constrained from the other side. High Level Bridge West's
2,228-cyclist day and Buena Vista Bridge's 2,111 are real peaks and are pinned
by tests so the rules can't drift into over-cleaning.

### Comparability over time

The counter fleet grew from 14 sensors in 2018 to 48 today, so a raw city-wide
total is substantially a chart of how many counters the city has installed.
Seven counters have also been decommissioned, which reads as a collapse. The
Trends tab therefore defaults to **mean per reporting counter** and offers a
**fixed panel** of counters present throughout; the raw total is available and
labelled with the caveat.

## Layout

```
app.R                  UI shell and server assembly
global.R               libraries and sourcing
R/socrata.R            paged, cached, retrying portal access
R/clean.R              data quality rules R1–R9
R/network.R            routable graph and agent paths
R/sim.R                counts to animation payloads
R/theme.R              palette and shared plot theme
R/data.R               the app-wide data bundle
R/deckmap_widget.R     Shiny binding for the animation
R/mod_*.R              one module per tab
www/deckmap.js         deck.gl + MapLibre widget
www/vendor/            pinned deck.gl 9.1.14, maplibre-gl 4.7.1
scripts/build_paths.R  force a route-network rebuild
tests/                 see below
notes/eda.R            the original exploratory analysis, kept for reference
cache/                 generated; safe to delete
```

## Tests

```
Rscript tests/test_clean.R   # 19 assertions on the data quality rules
Rscript tests/test_app.R     # 44 assertions driving every module server
```

`test_app.R` uses `shiny::testServer`, so it exercises the real reactive graph
— a broken renderer fails here rather than showing up as a blank card.

## Colour

Categorical hues and their order are validated for colourblind separation and
contrast in both light and dark, not chosen by eye. Re-ordering the palette in
`R/theme.R` voids that. Magnitude is always a single-hue ramp; the map's
counter intensity is orange so it never reads as the same encoding as the cyan
identity colour used for trails.

## Dependencies

```r
install.packages(c(
  "shiny", "bslib", "leaflet", "plotly", "DT", "sf", "httr2", "jsonlite",
  "readr", "igraph", "shinycssloaders", "htmltools"
))
```

`sfnetworks` and `tidygraph` are optional; the grid-snapping graph builder
doesn't need them.

Basemap tiles come from Carto's free endpoint and need no API key. The
JavaScript libraries are vendored, so only the tiles require network access —
the data layers render without it.
