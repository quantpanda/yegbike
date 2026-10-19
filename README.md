# YEG Bike

A Shiny app for keeping up with Edmonton's bike lanes: what's open, what's
closed, how busy it is, and how that has changed. Data is pulled from the
[City of Edmonton open data portal](https://data.edmonton.ca) once per server
process and shared by every visitor; the first visit after it is an hour old
triggers an incremental reload.

```
Rscript -e 'shiny::runApp(".", port = 8080)'
```

Cold start is about 6 seconds, warm about 1.

## How it starts quickly

Most of the counter series is settled history: of 112,000 sensor-days, only the
last few change. So the app ships a baseline under `data/` and asks the portal
only for rows newer than it. A cold start fetches a few hundred rows instead of
112,000, which took the fetch from 64 seconds to under 2.

Incremental loading cannot see one thing: the city occasionally revises
history rather than only appending to it (it corrected a malfunctioning
counter's 2022 readings that way). Re-run `scripts/build_deploy_data.R` and
redeploy every few months to pick those revisions up.

## Built for a small server

The app runs on a small shinyapps.io instance, so:

- **Data loads once per process**, not once per visitor (`shared_bundle()` in
  `R/data.R`). Previously each session spent ~16 s fetching and cleaning and
  held its own 36 MB copy.
- **The map draws one merged shape per route type**, built at deploy time:
  11 features and 0.8 MB instead of 9,860 segments and 5.4 MB. Construction
  outlines are simplified the same way (2.5 MB to 0.4 MB). Sidebar controls
  only show and hide layers, so a click sends about 1 KB.
- **The basemap is Esri's World Gray Canvas**, which needs no API key. CARTO's
  basemaps now serve an "API KEY REQUIRED" image for every tile.

Rebuild the baselines with:

```
Rscript scripts/build_deploy_data.R
```

Run it before redeploying after the city publishes new route geometry, and
every few months to stop the baseline drifting from corrected history.

## What's in it

| Tab | What it answers |
|---|---|
| **Overview** | How many people rode yesterday, how the year compares to last, what's under construction |
| **Map** | Where the routes are, how busy each counter is, which bike facilities are under construction right now |
| **Trends** | Daily, seasonal and year-over-year volumes; when in the week people ride; winter cycling; then per-counter history and typical day |
| **About** | Kilometres by route type, the build-out over time, and the data quality rules with a before/after chart |

## Data sources

| Purpose | Dataset | ID |
|---|---|---|
| Route geometry | Bike Routes | `vd4b-a4iv` |
| Counts, positions and outages | Bike and Pedestrian Counts (Eco Counter) | `tq23-qn4m` |
| Construction affecting bikes | On-street Construction and Maintenance | `7wiq-4rgy` |
| Road closures | Traffic Disruptions | `k4tx-5k8p` |
| Counter hardware and photos | Bike Counter Locations (Eco Counter) | `py7x-4d39` |

`tq23-qn4m` does nearly all the work: 10.4 million rows at 15-minute
resolution with per-direction counts *and* the city's own `out_of_service`
flag. Everything the daily view needs — counts, positions, outage flags — comes
from one server-side `GROUP BY` against it, so the app never downloads the bulk.

The app used to take daily counts from the obvious source, "Daily Pedestrian
and Bike Counts" (`sw7k-ptx8`), and that turned out to be a mistake on two
counts. It is slow out of all proportion to its size — a bare
`select max(log_timstamp)` over 112k rows takes ten seconds, and any filtered
read is a full scan — while the same rollup from the 10.4M-row table returns in
under two. And it is *dirtier*: being a derived table it carries 14 duplicated
sensor-days that its own source does not. The two agree exactly otherwise
(5,352 overlapping sensor-days compared, zero mismatches).

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

The counter data has real defects. Corrections are always applied; the rules
and a raw-versus-cleaned chart are on the **About** tab.

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
R/theme.R              palette and shared plot theme
R/data.R               the shared data bundle and derived views
R/mod_*.R              one module per tab section
scripts/build_deploy_data.R  baselines and map geometry shipped in data/
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
