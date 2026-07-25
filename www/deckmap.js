/* ---------------------------------------------------------------------------
 * deckmap.js -- a Shiny output binding wrapping deck.gl over MapLibre.
 *
 * Renders one day of Edmonton cycling counts in two modes:
 *
 *   measured   ScatterplotLayer, one dot per counter, radius and colour driven
 *              by that counter's actual 15-minute reading. Nothing inferred.
 *
 *   simulated  TripsLayer. Agents move along real bike-route geometry at a
 *              rate set by the real counts. Paths are synthetic; the counts
 *              are not. The UI says so.
 *
 * The animation clock is in minutes since local midnight, 0..1440, which is
 * also the unit the R side uses for agent start times and durations.
 * ------------------------------------------------------------------------- */

(function () {
  const MINUTES_PER_DAY = 1440;
  const TRAIL_MINUTES = 9;

  // Time multipliers. "1x" means one simulated minute per real second, which
  // walks the whole day in 24 seconds -- slow enough to read the commute peaks.
  const SPEEDS = { 1: 1, 4: 4, 16: 16 };

  function lerpColor(a, b, t) {
    return [
      Math.round(a[0] + (b[0] - a[0]) * t),
      Math.round(a[1] + (b[1] - a[1]) * t),
      Math.round(a[2] + (b[2] - a[2]) * t),
    ];
  }

  /* Counter intensity is a magnitude, so this is a single-hue sequential ramp
   * (orange), not a rainbow. On a dark basemap the light end of a sequential
   * scale is the *loud* end, so the ramp runs dim-to-bright: near-zero recedes
   * toward the map surface and busy counters glow.
   *
   * Orange rather than blue because the simulated-mode trails are already
   * cyan; keeping magnitude and identity on different hues stops the two
   * modes from reading as the same encoding. */
  const RAMP = [
    [58, 36, 23],
    [107, 58, 30],
    [162, 85, 31],
    [209, 116, 31],
    [240, 153, 63],
    [255, 194, 122],
  ];

  function rampColor(t) {
    if (!isFinite(t) || t <= 0) return RAMP[0];
    if (t >= 1) return RAMP[RAMP.length - 1];
    const x = t * (RAMP.length - 1);
    const i = Math.floor(x);
    return lerpColor(RAMP[i], RAMP[i + 1], x - i);
  }

  class DeckMap {
    constructor(el) {
      this.el = el;
      this.mode = "measured";
      this.playing = false;
      this.speed = 4;
      this.time = 7 * 60; // open on the morning commute rather than midnight
      this.payload = null;
      this.trips = null;
      this.lastFrame = null;
      this.hovered = null;

      this._buildDom();
      this._initMap();
    }

    _buildDom() {
      this.el.classList.add("deckmap");
      this.el.innerHTML = `
        <div class="deckmap-canvas"></div>
        <div class="deckmap-badge" data-role="badge"></div>
        <div class="deckmap-tooltip" data-role="tooltip" hidden></div>
        <div class="deckmap-controls">
          <button class="dm-play" data-role="play" aria-label="Play">
            <span data-role="play-icon">&#9654;</span>
          </button>
          <div class="dm-clock" data-role="clock">07:00</div>
          <input class="dm-scrub" data-role="scrub" type="range"
                 min="0" max="${MINUTES_PER_DAY}" step="1" value="420"
                 aria-label="Time of day">
          <div class="dm-speeds" role="group" aria-label="Playback speed">
            <button data-speed="1">1&times;</button>
            <button data-speed="4" class="on">4&times;</button>
            <button data-speed="16">16&times;</button>
          </div>
        </div>
        <div class="deckmap-modes" role="group" aria-label="View mode">
          <button data-mode="measured" class="on">Measured</button>
          <button data-mode="simulated">Simulated</button>
        </div>
        <div class="deckmap-legend" data-role="legend"></div>
      `;

      const q = (r) => this.el.querySelector(`[data-role="${r}"]`);
      this.dom = {
        canvas: this.el.querySelector(".deckmap-canvas"),
        badge: q("badge"),
        tooltip: q("tooltip"),
        play: q("play"),
        playIcon: q("play-icon"),
        clock: q("clock"),
        scrub: q("scrub"),
        legend: q("legend"),
      };

      this.dom.play.addEventListener("click", () => this.toggle());
      this.dom.scrub.addEventListener("input", (e) => {
        this.time = Number(e.target.value);
        this._syncClock();
        this._render();
      });
      this.el.querySelectorAll("[data-speed]").forEach((b) => {
        b.addEventListener("click", () => {
          this.speed = SPEEDS[b.dataset.speed] || 4;
          this.el.querySelectorAll("[data-speed]").forEach((x) => x.classList.remove("on"));
          b.classList.add("on");
        });
      });
      this.el.querySelectorAll("[data-mode]").forEach((b) => {
        b.addEventListener("click", () => this.setMode(b.dataset.mode));
      });
    }

    _initMap() {
      this.map = new maplibregl.Map({
        container: this.dom.canvas,
        style: "https://basemaps.cartocdn.com/gl/dark-matter-gl-style/style.json",
        center: [-113.4938, 53.5461],
        zoom: 11.2,
        pitch: 45,
        bearing: -15,
        attributionControl: { compact: true },
      });
      this.map.addControl(new maplibregl.NavigationControl({ showCompass: true }), "top-left");

      this.overlay = new deck.MapboxOverlay({
        interleaved: false,
        layers: [],
        getTooltip: null,
      });
      this.map.on("load", () => {
        this.map.addControl(this.overlay);
        this._render();
      });
    }

    // -- data ---------------------------------------------------------------

    setPayload(payload) {
      this.payload = payload;
      this.trips = null;
      if (payload && payload.simulated && payload.simulated.paths &&
          payload.simulated.paths.length) {
        this.trips = this._buildTrips(payload.simulated);
      }
      this._syncBadge();
      this._syncLegend();
      this._render();
    }

    /* Expand the compact agent representation into TripsLayer objects.
     *
     * R sends a path library plus, per agent, a path index, a start minute and
     * a duration. Timestamps are derived here from cumulative distance so a
     * rider moves at a constant speed along their route rather than jumping
     * between vertices at a uniform rate. Path arrays are shared by reference,
     * not copied -- many agents ride the same geometry. */
    _buildTrips(sim) {
      const paths = sim.paths;
      const cum = paths.map((p) => {
        const c = new Float32Array(p.length);
        let total = 0;
        for (let i = 1; i < p.length; i++) {
          const dx = (p[i][0] - p[i - 1][0]) * 66000; // metres per deg lon at 53.5N
          const dy = (p[i][1] - p[i - 1][1]) * 110540;
          total += Math.sqrt(dx * dx + dy * dy);
          c[i] = total;
        }
        if (total > 0) for (let i = 0; i < c.length; i++) c[i] /= total;
        return c;
      });

      const { p, t, d } = sim.agents;
      const n = Array.isArray(p) ? p.length : 0;
      const out = new Array(n);
      for (let i = 0; i < n; i++) {
        const pi = p[i];
        const frac = cum[pi];
        const start = t[i];
        const dur = d[i];
        const ts = new Array(frac.length);
        for (let k = 0; k < frac.length; k++) ts[k] = start + frac[k] * dur;
        out[i] = { path: paths[pi], timestamps: ts };
      }
      return out;
    }

    // -- playback -----------------------------------------------------------

    toggle() {
      this.playing = !this.playing;
      this.dom.playIcon.innerHTML = this.playing ? "&#10074;&#10074;" : "&#9654;";
      this.dom.play.setAttribute("aria-label", this.playing ? "Pause" : "Play");
      if (this.playing) {
        this.lastFrame = performance.now();
        this._tick();
      }
    }

    _tick() {
      if (!this.playing) return;
      const now = performance.now();
      const dt = (now - this.lastFrame) / 1000;
      this.lastFrame = now;

      this.time += dt * this.speed * 60;
      if (this.time > MINUTES_PER_DAY) this.time -= MINUTES_PER_DAY;

      this.dom.scrub.value = String(Math.round(this.time));
      this._syncClock();
      this._render();
      requestAnimationFrame(() => this._tick());
    }

    setMode(mode) {
      if (mode === this.mode) return;
      this.mode = mode;
      this.el.querySelectorAll("[data-mode]").forEach((b) =>
        b.classList.toggle("on", b.dataset.mode === mode)
      );
      this._syncBadge();
      this._syncLegend();
      // Only the layer set changes -- the map itself is untouched, so there is
      // no camera reset or basemap reload when switching modes.
      this._render();
    }

    _syncClock() {
      const m = Math.floor(this.time) % MINUTES_PER_DAY;
      const hh = String(Math.floor(m / 60)).padStart(2, "0");
      const mm = String(m % 60).padStart(2, "0");
      this.dom.clock.textContent = `${hh}:${mm}`;
    }

    // -- layers -------------------------------------------------------------

    _bin() {
      return Math.min(95, Math.max(0, Math.floor(this.time / 15)));
    }

    _layers() {
      if (!this.payload) return [];
      if (this.mode === "measured") return this._measuredLayers();
      return this._simulatedLayers();
    }

    _measuredLayers() {
      const bin = this._bin();
      const peak = Math.max(1, this.payload.measured.peak);
      const data = this.payload.measured.counters;

      return [
        // A dim ring for every counter, so offline or quiet sites stay visible
        // rather than disappearing and looking like missing coverage.
        new deck.ScatterplotLayer({
          id: "counter-base",
          data,
          getPosition: (d) => [d.lon, d.lat],
          getRadius: 90,
          radiusUnits: "meters",
          stroked: true,
          filled: false,
          getLineColor: (d) => (d.oos ? [120, 70, 70, 140] : [110, 120, 135, 110]),
          lineWidthMinPixels: 1,
        }),
        new deck.ScatterplotLayer({
          id: "counter-live",
          data,
          getPosition: (d) => [d.lon, d.lat],
          getRadius: (d) => 90 + 520 * Math.sqrt((d.cyc[bin] || 0) / peak),
          radiusUnits: "meters",
          getFillColor: (d) => {
            const v = (d.cyc[bin] || 0) / peak;
            const c = rampColor(Math.sqrt(v));
            return [c[0], c[1], c[2], v > 0 ? 210 : 30];
          },
          pickable: true,
          updateTriggers: {
            getRadius: bin,
            getFillColor: bin,
          },
          onHover: (info) => this._hover(info, bin),
        }),
      ];
    }

    _simulatedLayers() {
      if (!this.trips) return this._measuredLayers();
      return [
        new deck.TripsLayer({
          id: "trips",
          data: this.trips,
          getPath: (d) => d.path,
          getTimestamps: (d) => d.timestamps,
          getColor: [96, 205, 255],
          opacity: 0.85,
          widthMinPixels: 2.4,
          jointRounded: true,
          capRounded: true,
          fadeTrail: true,
          trailLength: TRAIL_MINUTES,
          currentTime: this.time,
        }),
        new deck.ScatterplotLayer({
          id: "counter-ghost",
          data: this.payload.measured.counters,
          getPosition: (d) => [d.lon, d.lat],
          getRadius: 70,
          radiusUnits: "meters",
          stroked: true,
          filled: false,
          getLineColor: [150, 165, 185, 90],
          lineWidthMinPixels: 1,
        }),
      ];
    }

    _hover(info, bin) {
      const tip = this.dom.tooltip;
      if (!info || !info.object) {
        tip.hidden = true;
        return;
      }
      const d = info.object;
      const now = d.cyc[bin] || 0;
      tip.hidden = false;
      tip.style.left = `${info.x + 14}px`;
      tip.style.top = `${info.y + 14}px`;
      tip.innerHTML =
        `<strong>${d.name}</strong>` +
        `<span>${now} bikes this 15 min</span>` +
        `<span>${d.total.toLocaleString()} over the day</span>` +
        (d.oos ? `<span class="dm-warn">counter flagged out of service</span>` : "");
    }

    _syncBadge() {
      if (!this.payload) return;
      const s = this.payload.simulated;
      if (this.mode === "simulated") {
        this.dom.badge.innerHTML =
          `<span class="dm-dot"></span>Simulated &mdash; real counts, synthetic paths`;
        this.dom.badge.classList.add("is-sim");
      } else {
        this.dom.badge.innerHTML = `<span class="dm-dot"></span>Measured &mdash; counter readings only`;
        this.dom.badge.classList.remove("is-sim");
      }
    }

    _syncLegend() {
      if (!this.payload) return;
      const m = this.payload.measured;
      const s = this.payload.simulated;
      if (this.mode === "simulated") {
        if (!this.trips) {
          this.dom.legend.innerHTML =
            `<div class="dm-legend-note">No route library found. Run
             <code>Rscript scripts/build_paths.R</code> to enable this mode.</div>`;
          return;
        }
        this.dom.legend.innerHTML =
          `<div class="dm-legend-title">${s.total.toLocaleString()} cyclists counted</div>
           <div class="dm-legend-note">${s.shown.toLocaleString()} riders drawn &middot;
             1 shown = ${s.scale} counted</div>`;
      } else {
        const total = m.counters.reduce((a, c) => a + c.total, 0);
        const swatches = [0, 0.25, 0.5, 0.75, 1]
          .map((t) => {
            const c = rampColor(t);
            return `<i style="background:rgb(${c[0]},${c[1]},${c[2]})"></i>`;
          })
          .join("");
        this.dom.legend.innerHTML =
          `<div class="dm-legend-title">${total.toLocaleString()} cyclists counted</div>
           <div class="dm-legend-scale">${swatches}</div>
           <div class="dm-legend-note">0 to ${m.peak} bikes per 15 min</div>`;
      }
    }

    _render() {
      if (!this.overlay) return;
      this.overlay.setProps({ layers: this._layers() });
    }

    resize() {
      if (this.map) this.map.resize();
    }
  }

  // -- Shiny binding --------------------------------------------------------

  const binding = new Shiny.OutputBinding();

  $.extend(binding, {
    find: function (scope) {
      return $(scope).find(".deckmap-output");
    },
    renderValue: function (el, data) {
      if (!el._deckmap) el._deckmap = new DeckMap(el);
      if (data) el._deckmap.setPayload(data);
    },
    resize: function (el) {
      if (el._deckmap) el._deckmap.resize();
    },
  });

  Shiny.outputBindings.register(binding, "yegbike.deckmap");
})();
