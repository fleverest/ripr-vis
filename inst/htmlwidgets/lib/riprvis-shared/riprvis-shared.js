/* Shared drawing code for the riprvis widgets. Ported from the OJS cells of
 * search-slides.qmd; the Observable-runtime idioms (viewof controls, the
 * `invalidation` promise, `Promises.delay` play loops) are replaced with plain
 * DOM controls and setInterval loops that each widget tears down explicitly.
 *
 * Loaded as a plain script after d3 (htmlwidgets guarantees the order), so
 * `window.d3` is available and everything here hangs off `window.RiprVis`. */
window.RiprVis = (function () {
  "use strict";
  var d3 = window.d3;

  // --- shared geometry ------------------------------------------------------
  // e_1 at the apex. Pixel coordinates in the fixed 500x456 viewBox every
  // ternary panel uses; the SVG scales to its container, so these never need
  // to know the rendered size.
  var CORNERS = [[250, 32], [22, 426], [478, 426]];

  function toXY(b) {
    return [
      b[0] * CORNERS[0][0] + b[1] * CORNERS[1][0] + b[2] * CORNERS[2][0],
      b[0] * CORNERS[0][1] + b[1] * CORNERS[1][1] + b[2] * CORNERS[2][1]
    ];
  }

  function toBary(x, y) {
    var A = CORNERS[0], B = CORNERS[1], C = CORNERS[2];
    var det = (B[1] - C[1]) * (A[0] - C[0]) + (C[0] - B[0]) * (A[1] - C[1]);
    var b0 = ((B[1] - C[1]) * (x - C[0]) + (C[0] - B[0]) * (y - C[1])) / det;
    var b1 = ((C[1] - A[1]) * (x - C[0]) + (A[0] - C[0]) * (y - C[1])) / det;
    return [b0, b1, 1 - b0 - b1];
  }

  function poly(V) {
    return "M" + V.map(function (v) {
      return toXY(v).map(function (z) { return z.toFixed(2); }).join(",");
    }).join("L") + "Z";
  }
  var SIMPLEX = poly([[1, 0, 0], [0, 1, 0], [0, 0, 1]]);

  var CLAY = "#b8452f";
  var INK = "#1f1d1a";
  var MUTED = "#7b746a";
  var COOL = "#2c6e8f";
  var SERIF = "'Iowan Old Style', Palatino, Georgia, serif";
  var MONO = "ui-monospace, Menlo, monospace";

  // theta_i = 1 at the corner opposite the i-th edge.
  function vertexLabels(g, k) {
    k = k || 1;
    var at = [
      { p: [1, 0, 0], dx: 0, dy: -13 },
      { p: [0, 1, 0], dx: -14, dy: 17 },
      { p: [0, 0, 1], dx: 14, dy: 17 }
    ];
    g.append("g").selectAll("text").data(at).join("text")
      .attr("x", function (d) { return toXY(d.p)[0] + d.dx / k; })
      .attr("y", function (d) { return toXY(d.p)[1] + d.dy / k; })
      .attr("text-anchor", "middle")
      .attr("font-size", 15 / k).attr("fill", INK)
      .attr("font-family", SERIF)
      .attr("font-style", "italic")
      .html(function (d, i) {
        return "θ<tspan font-size=\"" + (10.5 / k) + "\" dy=\"" +
          (3 / k) + "\">" + (i + 1) + "</tspan>";
      });
  }

  // Area proportional to weight, so r goes as sqrt(w) with a floor: a
  // concentrated mixture would otherwise draw its small atoms at a size
  // indistinguishable from absent, the more misleading of the two failures.
  function atomRadius(w, wmax, max, min) {
    if (max === undefined) max = 4.4;
    if (min === undefined) min = 1.2;
    return Math.max(min, max * Math.sqrt(w / wmax));
  }

  // Shared restyling of d3 axes to the deck's muted look.
  function styleAxes(svg) {
    svg.selectAll(".domain,.tick line").attr("stroke", "#c2bbae");
    svg.selectAll(".tick text").attr("fill", MUTED).attr("font-size", 9)
      .attr("font-family", MONO);
  }

  // --- the Bernstein field --------------------------------------------------
  // G_i is a polynomial whose Bernstein coefficients are the ratios
  // Q(y)/P_i(y), so the basis is built once per widget and every iterate is
  // then a matrix-vector product against its ratio vector.
  function buildGrid(lattice) {
    var nx = 112, ny = 98;
    // Padded past the simplex: G is a polynomial and is perfectly well
    // defined outside it, so the field is evaluated everywhere and only the
    // *drawing* is clipped. Filling the outside with a sentinel instead would
    // put a false contour along every edge where G > 1 -- a closed loop
    // hugging the boundary where the true level set is an open arc
    // terminating on it.
    var pad = 0.06;
    var w = CORNERS[2][0] - CORNERS[1][0], h = CORNERS[1][1] - CORNERS[0][1];
    var x0 = CORNERS[1][0] - pad * w, x1 = CORNERS[2][0] + pad * w;
    var y0 = CORNERS[0][1] - pad * h, y1 = CORNERS[1][1] + pad * h;
    var dx = (x1 - x0) / (nx - 1), dy = (y1 - y0) / (ny - 1);
    var Y = lattice.outcomes, lc = lattice.log_choose;
    var m = Y.length, n = lattice.n;
    var C = lc.map(Math.exp);

    var npts = nx * ny;
    var bary = new Float64Array(npts * 3);
    var inside = new Uint8Array(npts);
    var basis = new Float64Array(npts * m);
    var p0 = new Float64Array(n + 1), p1 = new Float64Array(n + 1),
      p2 = new Float64Array(n + 1);

    for (var j = 0; j < ny; j++) {
      for (var i = 0; i < nx; i++) {
        var k = j * nx + i;
        var b = toBary(x0 + i * dx, y0 + j * dy);
        bary[k * 3] = b[0]; bary[k * 3 + 1] = b[1]; bary[k * 3 + 2] = b[2];
        inside[k] = b[0] > 1e-9 && b[1] > 1e-9 && b[2] > 1e-9 ? 1 : 0;
        // powers rather than exp(log): outside the simplex a coordinate is
        // negative and its logarithm is not available, but the monomial is.
        p0[0] = p1[0] = p2[0] = 1;
        for (var e = 1; e <= n; e++) {
          p0[e] = p0[e - 1] * b[0];
          p1[e] = p1[e - 1] * b[1];
          p2[e] = p2[e - 1] * b[2];
        }
        for (var c = 0; c < m; c++) {
          basis[k * m + c] = C[c] * p0[Y[c][0]] * p1[Y[c][1]] * p2[Y[c][2]];
        }
      }
    }

    var grid = { nx: nx, ny: ny, x0: x0, y0: y0, dx: dx, dy: dy,
      npts: npts, bary: bary, inside: inside, basis: basis, m: m };
    // contour coordinates arrive in grid-index units
    grid.path = d3.geoPath(d3.geoTransform({
      point: function (x, y) {
        this.stream.point(grid.x0 + x * grid.dx, grid.y0 + y * grid.dy);
      }
    }));
    return grid;
  }

  function evalField(grid, ratio) {
    var npts = grid.npts, basis = grid.basis, m = grid.m;
    var z = new Float64Array(npts);
    for (var k = 0; k < npts; k++) {
      var g = 0;
      for (var c = 0; c < m; c++) g += basis[k * m + c] * ratio[c];
      // Far outside the simplex the polynomial goes negative; -99 there is
      // below every threshold and the whole region is clipped away anyway.
      z[k] = g > 0 ? Math.log10(g) : -99;
    }
    return z;
  }

  // Diverging about G = 1, the only value here that means anything: above it
  // the oracle has somewhere to go, below it that direction is spent.
  var fieldColour = d3.scaleLinear()
    .domain([-1.2, -0.4, 0, 0.4, 1.2])
    .range(["#e7edf1", "#cbdae3", "#f7f4ec", "#dd9b83", CLAY])
    .interpolate(d3.interpolateLab).clamp(true);

  // --- controls -------------------------------------------------------------
  // Hand-rolled rather than a widget library so the whole package stays two
  // script files. Each returns {el, value (get/set), ...} and calls onInput
  // on user interaction only -- programmatic set() stays silent, so the play
  // loop can drive a slider without re-entering itself.
  function el(tag, className, text) {
    var node = document.createElement(tag);
    if (className) node.className = className;
    if (text !== undefined) node.textContent = text;
    return node;
  }

  function rangeInput(max, label, onInput) {
    var input = el("input");
    input.type = "range";
    input.min = "0";
    input.max = String(max);
    input.step = "1";
    input.value = "0";
    var out = el("span", "riprvis-val", "0");
    var wrap = el("label", "riprvis-ctl");
    wrap.append(el("span", null, label), input, out);
    input.oninput = function () {
      out.textContent = input.value;
      onInput(+input.value);
    };
    return {
      el: wrap,
      get value() { return +input.value; },
      set value(v) { input.value = v; out.textContent = String(v); },
      setMax: function (m) { input.max = String(m); }
    };
  }

  function tabsInput(labels, onInput) {
    var many = labels.length > 1;
    var wrap = el("div", "riprvis-tabs" + (many ? "" : " riprvis-single"));
    var value = 0;
    var btns = labels.map(function (t, i) {
      var b = el("button", "riprvis-tab", t);
      b.type = "button";
      if (i === 0) b.classList.add("on");
      if (many) {
        b.onclick = function () {
          btns.forEach(function (o) { o.classList.remove("on"); });
          b.classList.add("on");
          value = i;
          onInput(i);
        };
      }
      wrap.append(b);
      return b;
    });
    return {
      el: wrap,
      get value() { return value; }
    };
  }

  function toggleInput(label, onInput) {
    var input = el("input");
    input.type = "checkbox";
    var wrap = el("label", "riprvis-ctl");
    wrap.append(input, el("span", null, label));
    input.onchange = function () { onInput(input.checked); };
    return {
      el: wrap,
      get value() { return input.checked; },
      set value(v) { input.checked = v; }
    };
  }

  // The play loops of the deck were async while-loops killed through
  // Observable's `invalidation`; outside Observable a setInterval with an
  // explicit stop() is the same thing without the footgun -- each widget
  // calls stop() at the top of renderValue, so a re-render (or a Shiny
  // update) can never leave a stale loop accelerating the animation.
  function makePlayLoop(intervalMs, tick) {
    var id = null;
    return {
      start: function () { if (id === null) id = setInterval(tick, intervalMs); },
      stop: function () { if (id !== null) { clearInterval(id); id = null; } },
      get running() { return id !== null; }
    };
  }

  // --- layout helpers -------------------------------------------------------
  // Each widget owns one container div and builds the same layout the deck's
  // `.stage` grid gave it: chart column left, panel column right, control row
  // under both. Returns the named regions.
  function stageLayout(container) {
    container.classList.add("riprvis");
    var stage = el("div", "riprvis-stage");
    var left = el("div", "riprvis-left");
    var right = el("div", "riprvis-right");
    stage.append(left, right);
    var knobs = el("div", "riprvis-knobs");
    container.append(stage, knobs);
    return { stage: stage, left: left, right: right, knobs: knobs };
  }

  function readout(html) {
    var node = el("div", "riprvis-readout");
    node.innerHTML = html;
    return node;
  }

  function dim(text) {
    return "<span class=\"riprvis-dim\">" + text + "</span>";
  }

  return {
    CORNERS: CORNERS, toXY: toXY, toBary: toBary, poly: poly,
    SIMPLEX: SIMPLEX,
    CLAY: CLAY, INK: INK, MUTED: MUTED, COOL: COOL,
    SERIF: SERIF, MONO: MONO,
    vertexLabels: vertexLabels, atomRadius: atomRadius, styleAxes: styleAxes,
    buildGrid: buildGrid, evalField: evalField, fieldColour: fieldColour,
    el: el, rangeInput: rangeInput, tabsInput: tabsInput,
    toggleInput: toggleInput, makePlayLoop: makePlayLoop,
    stageLayout: stageLayout, readout: readout, dim: dim
  };
})();
