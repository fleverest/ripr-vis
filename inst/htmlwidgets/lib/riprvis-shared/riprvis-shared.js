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

  // --- fit diagnostics ------------------------------------------------------
  // Family-agnostic: KL is what the fit minimises; the gap certifies how far
  // it still is from the minimum; the difference KL - log(1 + gap) is the
  // log-growth rate the resulting e-variable is guaranteed. Together they
  // show the third rising as the second falls, which is the whole reason for
  // running the fit to convergence rather than stopping when KL flattens.
  function fitPanels(fit, nFit, i) {
    var W = 420, H = 108, gap = 20, m = { l: 58, r: 10, t: 14, b: 20 };
    var svg = d3.create("svg")
      .attr("viewBox", "0 0 " + W + " " + (3 * H + 2 * gap))
      .attr("width", "100%").style("height", "auto")
      .style("overflow", "visible");
    var x = d3.scaleLinear([1, nFit], [m.l, W - m.r]);

    var series = [
      {
        label: "KL(Q ‖ P)", log: false, colour: COOL,
        v: fit.kl.map(function (k, j) { return { t: j + 1, y: k }; })
      },
      {
        label: "gap", log: true, colour: CLAY,
        v: fit.gap.map(function (g, j) { return { t: j + 1, y: g }; })
          .filter(function (d) { return d.y != null && d.y > 0; })
      },
      {
        label: "log-growth  KL − log(1 + gap)", log: false, colour: INK,
        v: fit.kl.map(function (k, j) {
          return {
            t: j + 1,
            y: fit.gap[j] == null ? null : k - Math.log1p(fit.gap[j])
          };
        }).filter(function (d) { return d.y != null; })
      }
    ];

    series.forEach(function (s, r) {
      var g = svg.append("g")
        .attr("transform", "translate(0," + (r * (H + gap)) + ")");
      var ext = d3.extent(s.v, function (d) { return d.y; });
      var y = s.log
        ? d3.scaleLog(ext, [H - m.b, m.t])
        : d3.scaleLinear([Math.min(ext[0], 0), ext[1]], [H - m.b, m.t]).nice();

      g.append("g").attr("transform", "translate(0," + (H - m.b) + ")")
        .call(d3.axisBottom(x).ticks(5).tickSize(3));
      g.append("g").attr("transform", "translate(" + m.l + ",0)")
        .call(s.log
          ? d3.axisLeft(y).ticks(3, "0.0e").tickSize(3)
          : d3.axisLeft(y).ticks(3).tickSize(3).tickFormat(d3.format(".3f")));

      g.append("path").datum(s.v).attr("fill", "none")
        .attr("stroke", s.colour).attr("stroke-width", 1.4)
        .attr("d", d3.line()
          .x(function (d) { return x(d.t); })
          .y(function (d) { return y(d.y); }));

      var here = s.v.filter(function (d) {
        return d.t <= Math.max(i, 1);
      }).slice(-1)[0];
      if (here) {
        g.append("line").attr("x1", x(here.t)).attr("x2", x(here.t))
          .attr("y1", m.t).attr("y2", H - m.b)
          .attr("stroke", MUTED).attr("stroke-dasharray", "2 3");
        g.append("circle").attr("r", 3).attr("fill", s.colour)
          .attr("cx", x(here.t)).attr("cy", y(here.y));
      }
      g.append("text").attr("x", m.l).attr("y", 9)
        .attr("font-size", 11).attr("fill", MUTED).text(s.label);
    });

    styleAxes(svg);
    return svg.node();
  }

  function fitReadout(fit, i) {
    var j = Math.max(i, 1) - 1;
    var kl = fit.kl[j], gap = fit.gap[j];
    var f = d3.format(".5f");
    var html = fit.phase[j] +
      " " + dim("KL") + " " + f(kl) +
      " " + dim("gap") + " " +
      (gap == null ? "—" : d3.format(".2e")(gap)) +
      " " + dim("growth") + " " +
      (gap == null ? "—" : f(kl - Math.log1p(gap)));
    return readout(html);
  }

  // --- planar (R^2) geometry -------------------------------------------------
  // The planar widgets have no canonical frame the way the ternary ones do
  // (CORNERS is fixed once and for all) -- every example fixes its own
  // extent, so the pixel mapping is built per widget from that extent
  // instead of being a module-level constant.
  var PLANE_VIEWBOX = { w: 500, h: 456 };
  var PLANE_MARGIN = { l: 46, r: 12, t: 12, b: 34 };

  // Isotropic data -> pixel map into the fixed viewBox's plot rect: one
  // scale for both axes, so a circle stays a circle and a right angle stays
  // one, with the shorter-spanning axis left centred with slack either side.
  function planeView(extent) {
    var m = PLANE_MARGIN;
    var rect = {
      x0: m.l, y0: m.t,
      x1: PLANE_VIEWBOX.w - m.r, y1: PLANE_VIEWBOX.h - m.b
    };
    var plotW = rect.x1 - rect.x0, plotH = rect.y1 - rect.y0;
    var xr = extent.x, yr = extent.y;
    var xspan = xr[1] - xr[0], yspan = yr[1] - yr[0];
    var k = Math.min(plotW / xspan, plotH / yspan);
    var cx = (xr[0] + xr[1]) / 2, cy = (yr[0] + yr[1]) / 2;
    var pcx = (rect.x0 + rect.x1) / 2, pcy = (rect.y0 + rect.y1) / 2;
    function sx(x) { return pcx + (x - cx) * k; }
    // y is up in data space, down in pixel space.
    function sy(y) { return pcy - (y - cy) * k; }
    return { sx: sx, sy: sy, k: k, rect: rect, extent: extent };
  }

  // SVG path for one (possibly unbounded) convex part, given in
  // vertex/ray/lineality generators: a ray or lineality direction is walked
  // out by L data units -- far past any sane viewport -- before the hull is
  // taken, so the drawn edge always exits through the plot rect's border
  // rather than stopping short of it. Callers clip to view.rect.
  function polyhedronPath(seed, view) {
    var plotW = view.rect.x1 - view.rect.x0;
    var plotH = view.rect.y1 - view.rect.y0;
    var L = 50 * Math.max(plotW, plotH) / view.k;
    var pts = [];
    seed.v.forEach(function (v) {
      pts.push([view.sx(v[0]), view.sy(v[1])]);
      (seed.r || []).forEach(function (r) {
        pts.push([view.sx(v[0] + L * r[0]), view.sy(v[1] + L * r[1])]);
      });
      (seed.l || []).forEach(function (l) {
        pts.push([view.sx(v[0] + L * l[0]), view.sy(v[1] + L * l[1])]);
        pts.push([view.sx(v[0] - L * l[0]), view.sy(v[1] - L * l[1])]);
      });
    });
    var hull = d3.polygonHull(pts);
    if (!hull) {
      // Fewer than three distinct points (a single vertex, or a part
      // degenerate to a segment): draw what there is rather than nothing.
      if (pts.length === 0) return "";
      return "M" + pts.map(function (p) { return p.join(","); }).join("L") +
        (pts.length > 2 ? "Z" : "");
    }
    return "M" + hull.map(function (p) { return p.join(","); }).join("L") +
      "Z";
  }

  // Bottom/left axes on the plot rect's own edges, plus faint zero lines and
  // the same italic-serif theta labels the simplex uses at its corners.
  function planeAxes(svg, view) {
    var r = view.rect, ex = view.extent;
    // The isotropic mapping centres the extent in the plot rect, so the data
    // interval the rect's full width/height covers is wider than the extent
    // along one axis; the tick scales must invert sx/sy exactly or the ticks
    // drift off the data by the aspect mismatch.
    var cx = (ex.x[0] + ex.x[1]) / 2, cy = (ex.y[0] + ex.y[1]) / 2;
    var pcx = (r.x0 + r.x1) / 2, pcy = (r.y0 + r.y1) / 2;
    var xScale = d3.scaleLinear(
      [cx + (r.x0 - pcx) / view.k, cx + (r.x1 - pcx) / view.k],
      [r.x0, r.x1]
    );
    var yScale = d3.scaleLinear(
      [cy - (r.y1 - pcy) / view.k, cy - (r.y0 - pcy) / view.k],
      [r.y1, r.y0]
    );

    svg.append("g").attr("transform", "translate(0," + r.y1 + ")")
      .call(d3.axisBottom(xScale).ticks(5).tickSize(3));
    svg.append("g").attr("transform", "translate(" + r.x0 + ",0)")
      .call(d3.axisLeft(yScale).ticks(5).tickSize(3));

    if (ex.x[0] < 0 && ex.x[1] > 0) {
      svg.append("line")
        .attr("x1", view.sx(0)).attr("x2", view.sx(0))
        .attr("y1", r.y0).attr("y2", r.y1)
        .attr("stroke", "#ddd7cb");
    }
    if (ex.y[0] < 0 && ex.y[1] > 0) {
      svg.append("line")
        .attr("x1", r.x0).attr("x2", r.x1)
        .attr("y1", view.sy(0)).attr("y2", view.sy(0))
        .attr("stroke", "#ddd7cb");
    }

    var labelStyle = function (t) {
      return t.attr("font-size", 13).attr("fill", INK)
        .attr("font-family", SERIF).attr("font-style", "italic");
    };
    labelStyle(svg.append("text")
      .attr("x", r.x1).attr("y", r.y1 - 8).attr("text-anchor", "end"))
      .html("θ<tspan font-size=\"9\" dy=\"2\">1</tspan>");
    labelStyle(svg.append("text")
      .attr("x", r.x0 + 8).attr("y", r.y0 + 14).attr("text-anchor", "start"))
      .html("θ<tspan font-size=\"9\" dy=\"2\">2</tspan>");

    styleAxes(svg);
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
    planeView: planeView, polyhedronPath: polyhedronPath,
    planeAxes: planeAxes,
    el: el, rangeInput: rangeInput, tabsInput: tabsInput,
    toggleInput: toggleInput, makePlayLoop: makePlayLoop,
    stageLayout: stageLayout, readout: readout, dim: dim,
    fitPanels: fitPanels, fitReadout: fitReadout
  };
})();
