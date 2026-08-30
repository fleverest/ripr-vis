/* Certifying the fit: the branch-and-bound search. Each cell (a triangulated
 * piece of a declared part of the null; an already-simplicial part is its own
 * single cell) is searched independently, so a tab strip picks which cell's
 * search the rest of the panel follows. Ported from aliveAt()/settled()/
 * drawSimplex()/drawWindows()/readout()/drawTree() in search-slides.qmd. */
HTMLWidgets.widget({
  name: "ripr_certify",
  type: "output",

  factory: function (el, width, height) {
    var V = window.RiprVis, d3 = window.d3;

    // Reshape the raw payload's parallel node arrays into row objects and add
    // the per-cell lookups every panel needs, mirroring the deck's prepare().
    function prepare(raw) {
      var ex = Object.assign({}, raw);
      var N = ex.nodes;
      ex.node = N.id.map(function (_, i) {
        return {
          part: N.part[i], cell: N.cell[i], id: N.id[i], parent: N.parent[i],
          depth: N.depth[i], born: N.born[i], retired: N.retired[i],
          fate: N.fate[i], upper: N.upper[i], volume: N.volume[i],
          V: N.vertices[i]
        };
      });
      ex.cellIds = Array.from(new Set(ex.node.map(function (d) { return d.cell; })))
        .sort(d3.ascending);
      ex.byId = new Map(ex.node.map(function (d) {
        return [d.cell + ":" + d.id, d];
      }));
      // Each cell is searched independently, so its own run length -- not
      // the certificate's overall iteration count -- is what its panels and
      // its slice of the play loop should use.
      ex.Tof = function (i) { return ex.upper[i].length; };
      ex.T = d3.max(ex.cellIds.map(function (_, i) { return ex.Tof(i); }));
      // A trace shorter than T means that cell converged first; hold its
      // last value rather than letting the window vanish from the panel.
      ex.win = function (i, t) {
        var lo = ex.lower[i], hi = ex.upper[i];
        if (t < 1) return null;
        var j = Math.min(t, lo.length) - 1;
        return { lo: lo[j], hi: hi[j] };
      };
      return ex;
    }

    // Every node born by t that has not been split away by t. These tile the
    // cell exactly at every t, so a frame is a genuine partition and not a
    // sample of one.
    function aliveAt(ex, t, cell) {
      return ex.node.filter(function (d) {
        return (cell == null || d.cell === cell) && d.born <= t &&
          !(d.fate === "split" && d.retired != null && d.retired <= t);
      });
    }

    function settled(d, t) {
      return d.fate === "pruned" && d.retired != null && d.retired <= t;
    }

    function drawSimplex(ex, t, ci, opts) {
      var zoom = !!(opts && opts.zoom);
      var svg = d3.create("svg").attr("viewBox", "0 0 500 456")
        .attr("width", "100%").style("height", "auto")
        .style("overflow", "visible");

      // Light = low bound = little left to gain; dark = high = still worth
      // splitting. Fixed across the run so frames compare.
      var fill = d3.scaleLinear()
        .domain(d3.extent(ex.node, function (d) { return d.upper; }))
        .range(["#f6f2e8", V.CLAY]).interpolate(d3.interpolateLab).clamp(true);

      var cellId = ex.cellIds[ci];
      var cell = ex.cells[ci];
      var partIdx = cell.part - 1;
      var k = 1;
      var g = svg.append("g");

      // Cells halve in volume at every split, so a fixed view is empty after
      // a handful of iterations. Following the live cells is the only way
      // the later frames carry anything; scale then stops being comparable,
      // so it is reported via the "follow live cells" toggle itself.
      if (zoom) {
        var live = aliveAt(ex, t, cellId).filter(function (d) {
          return !settled(d, t);
        });
        if (live.length) {
          var pts = [];
          live.forEach(function (d) {
            d.V.forEach(function (v) { pts.push(V.toXY(v)); });
          });
          var x0 = d3.min(pts, function (p) { return p[0]; });
          var x1 = d3.max(pts, function (p) { return p[0]; });
          var y0 = d3.min(pts, function (p) { return p[1]; });
          var y1 = d3.max(pts, function (p) { return p[1]; });
          k = 430 / (Math.max(x1 - x0, y1 - y0, 1e-9) * 1.7);
          g.attr("transform",
            "translate(" + (250 - k * (x0 + x1) / 2) + "," +
            (229 - k * (y0 + y1) / 2) + ") scale(" + k + ")");
        }
      }

      g.append("path").attr("d", V.SIMPLEX)
        .attr("fill", "none").attr("stroke", "#cdc6b8")
        .attr("stroke-width", 1 / k);
      // The other parts, faintly, so it is clear this is one cell of several
      // searches and not the whole null. A part with several cells still
      // reads right: only the active cell's own part is left off.
      g.append("g").selectAll("path")
        .data(ex.seeds.filter(function (d, i) { return i !== partIdx; }))
        .join("path").attr("d", V.poly).attr("fill", "none")
        .attr("stroke", "#bfb8aa").attr("stroke-width", 1 / k)
        .attr("stroke-dasharray", (4 / k) + " " + (3 / k));
      g.append("path").attr("d", V.poly(cell.vertices))
        .attr("fill", "#fdfcf9").attr("stroke", "none");

      // Only the selected cell. Cells are searched independently, so drawing
      // several at once shows more than one bisection at iteration 1 and
      // leaves another search's cell edges crossing this one.
      g.append("g").selectAll("path").data(aliveAt(ex, t, cellId)).join("path")
        .attr("d", function (d) { return V.poly(d.V); })
        // A pruned cell is proven not to hold the maximum. Grey says the
        // region is settled, which is the content of a prune; dropping it
        // would leave a hole and hide how much was ruled out.
        .attr("fill", function (d) {
          return settled(d, t) ? "#e9e5db" : fill(d.upper);
        })
        .attr("stroke", function (d) {
          return d.born === t ? V.INK : settled(d, t) ? "#dcd6ca" : "#948d80";
        })
        .attr("stroke-width", function (d) {
          return (d.born === t ? 1.8 : settled(d, t) ? 0.35 : 0.5) / k;
        });

      var wmax = d3.max(ex.marks.weights);
      g.append("g").selectAll("circle").data(ex.marks.q).join("circle")
        .attr("cx", function (p) { return V.toXY(p)[0]; })
        .attr("cy", function (p) { return V.toXY(p)[1]; })
        .attr("r", function (p, i) {
          return V.atomRadius(ex.marks.weights[i], wmax, 3.4, 2) / k;
        })
        .attr("fill", V.INK);

      V.vertexLabels(g, k);
      return svg.node();
    }

    // One panel per cell. The band is [incumbent, bound] at every iteration:
    // the interval the search is entitled to claim. Bisection drives the two
    // ends together, and the panel is that closing, with the window at t
    // picked out.
    function drawWindows(ex, t, ci) {
      var lo = ex.lower[ci], hi = ex.upper[ci];
      var n = lo.length;
      // A band panel plus a thin log strip. In value terms the window shuts
      // within a few iterations and the band looks empty thereafter; on a
      // log axis the remaining iterations are visibly still working, which
      // is the honest reading of the same two numbers.
      var H = 150, strip = 46, W = 420, m = { l: 56, r: 10, t: 14, b: 20 };
      var svg = d3.create("svg")
        .attr("viewBox", "0 0 " + W + " " + (H + strip + 10))
        .attr("width", "100%").style("height", "auto")
        .style("overflow", "visible");
      var g = svg.append("g");
      var x = d3.scaleLinear([1, ex.Tof(ci)], [m.l, W - m.r]);
      var ext = [d3.min(lo), d3.max(hi)];
      var pad = (ext[1] - ext[0]) * 0.08;
      var y = d3.scaleLinear([ext[0] - pad, ext[1] + pad], [H - m.b, m.t]);
      var data = d3.range(n).map(function (j) {
        return { t: j + 1, lo: lo[j], hi: hi[j] };
      });

      g.append("g").attr("transform", "translate(0," + (H - m.b) + ")")
        .call(d3.axisBottom(x).ticks(6).tickSize(3));
      g.append("g").attr("transform", "translate(" + m.l + ",0)")
        .call(d3.axisLeft(y).ticks(4).tickSize(3).tickFormat(d3.format(".3f")));

      // 1 is where a valid e-variable's bound has to land; drawing it says
      // how little room the search is actually working in.
      if (y.domain()[0] <= 1 && y.domain()[1] >= 1) {
        g.append("line").attr("x1", m.l).attr("x2", W - m.r)
          .attr("y1", y(1)).attr("y2", y(1))
          .attr("stroke", V.MUTED).attr("stroke-dasharray", "2 3");
      }

      g.append("path").datum(data).attr("fill", V.CLAY).attr("fill-opacity", 0.16)
        .attr("d", d3.area().curve(d3.curveStepAfter)
          .x(function (d) { return x(d.t); })
          .y0(function (d) { return y(d.lo); })
          .y1(function (d) { return y(d.hi); }));
      [["hi", V.CLAY], ["lo", V.INK]].forEach(function (pair) {
        var f = pair[0], col = pair[1];
        g.append("path").datum(data).attr("fill", "none")
          .attr("stroke", col).attr("stroke-width", 1.3)
          .attr("d", d3.line().curve(d3.curveStepAfter)
            .x(function (d) { return x(d.t); })
            .y(function (d) { return y(d[f]); }));
      });

      var w = ex.win(ci, t);
      if (w) {
        var px = x(Math.min(t, n));
        g.append("line").attr("x1", px).attr("x2", px)
          .attr("y1", y(w.lo)).attr("y2", y(w.hi))
          .attr("stroke", V.INK).attr("stroke-width", 2.6);
        [w.lo, w.hi].forEach(function (v) {
          g.append("line").attr("x1", px - 5).attr("x2", px + 5)
            .attr("y1", y(v)).attr("y2", y(v))
            .attr("stroke", V.INK).attr("stroke-width", 2.6);
        });
      }

      // width on a log axis, sharing the x-scale
      var gs = g.append("g").attr("transform", "translate(0," + (H + 10) + ")");
      var wd = data.map(function (d) {
        return { t: d.t, w: Math.max(d.hi - d.lo, 1e-17) };
      });
      var ys = d3.scaleLog(d3.extent(wd, function (d) { return d.w; }),
        [strip - 12, 2]);
      gs.append("g").attr("transform", "translate(" + m.l + ",0)")
        .call(d3.axisLeft(ys).ticks(2, "0.0e").tickSize(2));
      gs.append("path").datum(wd).attr("fill", "none")
        .attr("stroke", V.CLAY).attr("stroke-width", 1.2)
        .attr("d", d3.line().curve(d3.curveStepAfter)
          .x(function (d) { return x(d.t); })
          .y(function (d) { return ys(d.w); }));
      if (w) {
        gs.append("circle").attr("r", 2.8).attr("fill", V.INK)
          .attr("cx", x(Math.min(t, n)))
          .attr("cy", ys(Math.max(wd[Math.min(t, n) - 1].w, 1e-17)));
      }

      g.append("text").attr("x", m.l).attr("y", 9)
        .attr("font-size", 11).attr("fill", V.MUTED)
        .text("enclosure over " + ex.labels.cells[ci]);

      V.styleAxes(svg);
      return svg.node();
    }

    function drawReadout(ex, t, ci) {
      if (t < 1) return V.readout("&nbsp;");
      var w = ex.win(ci, t);
      return V.readout(
        "[" + w.lo.toFixed(7) + ", " + w.hi.toFixed(7) + "] " +
        V.dim("width " + d3.format(".1e")(Math.max(w.hi - w.lo, 0)))
      );
    }

    // Depth against birth iteration, each node joined to its parent. A
    // max-bound queue can walk straight down one branch, which shows here as
    // a diagonal and is invisible in the geometry once the cells are small.
    function drawTree(ex, t, ci) {
      var W = 420, H = 118, m = { l: 56, r: 10, t: 12, b: 22 };
      var svg = d3.create("svg").attr("viewBox", "0 0 " + W + " " + H)
        .attr("width", "100%").style("height", "auto")
        .style("overflow", "visible");
      var cellId = ex.cellIds[ci];
      var mine = ex.node.filter(function (d) { return d.cell === cellId; });
      var x = d3.scaleLinear([0, ex.Tof(ci)], [m.l, W - m.r]);
      var y = d3.scaleLinear(d3.extent(mine, function (d) { return d.depth; }),
        [H - m.b, m.t]);

      svg.append("g").attr("transform", "translate(0," + (H - m.b) + ")")
        .call(d3.axisBottom(x).ticks(5).tickSize(3));
      svg.append("g").attr("transform", "translate(" + m.l + ",0)")
        .call(d3.axisLeft(y).ticks(4).tickSize(3));

      var shown = mine.filter(function (d) { return d.born <= t; });
      svg.append("g").selectAll("line")
        .data(shown.filter(function (d) { return d.parent != null; }))
        .join("line")
        .attr("x1", function (d) {
          return x(ex.byId.get(cellId + ":" + d.parent).born);
        })
        .attr("y1", function (d) {
          return y(ex.byId.get(cellId + ":" + d.parent).depth);
        })
        .attr("x2", function (d) { return x(d.born); })
        .attr("y2", function (d) { return y(d.depth); })
        .attr("stroke", "#ddd7cb").attr("stroke-width", 0.7);
      svg.append("g").selectAll("circle").data(shown).join("circle")
        .attr("cx", function (d) { return x(d.born); })
        .attr("cy", function (d) { return y(d.depth); })
        .attr("r", 2.7)
        .attr("fill", function (d) {
          return d.fate === "split" && d.retired <= t ? "#bbb4a6"
            : settled(d, t) ? "#dcc9b2" : V.CLAY;
        });

      svg.append("text").attr("x", m.l).attr("y", 8)
        .attr("font-size", 11).attr("fill", V.MUTED).text("depth against birth");
      V.styleAxes(svg);
      return svg.node();
    }

    var loop = null, t = 0, ci = 0, zoom = false;

    return {
      renderValue: function (x) {
        // Kill any loop from a previous render before tearing the DOM down,
        // so a stale interval can never survive a Shiny update.
        if (loop) loop.stop();

        var ex = prepare(JSON.parse(x.data));

        el.replaceChildren();
        var regions = V.stageLayout(el);

        var simplexWrap = V.el("div");
        var readoutWrap = V.el("div");
        regions.left.append(simplexWrap, readoutWrap);

        var windowsWrap = V.el("div");
        var treeWrap = V.el("div");
        regions.right.append(windowsWrap, treeWrap);

        t = 0; ci = 0; zoom = false;

        function redraw() {
          simplexWrap.replaceChildren(drawSimplex(ex, t, ci, { zoom: zoom }));
          readoutWrap.replaceChildren(drawReadout(ex, t, ci));
          windowsWrap.replaceChildren(drawWindows(ex, t, ci));
          treeWrap.replaceChildren(drawTree(ex, t, ci));
        }

        // The cells are searched independently, so showing several at once
        // makes iteration 1 look like more than one bisection and mixes
        // more than one set of cell boundaries; one at a time is the honest
        // picture.
        var tabs = V.tabsInput(ex.labels.cells, function (v) {
          ci = v;
          t = 0;
          slider.setMax(ex.Tof(ci));
          slider.value = 0;
          redraw();
        });

        var slider = V.rangeInput(ex.Tof(ci), "iteration", function (v) {
          t = v;
          redraw();
        });

        loop = V.makePlayLoop(300, function () {
          var last = ex.Tof(ci);
          t = t >= last ? 0 : t + 1;
          slider.value = t;
          redraw();
        });

        var play = V.toggleInput("play", function (checked) {
          if (checked) loop.start(); else loop.stop();
        });

        var zoomToggle = V.toggleInput("follow live cells", function (checked) {
          zoom = checked;
          redraw();
        });

        regions.knobs.append(tabs.el, slider.el, play.el, zoomToggle.el);

        redraw();
      },

      resize: function (width, height) {
        // Every SVG here is fixed-aspect via its viewBox and scales with its
        // container; there is nothing to recompute on resize.
      }
    };
  }
});
