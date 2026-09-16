/* Comparing fits: the KL / gap / growth traces of several runs of the same
 * problem drawn over one another, against the step count or -- where the
 * trace carries times -- the clock. The panels are the ones
 * RiprVis.fitPanels draws for a single fit, widened and given one line per
 * run. Colour and dash follow the run's grouping in the payload (or its
 * position, when ungrouped) and never change with the axis or the hover.
 *
 * With many runs the picture is dense, so the widget carries its own
 * declutter controls: every legend key and every run chip is a toggle
 * (click to hide or show, shift-click to keep only it), the x range can be
 * set by dragging on any panel or typed in, and each panel's y range can be
 * typed in too. The y scales always fit what is visible, so narrowing the
 * x window is usually enough to separate the lines; a button keeps only the
 * runs whose growth peaks highest, at any point of their traces. */
HTMLWidgets.widget({
  name: "ripr_compare",
  type: "output",

  factory: function (el, width, height) {
    var V = window.RiprVis, d3 = window.d3;

    // Fixed order, validated for adjacent-pair separation under protan and
    // deutan simulation against the paper surface; a sixth colour level and
    // beyond fall back to grey rather than a generated hue.
    var COLOURS = ["#2a6aa8", "#b8452f", "#2f8f6f", "#8455b0", "#b5820a"];
    var OVERFLOW = "#8d867a";
    // Dash levels in order: dotted, dashed, solid, dash-dot -- so the third
    // level, the one the eye should rest on, is the plain line.
    var DASHES = ["1.5 3", "6 3", null, "8 3 2 3"];
    // The same four by name, for a payload that says which pattern each
    // dash level takes rather than leaving it to the order.
    var PATTERNS = { dotted: "1.5 3", dashed: "6 3", solid: null, "dash-dot": "8 3 2 3" };
    // Beyond this many visible runs the end labels and the per-run readout
    // give way to the legend and a nearest-line hover.
    var FEW = 5;

    var W = 900, H = 120, GAP = 18;
    var M = { l: 64, r: 100, t: 14, b: 22 };
    // Clip-path ids must be unique per widget instance on the page.
    var uid = "riprvis-cmp-" + Math.random().toString(36).slice(2, 8);

    // The pattern of dash level i: the one the payload names for it, else
    // the positional default.
    function dashOf(data, i) {
      var named = data.legend.dashes;
      return named && i < named.length && named[i] in PATTERNS
        ? PATTERNS[named[i]] : DASHES[i % DASHES.length];
    }

    function seriesOf(data) {
      return data.runs.map(function (r, i) {
        var pts = r.step.map(function (s, j) {
          var gap = r.gap[j];
          return {
            step: s,
            time: r.time[j],
            phase: r.phase[j],
            kl: r.kl[j],
            gap: gap == null ? null : gap,
            growth: gap == null ? null : r.kl[j] - Math.log1p(gap)
          };
        });
        var c = r.colour - 1;
        return {
          index: i,
          label: r.label,
          colour: c < COLOURS.length ? COLOURS[c] : OVERFLOW,
          dash: data.legend.dash ? dashOf(data, r.dash - 1) : null,
          // Level indices, 0-based, for the legend toggles; a run of an
          // ungrouped payload belongs to no level.
          colourLevel: data.legend.colour ? r.colour - 1 : null,
          dashLevel: data.legend.dash ? r.dash - 1 : null,
          pts: pts,
          // Where the run's growth peaks, over the points with a gap; the
          // top-few button ranks the runs by it.
          peak: d3.max(pts, function (p) { return p.growth; }),
          timed: pts.some(function (p) { return p.time != null; })
        };
      });
    }

    // The indices of the `n` runs whose growth peaks highest, at any point
    // of their traces; a run with no growth recorded is never among them.
    function topByGrowth(series, n) {
      return series.filter(function (s) { return s.peak != null; })
        .sort(function (a, b) { return b.peak - a.peak; })
        .slice(0, n).map(function (s) { return s.index; });
    }

    var PANELS = [
      { key: "kl", label: "KL(Q ‖ P)", log: false, fmt: d3.format(".5f") },
      { key: "gap", label: "gap", log: true, fmt: d3.format(".2e") },
      {
        key: "growth", label: "growth  KL − log(1 + gap)", log: false,
        fmt: d3.format(".5f")
      }
    ];

    function xOf(p, axis) { return axis === "time" ? p.time : p.step; }

    // Points a run contributes on the chosen axis: those with a value there,
    // and strictly positive ones when the axis is logarithmic.
    function onAxis(run, axis, logX) {
      return run.pts.filter(function (p) {
        var x = xOf(p, axis);
        return x != null && (!logX || x > 0);
      });
    }

    function panelPts(pts, panel) {
      return pts.filter(function (p) {
        var v = p[panel.key];
        return v != null && (!panel.log || v > 0);
      });
    }

    // Is a value inside a [lo, hi] range whose ends may be null (open)?
    function within(v, range) {
      return (range[0] == null || v >= range[0]) &&
        (range[1] == null || v <= range[1]);
    }

    function swatch(colour, dash) {
      var sp = V.el("span", "riprvis-swatch");
      sp.innerHTML = "<svg width=\"22\" height=\"6\" viewBox=\"0 0 22 6\">" +
        "<line x1=\"0\" y1=\"3\" x2=\"22\" y2=\"3\" stroke=\"" + colour +
        "\" stroke-width=\"2\"" +
        (dash ? " stroke-dasharray=\"" + dash + "\"" : "") + "/></svg>";
      return sp;
    }

    // The runs the toggles leave visible: the run's own chip on, and its
    // colour and dash levels on.
    function visible(series, st) {
      return series.filter(function (s) {
        return st.runOn.has(s.index) &&
          (s.colourLevel == null || st.colourOn.has(s.colourLevel)) &&
          (s.dashLevel == null || st.dashOn.has(s.dashLevel));
      });
    }

    function drawPanels(series, st, labelled, onBrush) {
      var axis = st.axis, logX = st.logX;
      var svg = d3.create("svg")
        .attr("viewBox", "0 0 " + W + " " + (PANELS.length * (H + GAP) - GAP))
        .attr("width", "100%").style("height", "auto")
        .style("overflow", "visible");
      var defs = svg.append("defs");

      var shown = series.map(function (s) {
        return { run: s, pts: onAxis(s, axis, logX) };
      }).filter(function (d) { return d.pts.length > 0; });

      var xs = [];
      shown.forEach(function (d) {
        d.pts.forEach(function (p) { xs.push(xOf(p, axis)); });
      });
      var xext = xs.length ? d3.extent(xs) : [0, 1];
      // The typed or brushed window wins over the data's own extent.
      var lo = st.xRange[0] != null ? st.xRange[0] : xext[0];
      var hi = st.xRange[1] != null ? st.xRange[1] : xext[1];
      if (logX && !(lo > 0)) lo = Math.max(xext[0], 1e-9);
      if (!(hi > lo)) hi = lo + Math.max(Math.abs(lo) * 1e-4, 1e-9);
      var x = logX
        ? d3.scaleLog([lo, hi], [M.l, W - M.r])
        : d3.scaleLinear([st.xRange[0] != null ? lo : Math.min(0, lo), hi],
          [M.l, W - M.r]);
      var xwin = [x.domain()[0], x.domain()[1]];

      var scales = { x: x, y: {} };
      var paths = new Map();
      shown.forEach(function (d) { paths.set(d.run, []); });

      PANELS.forEach(function (panel, r) {
        var top = r * (H + GAP);
        var g = svg.append("g").attr("transform", "translate(0," + top + ")");
        // The y scale fits the points inside the x window, so zooming in
        // on x re-spreads the lines vertically as well.
        var vals = [];
        shown.forEach(function (d) {
          panelPts(d.pts, panel).forEach(function (p) {
            if (within(xOf(p, axis), xwin)) vals.push(p[panel.key]);
          });
        });
        var ext = vals.length ? d3.extent(vals) : [0, 1];
        var yr = st.yRange[panel.key];
        var ylo = yr[0] != null ? yr[0] : (panel.log ? ext[0] : Math.min(ext[0], 0));
        var yhi = yr[1] != null ? yr[1] : ext[1];
        if (panel.log && !(ylo > 0)) ylo = Math.max(ext[0], 1e-300);
        if (!(yhi > ylo)) yhi = ylo + Math.max(Math.abs(ylo) * 1e-4, 1e-12);
        var y = panel.log
          ? d3.scaleLog([ylo, yhi], [H - M.b, M.t])
          : d3.scaleLinear([ylo, yhi], [H - M.b, M.t]);
        if (!panel.log && yr[0] == null && yr[1] == null) y = y.nice();
        scales.y[panel.key] = y;

        var clipId = uid + "-" + panel.key;
        defs.append("clipPath").attr("id", clipId).append("rect")
          .attr("x", M.l).attr("y", M.t - 2)
          .attr("width", W - M.l - M.r).attr("height", H - M.t - M.b + 4);

        // Recessive grid: one faint rule per tick, under the lines.
        g.append("g").selectAll("line").data(y.ticks(3)).join("line")
          .attr("x1", M.l).attr("x2", W - M.r)
          .attr("y1", y).attr("y2", y)
          .attr("stroke", "#ece7dc");

        var xAxis = logX
          ? d3.axisBottom(x).ticks(6, "~g").tickSize(3)
          : d3.axisBottom(x).ticks(6).tickSize(3);
        g.append("g").attr("transform", "translate(0," + (H - M.b) + ")")
          .call(xAxis);
        g.append("g").attr("transform", "translate(" + M.l + ",0)")
          .call(panel.log
            ? d3.axisLeft(y).ticks(3, "0.0e").tickSize(3)
            : d3.axisLeft(y).ticks(3).tickSize(3)
              .tickFormat(d3.format(".3f")));

        // Lines are clipped to the plotting area, so a typed range simply
        // cuts them off rather than stretching the scale to hold them.
        var lines = g.append("g").attr("clip-path", "url(#" + clipId + ")");
        var ends = [];
        shown.forEach(function (d) {
          var pts = panelPts(d.pts, panel);
          if (!pts.length) return;
          var path = lines.append("path").datum(pts).attr("fill", "none")
            .attr("stroke", d.run.colour).attr("stroke-width", 1.6)
            .attr("stroke-dasharray", d.run.dash)
            .attr("stroke-linejoin", "round").attr("stroke-linecap", "round")
            .attr("d", d3.line()
              .x(function (p) { return x(xOf(p, axis)); })
              .y(function (p) { return y(p[panel.key]); }));
          paths.get(d.run).push(path);
          // The label sits at the last point inside the window.
          var inside = pts.filter(function (p) {
            return within(xOf(p, axis), xwin) && within(p[panel.key], y.domain());
          });
          if (inside.length) {
            var last = inside[inside.length - 1];
            ends.push({
              run: d.run, x: x(xOf(last, axis)), y: y(last[panel.key])
            });
          }
        });

        if (labelled) endLabels(g, ends, shown, panel, x, y, axis);

        // Drag across a panel to zoom the x window; double-click resets it.
        // The brush sits above the lines but below the hover marks, and its
        // overlay lets mousemove through to the SVG for the readout.
        g.append("g").attr("class", "riprvis-brush").call(
          d3.brushX()
            .extent([[M.l, M.t], [W - M.r, H - M.b]])
            .on("end", function (event) {
              if (!event.sourceEvent || !event.selection) return;
              onBrush([x.invert(event.selection[0]), x.invert(event.selection[1])]);
            })
        );

        // Positioned by transform rather than x/y: Chrome mispaints
        // attribute-positioned SVG text created under a CSS zoom (as in a
        // reveal.js deck) by the zoom factor, while transforms are exact.
        g.append("text").attr("transform", "translate(" + M.l + ",9)")
          .attr("font-size", 11).attr("fill", V.MUTED).text(panel.label);
        if (r === PANELS.length - 1) {
          g.append("text")
            .attr("transform", "translate(" + (W - M.r) + "," + (H + 2) + ")")
            .attr("text-anchor", "end")
            .attr("font-size", 10).attr("fill", V.MUTED)
            .attr("font-family", V.MONO)
            .text(axis === "time" ? "time (s)" : "step");
        }
      });

      V.styleAxes(svg);
      var hover = svg.append("g").attr("pointer-events", "none");
      return { svg: svg, scales: scales, hover: hover, shown: shown,
        paths: paths };
    }

    // Direct labels at the lines' ends, so identity never rests on the
    // colour alone. Two things get in the way of a label: another label
    // (runs that converge end at the same height) and another run's line
    // (a run that finishes early on the clock ends mid-chart, with the
    // longer runs passing right through where its label wants to sit).
    // Labels sharing a column are spread apart; every label is then pushed
    // off any line within a line-height of it, and given a paper halo for
    // whatever still crosses beneath. A leader in the run's colour joins
    // each to its line.
    function endLabels(g, ends, shown, panel, x, y, axis) {
      var LH = 12;
      var slots = ends.map(function (e) { return e.y; });
      var order = d3.range(ends.length).sort(function (a, b) {
        return ends[a].x - ends[b].x || ends[a].y - ends[b].y;
      });
      var col = [];
      function spread(idx) {
        idx.sort(function (a, b) { return slots[a] - slots[b]; });
        for (var k = 1; k < idx.length; k++) {
          var lo = slots[idx[k - 1]] + LH;
          if (slots[idx[k]] < lo) slots[idx[k]] = lo;
        }
      }
      order.forEach(function (i) {
        if (col.length && ends[i].x - ends[col[0]].x > 30) {
          spread(col);
          col = [];
        }
        col.push(i);
      });
      spread(col);
      function lineY(d, X) {
        var pts = panelPts(d.pts, panel);
        for (var k = 1; k < pts.length; k++) {
          var xa = x(xOf(pts[k - 1], axis)), xb = x(xOf(pts[k], axis));
          if (xa <= X && X <= xb) {
            var ya = y(pts[k - 1][panel.key]), yb = y(pts[k][panel.key]);
            return xb === xa ? ya : ya + (yb - ya) * (X - xa) / (xb - xa);
          }
        }
        return null;
      }
      ends.forEach(function (e, i) {
        var lx = e.x + 17, rx = lx + 6.3 * e.run.label.length;
        var crossings = [];
        shown.forEach(function (d) {
          if (d.run === e.run) return;
          [lx, (lx + rx) / 2, rx].forEach(function (X) {
            var ly = lineY(d, X);
            if (ly != null) crossings.push(ly);
          });
        });
        var clear = function (yy) {
          return crossings.every(function (c) { return Math.abs(c - yy) >= LH * 0.8; }) &&
            ends.every(function (o, j) { return j === i || Math.abs(slots[j] - yy) >= LH; });
        };
        if (!clear(slots[i])) {
          var tries = [slots[i] - LH, slots[i] + LH, slots[i] - 2 * LH,
            slots[i] + 2 * LH];
          for (var t = 0; t < tries.length; t++) {
            if (tries[t] >= M.t && tries[t] <= H - M.b + 4 && clear(tries[t])) {
              slots[i] = tries[t];
              break;
            }
          }
        }
      });
      var over = d3.max(slots) - (H - M.b + 4);
      if (over > 0) slots = slots.map(function (v) { return v - over; });
      ends.forEach(function (e, i) {
        g.append("path").attr("fill", "none")
          .attr("stroke", e.run.colour).attr("stroke-width", 1.4)
          .attr("d", "M" + (e.x + 3) + "," + e.y +
            "L" + (e.x + 9) + "," + slots[i] + "H" + (e.x + 14));
        g.append("text").attr("transform",
          "translate(" + (e.x + 17) + "," + (slots[i] + 3.5) + ")")
          .attr("font-size", 10.5).attr("fill", V.INK)
          .attr("font-family", V.MONO)
          .attr("paint-order", "stroke").attr("stroke", "#fdfcf9")
          .attr("stroke-width", 3).attr("stroke-linejoin", "round")
          .text(e.run.label);
      });
    }

    // The last point of each run at or before the cursor's x; with no
    // cursor, its final point -- the resting readout is where each run ended.
    function pick(shown, axis, cx) {
      return shown.map(function (d) {
        var pts = d.pts;
        if (cx == null) return { run: d.run, p: pts[pts.length - 1] };
        var p = null;
        for (var i = 0; i < pts.length; i++) {
          if (xOf(pts[i], axis) <= cx) p = pts[i]; else break;
        }
        return { run: d.run, p: p };
      });
    }

    function drawHover(view, axis, picked, cx) {
      var hv = view.hover;
      hv.selectAll("*").remove();
      if (cx == null) return;
      var px = view.scales.x(cx);
      PANELS.forEach(function (panel, r) {
        var top = r * (H + GAP);
        hv.append("line").attr("x1", px).attr("x2", px)
          .attr("y1", top + M.t).attr("y2", top + H - M.b)
          .attr("stroke", V.MUTED).attr("stroke-dasharray", "2 3");
        picked.forEach(function (d) {
          if (!d.p) return;
          var v = d.p[panel.key];
          if (v == null || (panel.log && !(v > 0))) return;
          var y = view.scales.y[panel.key];
          if (!within(v, y.domain())) return;
          hv.append("circle").attr("r", 4.5)
            .attr("cx", view.scales.x(xOf(d.p, axis)))
            .attr("cy", top + y(v))
            .attr("fill", d.run.colour).attr("stroke", "#fdfcf9")
            .attr("stroke-width", 2);
        });
      });
    }

    // With many runs, only the one nearest the cursor is read off: in the
    // panel the cursor is over, the picked point whose drawn height is
    // closest to it.
    function nearest(view, axis, picked, py) {
      var r = Math.min(PANELS.length - 1,
        Math.max(0, Math.floor(py / (H + GAP))));
      var panel = PANELS[r], top = r * (H + GAP);
      var best = null, dist = Infinity;
      picked.forEach(function (d) {
        if (!d.p) return;
        var v = d.p[panel.key];
        if (v == null || (panel.log && !(v > 0))) return;
        var dy = Math.abs(top + view.scales.y[panel.key](v) - py);
        if (dy < dist) { dist = dy; best = d; }
      });
      return best;
    }

    function highlight(view, run) {
      view.paths.forEach(function (ps, r) {
        ps.forEach(function (p) {
          p.attr("stroke-opacity", run == null || r === run ? 1 : 0.18)
            .attr("stroke-width", r === run ? 2.4 : 1.6);
        });
      });
    }

    function readoutLines(picked, hint) {
      var node = V.el("div", "riprvis-readout");
      if (hint) {
        node.append(V.el("span", "riprvis-hint", hint));
        return node;
      }
      picked.forEach(function (d) {
        var line = V.el("span", "riprvis-line");
        line.append(swatch(d.run.colour, d.run.dash), V.el("span", null, d.run.label));
        var p = d.p;
        if (!p) {
          line.append(dim("not yet started"));
        } else {
          line.append(
            dim(p.phase), text(""),
            dim("step"), text(String(p.step)),
            dim("t"), text(p.time == null ? "—" : d3.format(".2f")(p.time) + "s"),
            dim("KL"), text(PANELS[0].fmt(p.kl)),
            dim("gap"), text(p.gap == null ? "—" : PANELS[1].fmt(p.gap)),
            dim("growth"), text(p.growth == null ? "—" : PANELS[2].fmt(p.growth))
          );
        }
        node.append(line);
      });
      return node;
    }
    function dim(t) { return V.el("span", "riprvis-dim", t); }
    function text(t) { return V.el("span", null, " " + t); }

    // A toggle key: click flips it, shift-click keeps only it within its
    // set (all if it was already the only one on).
    function key(className, node, isOn, onToggle, onSolo) {
      node.classList.add(className);
      node.classList.toggle("off", !isOn);
      node.title = "click to hide or show, shift-click to show only this";
      node.onclick = function (event) {
        if (event.shiftKey) onSolo(); else onToggle();
      };
      return node;
    }

    // Toggle a member of a set, or solo it: keep only it, unless it is
    // already alone, in which case bring everything back.
    function toggleIn(set, i) { if (set.has(i)) set.delete(i); else set.add(i); }
    function soloIn(set, i, all) {
      if (set.size === 1 && set.has(i)) {
        all.forEach(function (j) { set.add(j); });
      } else {
        set.clear();
        set.add(i);
      }
    }
    function allOf(n) { return d3.range(n); }

    // The legend: one toggle per colour level and per dash level when the
    // runs are grouped, plus a chip per run either way, so a single run can
    // be pulled out of a group or an ungrouped payload can be thinned.
    function legendOf(data, series, st, onChange) {
      var wrap = V.el("div");
      var legend = V.el("div", "riprvis-legend");
      function group(levels, on, colourOf, dashOf) {
        var g = V.el("span", "riprvis-group");
        levels.forEach(function (name, i) {
          var item = V.el("span");
          item.append(swatch(colourOf(i), dashOf(i)), V.el("span", null, name));
          key("riprvis-key", item, on.has(i), function () {
            toggleIn(on, i); onChange();
          }, function () {
            soloIn(on, i, allOf(levels.length)); onChange();
          });
          g.append(item, V.el("span", null, " "));
        });
        if (on.size < levels.length) {
          var all = V.el("button", "riprvis-all", "all");
          all.type = "button";
          all.onclick = function () {
            allOf(levels.length).forEach(function (j) { on.add(j); });
            onChange();
          };
          g.append(all);
        }
        return g;
      }
      if (data.legend.colour) {
        legend.append(group(data.legend.colour, st.colourOn,
          function (i) { return i < COLOURS.length ? COLOURS[i] : OVERFLOW; },
          function () { return null; }));
      }
      if (data.legend.dash) {
        legend.append(group(data.legend.dash, st.dashOn,
          function () { return V.INK; },
          function (i) { return dashOf(data, i); }));
      }
      if (data.legend.colour || data.legend.dash) wrap.append(legend);

      var chips = V.el("div", "riprvis-chips");
      series.forEach(function (s) {
        var chip = V.el("span");
        chip.append(swatch(s.colour, s.dash), V.el("span", null, s.label));
        if (data.has_time && !s.timed) {
          chip.append(V.el("span", "riprvis-dim", " (untimed)"));
        }
        key("riprvis-chip", chip, st.runOn.has(s.index), function () {
          toggleIn(st.runOn, s.index); onChange();
        }, function () {
          soloIn(st.runOn, s.index, allOf(series.length)); onChange();
        });
        chips.append(chip);
      });
      if (st.runOn.size < series.length) {
        var all = V.el("button", "riprvis-all", "all runs");
        all.type = "button";
        all.onclick = function () {
          allOf(series.length).forEach(function (j) { st.runOn.add(j); });
          onChange();
        };
        chips.append(all);
      }
      // With more runs than the labelled view holds, keep only the FEW
      // whose growth peaks highest, and turn every level back on so all of
      // them show; the chips then say which they are.
      if (series.length > FEW) {
        var top = V.el("button", "riprvis-all", "top " + FEW + " by growth");
        top.type = "button";
        top.title = "keep only the " + FEW +
          " runs whose growth peaks highest at any point";
        top.onclick = function () {
          st.runOn = new Set(topByGrowth(series, FEW));
          allOf(data.legend.colour ? data.legend.colour.length : 0)
            .forEach(function (j) { st.colourOn.add(j); });
          allOf(data.legend.dash ? data.legend.dash.length : 0)
            .forEach(function (j) { st.dashOn.add(j); });
          onChange();
        };
        chips.append(top);
      }
      wrap.append(chips);
      return wrap;
    }

    // A pair of number inputs for a [min, max] range; empty means open.
    function rangeInput(label, range, onChange) {
      var wrap = V.el("span", "riprvis-range");
      wrap.append(V.el("span", null, label + " "));
      var inputs = [0, 1].map(function (k) {
        var input = V.el("input");
        input.type = "number";
        input.step = "any";
        input.placeholder = k === 0 ? "min" : "max";
        input.value = range[k] == null ? "" : String(range[k]);
        input.onchange = function () {
          var v = input.value === "" ? null : Number(input.value);
          range[k] = v == null || isNaN(v) ? null : v;
          onChange();
        };
        return input;
      });
      wrap.append(inputs[0], V.el("span", null, "–"), inputs[1]);
      return {
        el: wrap,
        sync: function () {
          inputs.forEach(function (input, k) {
            input.value = range[k] == null ? "" : d3.format(".4~g")(range[k]);
          });
        }
      };
    }

    return {
      renderValue: function (x) {
        var data = JSON.parse(x.data);
        // An ungrouped payload carries no legend entries; guard against
        // anything that is not a list of level names.
        var lg = data.legend || {};
        data.legend = {
          colour: Array.isArray(lg.colour) ? lg.colour : null,
          dash: Array.isArray(lg.dash) ? lg.dash : null,
          dashes: Array.isArray(lg.dashes) ? lg.dashes : null
        };
        var series = seriesOf(data);

        // Everything the controls change lives here; redraw() reads it.
        var st = {
          axis: "step",
          logX: false,
          xRange: [null, null],
          yRange: { kl: [null, null], gap: [null, null], growth: [null, null] },
          runOn: new Set(allOf(series.length)),
          colourOn: new Set(allOf(data.legend.colour ? data.legend.colour.length : 0)),
          dashOn: new Set(allOf(data.legend.dash ? data.legend.dash.length : 0))
        };

        el.replaceChildren();
        el.classList.add("riprvis", "riprvis-wide");

        var legendWrap = V.el("div");
        var chartWrap = V.el("div");
        var readoutWrap = V.el("div");
        var knobs = V.el("div", "riprvis-knobs");
        var ranges = V.el("div", "riprvis-ranges");
        el.append(legendWrap, chartWrap, readoutWrap, knobs, ranges);

        var view = null;
        var HINT = "hover a line to read it off";

        function rest(few) {
          if (few) {
            readoutWrap.replaceChildren(readoutLines(pick(view.shown, st.axis, null)));
          } else {
            readoutWrap.replaceChildren(readoutLines(null, HINT));
            highlight(view, null);
          }
        }

        var xInput = rangeInput(st.axis, st.xRange, redraw);
        var yInputs = PANELS.map(function (panel) {
          return rangeInput(panel.key, st.yRange[panel.key], redraw);
        });

        function redraw() {
          legendWrap.replaceChildren(legendOf(data, series, st, redraw));
          var shown = visible(series, st);
          var few = shown.length <= FEW;
          view = drawPanels(shown, st, few, function (range) {
            st.xRange = range;
            xInput.sync();
            redraw();
          });
          // The x inputs speak the current axis's units.
          xInput.el.firstChild.textContent = st.axis + " ";
          chartWrap.replaceChildren(view.svg.node());
          rest(few);

          var node = view.svg.node();
          view.svg.on("mousemove", function (event) {
            var pt = d3.pointer(event, node);
            if (pt[0] < M.l || pt[0] > W - M.r) return;
            var cx = view.scales.x.invert(pt[0]);
            var picked = pick(view.shown, st.axis, cx);
            if (!few) {
              var best = nearest(view, st.axis, picked, pt[1]);
              picked = best ? [best] : [];
              highlight(view, best ? best.run : null);
            }
            drawHover(view, st.axis, picked, cx);
            readoutWrap.replaceChildren(
              picked.length ? readoutLines(picked) : readoutLines(null, HINT)
            );
          }).on("mouseleave", function () {
            drawHover(view, st.axis, null, null);
            rest(few);
          }).on("dblclick", function () {
            st.xRange = [null, null];
            xInput.sync();
            redraw();
          });
        }

        var tabs = V.tabsInput(
          data.has_time ? ["step", "time"] : ["step"],
          function (i) {
            st.axis = i === 1 ? "time" : "step";
            // A window typed in one axis's units means nothing in the other.
            st.xRange = [null, null];
            xInput.sync();
            redraw();
          }
        );
        var logToggle = V.toggleInput("log x", function (checked) {
          st.logX = checked;
          redraw();
        });
        knobs.append(tabs.el, logToggle.el);

        var reset = V.el("button", null, "reset axes");
        reset.type = "button";
        reset.title = "clear every typed or brushed range";
        reset.onclick = function () {
          st.xRange[0] = st.xRange[1] = null;
          PANELS.forEach(function (panel) {
            st.yRange[panel.key][0] = st.yRange[panel.key][1] = null;
          });
          xInput.sync();
          yInputs.forEach(function (yi) { yi.sync(); });
          redraw();
        };
        ranges.append(xInput.el);
        yInputs.forEach(function (yi) { ranges.append(yi.el); });
        ranges.append(reset,
          V.el("span", "riprvis-dim", "drag on a panel to zoom x, double-click to reset"));

        redraw();
      },

      resize: function (width, height) {
        // The SVG is fixed-aspect via its viewBox and scales with its
        // container; nothing to recompute.
      }
    };
  }
});
