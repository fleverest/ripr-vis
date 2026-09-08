/* Comparing fits: the KL / gap / growth traces of several runs of the same
 * problem drawn over one another, against the oracle-step count or -- where
 * the trace carries times -- the clock. The panels are the ones
 * RiprVis.fitPanels draws for a single fit, widened and given one line per
 * run. Colour and dash follow the run's grouping in the payload (or its
 * position, when ungrouped) and never change with the axis or the hover. */
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
    // Beyond this many runs the end labels and the per-run readout give way
    // to the legend and a nearest-line hover.
    var FEW = 5;

    var W = 900, H = 120, GAP = 18;
    var M = { l: 64, r: 100, t: 14, b: 22 };

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
          dash: data.legend.dash ? DASHES[(r.dash - 1) % DASHES.length] : null,
          pts: pts,
          timed: pts.some(function (p) { return p.time != null; })
        };
      });
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

    function swatch(colour, dash) {
      var sp = V.el("span", "riprvis-swatch");
      sp.innerHTML = "<svg width=\"22\" height=\"6\" viewBox=\"0 0 22 6\">" +
        "<line x1=\"0\" y1=\"3\" x2=\"22\" y2=\"3\" stroke=\"" + colour +
        "\" stroke-width=\"2\"" +
        (dash ? " stroke-dasharray=\"" + dash + "\"" : "") + "/></svg>";
      return sp;
    }

    function drawPanels(series, axis, logX, labelled) {
      var svg = d3.create("svg")
        .attr("viewBox", "0 0 " + W + " " + (PANELS.length * (H + GAP) - GAP))
        .attr("width", "100%").style("height", "auto")
        .style("overflow", "visible");

      var shown = series.map(function (s) {
        return { run: s, pts: onAxis(s, axis, logX) };
      }).filter(function (d) { return d.pts.length > 0; });

      var xs = [];
      shown.forEach(function (d) {
        d.pts.forEach(function (p) { xs.push(xOf(p, axis)); });
      });
      var xext = d3.extent(xs);
      var x = logX
        ? d3.scaleLog([xext[0], Math.max(xext[1], xext[0] * 1.0001)],
          [M.l, W - M.r])
        : d3.scaleLinear([0, Math.max(xext[1], 1e-9)], [M.l, W - M.r]);

      var scales = { x: x, y: {} };
      var paths = new Map();
      shown.forEach(function (d) { paths.set(d.run, []); });

      PANELS.forEach(function (panel, r) {
        var top = r * (H + GAP);
        var g = svg.append("g").attr("transform", "translate(0," + top + ")");
        var vals = [];
        shown.forEach(function (d) {
          panelPts(d.pts, panel).forEach(function (p) { vals.push(p[panel.key]); });
        });
        var ext = vals.length ? d3.extent(vals) : [0, 1];
        var y = panel.log
          ? d3.scaleLog([ext[0], Math.max(ext[1], ext[0] * 1.0001)],
            [H - M.b, M.t])
          : d3.scaleLinear([Math.min(ext[0], 0), ext[1]], [H - M.b, M.t])
            .nice();
        scales.y[panel.key] = y;

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

        var ends = [];
        shown.forEach(function (d) {
          var pts = panelPts(d.pts, panel);
          if (!pts.length) return;
          var path = g.append("path").datum(pts).attr("fill", "none")
            .attr("stroke", d.run.colour).attr("stroke-width", 1.6)
            .attr("stroke-dasharray", d.run.dash)
            .attr("stroke-linejoin", "round").attr("stroke-linecap", "round")
            .attr("d", d3.line()
              .x(function (p) { return x(xOf(p, axis)); })
              .y(function (p) { return y(p[panel.key]); }));
          paths.get(d.run).push(path);
          var last = pts[pts.length - 1];
          ends.push({
            run: d.run, x: x(xOf(last, axis)), y: y(last[panel.key])
          });
        });

        if (labelled) endLabels(g, ends, shown, panel, x, y, axis);

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
            .text(axis === "time" ? "time (s)" : "oracle step");
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
          hv.append("circle").attr("r", 4.5)
            .attr("cx", view.scales.x(xOf(d.p, axis)))
            .attr("cy", top + view.scales.y[panel.key](v))
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

    function legendOf(data, series) {
      var legend = V.el("div", "riprvis-legend");
      if (!data.legend.colour && !data.legend.dash) {
        series.forEach(function (s) {
          var item = V.el("span");
          item.append(swatch(s.colour, null), V.el("span", null, s.label));
          if (data.has_time && !s.timed) {
            item.append(V.el("span", "riprvis-dim", " (untimed)"));
          }
          legend.append(item);
        });
        return legend;
      }
      if (data.legend.colour) {
        var gc = V.el("span", "riprvis-group");
        data.legend.colour.forEach(function (name, i) {
          var item = V.el("span");
          item.append(swatch(i < COLOURS.length ? COLOURS[i] : OVERFLOW, null),
            V.el("span", null, name));
          gc.append(item, V.el("span", null, " "));
        });
        legend.append(gc);
      }
      if (data.legend.dash) {
        var gd = V.el("span", "riprvis-group");
        data.legend.dash.forEach(function (name, i) {
          var item = V.el("span");
          item.append(swatch(V.INK, DASHES[i % DASHES.length]),
            V.el("span", null, name));
          gd.append(item, V.el("span", null, " "));
        });
        legend.append(gd);
      }
      return legend;
    }

    return {
      renderValue: function (x) {
        var data = JSON.parse(x.data);
        // An ungrouped payload carries no legend entries; guard against
        // anything that is not a list of level names.
        var lg = data.legend || {};
        data.legend = {
          colour: Array.isArray(lg.colour) ? lg.colour : null,
          dash: Array.isArray(lg.dash) ? lg.dash : null
        };
        var series = seriesOf(data);
        var few = series.length <= FEW;

        el.replaceChildren();
        el.classList.add("riprvis", "riprvis-wide");

        var chartWrap = V.el("div");
        var readoutWrap = V.el("div");
        var knobs = V.el("div", "riprvis-knobs");
        el.append(legendOf(data, series), chartWrap, readoutWrap, knobs);

        var axis = "step", logX = false;
        var view = null;
        var HINT = "hover a line to read it off";

        function rest() {
          if (few) {
            readoutWrap.replaceChildren(readoutLines(pick(view.shown, axis, null)));
          } else {
            readoutWrap.replaceChildren(readoutLines(null, HINT));
            highlight(view, null);
          }
        }

        function redraw() {
          view = drawPanels(series, axis, logX, few);
          chartWrap.replaceChildren(view.svg.node());
          rest();

          var node = view.svg.node();
          view.svg.on("mousemove", function (event) {
            var pt = d3.pointer(event, node);
            var cx = view.scales.x.invert(
              Math.max(M.l, Math.min(W - M.r, pt[0]))
            );
            var picked = pick(view.shown, axis, cx);
            if (!few) {
              var best = nearest(view, axis, picked, pt[1]);
              picked = best ? [best] : [];
              highlight(view, best ? best.run : null);
            }
            drawHover(view, axis, picked, cx);
            readoutWrap.replaceChildren(
              picked.length ? readoutLines(picked) : readoutLines(null, HINT)
            );
          }).on("mouseleave", function () {
            drawHover(view, axis, null, null);
            rest();
          });
        }

        var tabs = V.tabsInput(
          data.has_time ? ["step", "time"] : ["step"],
          function (i) { axis = i === 1 ? "time" : "step"; redraw(); }
        );
        var logToggle = V.toggleInput("log x", function (checked) {
          logX = checked;
          redraw();
        });
        knobs.append(tabs.el, logToggle.el);

        redraw();
      },

      resize: function (width, height) {
        // The SVG is fixed-aspect via its viewBox and scales with its
        // container; nothing to recompute.
      }
    };
  }
});
