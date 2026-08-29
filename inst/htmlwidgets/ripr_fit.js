/* Fitting the projection: the Bernstein field G_i = E_theta[Q/P_i] over the
 * course of a Frank-Wolfe/EM fit, alongside its KL/gap/log-growth trace.
 * Ported from drawField()/fieldReadout()/drawFitPanels() in
 * search-slides.qmd. The Bernstein basis (RiprVis.buildGrid) is expensive, so
 * it is built once per renderValue and reused for every step's contour. */
HTMLWidgets.widget({
  name: "ripr_fit",
  type: "output",

  factory: function (el, width, height) {
    var V = window.RiprVis, d3 = window.d3;

    function drawField(ex, i, grid) {
      var svg = d3.create("svg").attr("viewBox", "0 0 500 456")
        .attr("width", "100%").style("height", "auto")
        .style("overflow", "visible");
      // Namespaced by the widget's own element id as well as the example's
      // title, so several fit widgets on one page never share a clip id.
      var clipId = "clip-" + el.id + "-" + ex.labels.title;
      svg.append("clipPath").attr("id", clipId)
        .append("path").attr("d", V.SIMPLEX);

      var j = Math.max(i, 1) - 1;
      var z = V.evalField(grid, ex.fit.ratio[j]);
      var bands = d3.contours().size([grid.nx, grid.ny])
        .thresholds(d3.range(-1.2, 1.21, 0.1))(z);

      var g = svg.append("g").attr("clip-path", "url(#" + clipId + ")");
      g.append("path").attr("d", V.SIMPLEX).attr("fill", V.fieldColour(-1.2));
      g.selectAll("path.band").data(bands).join("path").attr("class", "band")
        .attr("d", grid.path)
        .attr("fill", function (d) { return V.fieldColour(d.value); });

      // The one contour that matters.
      g.append("g")
        .selectAll("path")
        .data(d3.contours().size([grid.nx, grid.ny]).thresholds([0])(z))
        .join("path")
        .attr("d", grid.path).attr("fill", "none")
        .attr("stroke", V.INK).attr("stroke-width", 1.7);

      var outer = svg.append("g");
      // The null, so it is clear where the oracle is allowed to look.
      outer.append("g").selectAll("path").data(ex.seeds).join("path")
        .attr("d", V.poly).attr("fill", "none")
        .attr("stroke", V.INK).attr("stroke-width", 1)
        .attr("stroke-dasharray", "4 3");
      outer.append("path").attr("d", V.SIMPLEX)
        .attr("fill", "none").attr("stroke", "#8d867a")
        .attr("stroke-width", 1.2);

      // Support of the current iterate, and the alternative for reference.
      var sup = ex.fit.support[j];
      var wmax = d3.max(sup.weights);
      outer.append("g").selectAll("circle").data(sup.atoms).join("circle")
        .attr("cx", function (p) { return V.toXY(p)[0]; })
        .attr("cy", function (p) { return V.toXY(p)[1]; })
        .attr("r", function (p, k) { return V.atomRadius(sup.weights[k], wmax); })
        .attr("fill", "none").attr("stroke", V.COOL).attr("stroke-width", 1.2);
      outer.append("g").selectAll("circle").data(ex.marks.q).join("circle")
        .attr("cx", function (p) { return V.toXY(p)[0]; })
        .attr("cy", function (p) { return V.toXY(p)[1]; })
        .attr("r", 3).attr("fill", V.INK);

      // Where the gap is attained: the maximiser of G over H0, taken from the
      // trace rather than found on the grid, so it is exact and needs no
      // point-in-null test here.
      var target = ex.fit.gap_theta[j];
      if (target) {
        var p = V.toXY(target);
        outer.append("circle").attr("cx", p[0]).attr("cy", p[1]).attr("r", 6)
          .attr("fill", "none").attr("stroke", V.CLAY).attr("stroke-width", 1.8);
      }

      V.vertexLabels(outer);
      return svg.node();
    }

    function fieldReadout(ex, i) {
      var j = Math.max(i, 1) - 1;
      var kl = ex.fit.kl[j], gap = ex.fit.gap[j];
      var f = d3.format(".5f");
      var html = ex.fit.phase[j] +
        " " + V.dim("KL") + " " + f(kl) +
        " " + V.dim("gap") + " " +
        (gap == null ? "—" : d3.format(".2e")(gap)) +
        " " + V.dim("growth") + " " +
        (gap == null ? "—" : f(kl - Math.log1p(gap)));
      return V.readout(html);
    }

    // KL is what the fit minimises; the gap certifies how far it still is
    // from the minimum; the difference KL - log(1 + gap) is the log-growth
    // rate the resulting e-variable is guaranteed. Together they show the
    // third rising as the second falls, which is the whole reason for
    // running the fit to convergence rather than stopping when KL flattens.
    function drawFitPanels(ex, i) {
      var W = 420, H = 108, gap = 20, m = { l: 58, r: 10, t: 14, b: 20 };
      var n = ex.nFit;
      var svg = d3.create("svg")
        .attr("viewBox", "0 0 " + W + " " + (3 * H + 2 * gap))
        .attr("width", "100%").style("height", "auto")
        .style("overflow", "visible");
      var x = d3.scaleLinear([1, n], [m.l, W - m.r]);

      var series = [
        {
          label: "KL(Q ‖ P)", log: false, colour: V.COOL,
          v: ex.fit.kl.map(function (k, j) { return { t: j + 1, y: k }; })
        },
        {
          label: "gap", log: true, colour: V.CLAY,
          v: ex.fit.gap.map(function (g, j) { return { t: j + 1, y: g }; })
            .filter(function (d) { return d.y != null && d.y > 0; })
        },
        {
          label: "log-growth  KL − log(1 + gap)", log: false, colour: V.INK,
          v: ex.fit.kl.map(function (k, j) {
            return {
              t: j + 1,
              y: ex.fit.gap[j] == null ? null : k - Math.log1p(ex.fit.gap[j])
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
            .attr("stroke", V.MUTED).attr("stroke-dasharray", "2 3");
          g.append("circle").attr("r", 3).attr("fill", s.colour)
            .attr("cx", x(here.t)).attr("cy", y(here.y));
        }
        g.append("text").attr("x", m.l).attr("y", 9)
          .attr("font-size", 11).attr("fill", V.MUTED).text(s.label);
      });

      V.styleAxes(svg);
      return svg.node();
    }

    var loop = null;
    var step = 0;

    return {
      renderValue: function (x) {
        // Kill any loop from a previous render before tearing the DOM down,
        // so a stale interval can never survive a Shiny update.
        if (loop) loop.stop();

        var ex = JSON.parse(x.data);
        ex.nFit = ex.fit.ratio.length;
        // Built once: the Bernstein basis does not change across steps, only
        // the ratio vector it is dotted against does.
        var grid = V.buildGrid(ex.lattice);

        el.replaceChildren();
        var regions = V.stageLayout(el);

        var fieldWrap = V.el("div");
        var readoutWrap = V.el("div");
        regions.left.append(fieldWrap, readoutWrap);

        var panelsWrap = V.el("div");
        regions.right.append(panelsWrap);

        step = 0;

        function redraw() {
          fieldWrap.replaceChildren(drawField(ex, step, grid));
          readoutWrap.replaceChildren(fieldReadout(ex, step));
          panelsWrap.replaceChildren(drawFitPanels(ex, step));
        }

        var slider = V.rangeInput(ex.nFit, "step", function (v) {
          step = v;
          redraw();
        });

        loop = V.makePlayLoop(220, function () {
          var last = ex.nFit;
          var v = slider.value >= last ? 1 : slider.value + 1;
          slider.value = v;
          step = v;
          redraw();
        });

        var play = V.toggleInput("play", function (checked) {
          if (checked) loop.start(); else loop.stop();
        });

        regions.knobs.append(slider.el, play.el);

        redraw();
      },

      resize: function (width, height) {
        // Every SVG here is fixed-aspect via its viewBox and scales with its
        // container; there is nothing to recompute on resize.
      }
    };
  }
});
