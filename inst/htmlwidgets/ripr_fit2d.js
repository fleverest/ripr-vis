/* Fitting the projection over the parameter plane: the field
 * G_i(theta) = E_theta[Q/P_i] over the course of a Frank-Wolfe/EM fit,
 * alongside its KL/gap/log-growth trace. The planar analogue of ripr_fit.js.
 * Unlike the ternary field the grid values arrive precomputed from R (there
 * is no finite Bernstein basis for a continuous family), so drawField2d only
 * has to contour and colour them; the contour transform (grid index ->
 * pixel) depends only on the field's fixed grid, so it is built once per
 * renderValue and reused for every step. */
HTMLWidgets.widget({
  name: "ripr_fit2d",
  type: "output",

  factory: function (el, width, height) {
    var V = window.RiprVis, d3 = window.d3;

    function drawField2d(ex, i, view, path) {
      var svg = d3.create("svg").attr("viewBox", "0 0 500 456")
        .attr("width", "100%").style("height", "auto")
        .style("overflow", "visible");
      var r = view.rect;
      // Namespaced by the widget's own element id as well as the example's
      // title, so several fit widgets on one page never share a clip id.
      var clipId = ("clip-" + el.id + "-" + ex.labels.title)
        .replace(/[^A-Za-z0-9_-]/g, "-");
      svg.append("clipPath").attr("id", clipId).append("rect")
        .attr("x", r.x0).attr("y", r.y0)
        .attr("width", r.x1 - r.x0).attr("height", r.y1 - r.y0);

      var j = Math.max(i, 1) - 1;
      var f = ex.fit.field;
      var z = ex.fit.z[j];
      var bands = d3.contours().size([f.nx, f.ny])
        .thresholds(d3.range(-1.2, 1.21, 0.1))(z);

      // Clipped: the field bands and the null's parts, whose rays extend far
      // past the plot rect and must stop at its edge.
      var g = svg.append("g").attr("clip-path", "url(#" + clipId + ")");
      g.append("rect")
        .attr("x", r.x0).attr("y", r.y0)
        .attr("width", r.x1 - r.x0).attr("height", r.y1 - r.y0)
        .attr("fill", V.fieldColour(-1.2));
      g.selectAll("path.band").data(bands).join("path").attr("class", "band")
        .attr("d", path)
        .attr("fill", function (d) { return V.fieldColour(d.value); });

      // The one contour that matters.
      g.append("g")
        .selectAll("path")
        .data(d3.contours().size([f.nx, f.ny]).thresholds([0])(z))
        .join("path")
        .attr("d", path).attr("fill", "none")
        .attr("stroke", V.INK).attr("stroke-width", 1.7);

      // The null, so it is clear where the oracle is allowed to look.
      g.append("g").selectAll("path").data(ex.seeds).join("path")
        .attr("d", function (s) { return V.polyhedronPath(s, view); })
        .attr("fill", "none")
        .attr("stroke", V.INK).attr("stroke-width", 1)
        .attr("stroke-dasharray", "4 3");

      var outer = svg.append("g");
      outer.append("rect")
        .attr("x", r.x0).attr("y", r.y0)
        .attr("width", r.x1 - r.x0).attr("height", r.y1 - r.y0)
        .attr("fill", "none").attr("stroke", "#8d867a")
        .attr("stroke-width", 1.2);

      // Support of the current iterate, and the alternative for reference.
      var sup = ex.fit.support[j];
      var wmax = d3.max(sup.weights);
      outer.append("g").selectAll("circle").data(sup.atoms).join("circle")
        .attr("cx", function (p) { return view.sx(p[0]); })
        .attr("cy", function (p) { return view.sy(p[1]); })
        .attr("r", function (p, k) { return V.atomRadius(sup.weights[k], wmax); })
        .attr("fill", "none").attr("stroke", V.COOL).attr("stroke-width", 1.2);
      outer.append("g").selectAll("circle").data(ex.marks.q).join("circle")
        .attr("cx", function (p) { return view.sx(p[0]); })
        .attr("cy", function (p) { return view.sy(p[1]); })
        .attr("r", 3).attr("fill", V.INK);

      // Where the gap is attained: the maximiser of G over H0, taken from the
      // trace rather than found on the grid, so it is exact and needs no
      // point-in-null test here.
      var target = ex.fit.gap_theta[j];
      if (target) {
        outer.append("circle")
          .attr("cx", view.sx(target[0])).attr("cy", view.sy(target[1]))
          .attr("r", 6).attr("fill", "none")
          .attr("stroke", V.CLAY).attr("stroke-width", 1.8);
      }

      V.planeAxes(outer, view);
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
        ex.nFit = ex.fit.z.length;
        var view = V.planeView(ex.extent);
        var f = ex.fit.field;
        // Built once: the grid -> pixel transform does not change across
        // steps, only the z-values it contours do.
        var path = d3.geoPath(d3.geoTransform({
          point: function (px, py) {
            this.stream.point(view.sx(f.x0 + px * f.dx), view.sy(f.y0 + py * f.dy));
          }
        }));

        el.replaceChildren();
        var regions = V.stageLayout(el);

        var fieldWrap = V.el("div");
        var readoutWrap = V.el("div");
        regions.left.append(fieldWrap, readoutWrap);

        var panelsWrap = V.el("div");
        regions.right.append(panelsWrap);

        step = 0;

        function redraw() {
          fieldWrap.replaceChildren(drawField2d(ex, step, view, path));
          readoutWrap.replaceChildren(V.fitReadout(ex.fit, step));
          panelsWrap.replaceChildren(V.fitPanels(ex.fit, ex.nFit, step));
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
