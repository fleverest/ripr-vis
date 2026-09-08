/* The planar problem, stated: the null region's polyhedral parts (possibly
 * unbounded -- vertices plus recession rays and lineality directions) and the
 * alternative's support over the parameter plane. The planar analogue of
 * ripr_problem_simplex.js. */
HTMLWidgets.widget({
  name: "ripr_problem2d",
  type: "output",

  factory: function (el, width, height) {
    var V = window.RiprVis, d3 = window.d3;

    function drawProblem2d(ex) {
      var svg = d3.create("svg").attr("viewBox", "0 0 500 456")
        .attr("width", "100%").style("height", "auto")
        .style("overflow", "visible");
      var view = V.planeView(ex.extent);
      var r = view.rect;

      // Namespaced by the widget's own element id as well as the example's
      // title, so several problem widgets on one page never share a clip id.
      var clipId = ("clip-" + el.id + "-" + ex.labels.title)
        .replace(/[^A-Za-z0-9_-]/g, "-");
      svg.append("clipPath").attr("id", clipId).append("rect")
        .attr("x", r.x0).attr("y", r.y0)
        .attr("width", r.x1 - r.x0).attr("height", r.y1 - r.y0);

      // Unbounded parts recede past the plot rect along their rays, so the
      // fill and the part outlines are clipped to it; the rect's own border
      // and the alternative's atoms are drawn on top, unclipped.
      var g = svg.append("g").attr("clip-path", "url(#" + clipId + ")");

      var paths = ex.seeds.map(function (s) { return V.polyhedronPath(s, view); });
      // One path holding every part as a subpath, so the union fills as a
      // single region: stroking the parts separately would draw a boundary
      // through the middle of H0 where they overlap, and filling them as
      // separate paths leaves an antialiasing seam along the shared edge.
      g.append("path").attr("d", paths.join(""))
        .attr("fill", "#dedad0").attr("stroke", "none");
      // The parts themselves, dashed, as on the fitting view.
      g.append("g").selectAll("path").data(paths).join("path")
        .attr("d", function (d) { return d; }).attr("fill", "none")
        .attr("stroke", "#9c9488").attr("stroke-width", 1)
        .attr("stroke-dasharray", "4 3");

      svg.append("rect")
        .attr("x", r.x0).attr("y", r.y0)
        .attr("width", r.x1 - r.x0).attr("height", r.y1 - r.y0)
        .attr("fill", "none").attr("stroke", "#8d867a")
        .attr("stroke-width", 1.2);

      var wmax = d3.max(ex.marks.weights);
      svg.append("g").selectAll("circle").data(ex.marks.q).join("circle")
        .attr("cx", function (p) { return view.sx(p[0]); })
        .attr("cy", function (p) { return view.sy(p[1]); })
        .attr("r", function (p, i) {
          return V.atomRadius(ex.marks.weights[i], wmax, 6.5, 3.5);
        })
        .attr("fill", V.INK);

      V.planeAxes(svg, view);
      return svg.node();
    }

    return {
      renderValue: function (x) {
        var ex = JSON.parse(x.data);
        el.replaceChildren();
        el.classList.add("riprvis");
        var wrap = V.el("div", "riprvis-solo");
        wrap.append(drawProblem2d(ex));
        el.append(wrap);
      },

      resize: function (width, height) {
        // The SVG is fixed-aspect via its viewBox and scales with the
        // container; nothing to recompute.
      }
    };
  }
});
