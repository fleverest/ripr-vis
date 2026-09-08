/* The problem, stated: the null region's parts and the alternative's support
 * on the ternary simplex. Ported from drawProblem() in search-slides.qmd. */
HTMLWidgets.widget({
  name: "ripr_problem_simplex",
  type: "output",

  factory: function (el, width, height) {
    var V = window.RiprVis, d3 = window.d3;

    function drawProblem(ex) {
      var svg = d3.create("svg").attr("viewBox", "0 0 500 456")
        .attr("width", "100%").style("height", "auto")
        .style("overflow", "visible");
      var g = svg.append("g");

      // One path holding every part as a subpath, so the union fills as a
      // single region: stroking the parts separately would draw a boundary
      // through the middle of H0 where they overlap, and filling them as
      // separate paths leaves an antialiasing seam along the shared edge.
      g.append("path").attr("d", ex.seeds.map(V.poly).join(""))
        .attr("fill", "#dedad0").attr("stroke", "none");
      // The parts themselves, dashed, as on the fitting view.
      g.append("g").selectAll("path").data(ex.seeds).join("path")
        .attr("d", V.poly).attr("fill", "none")
        .attr("stroke", "#9c9488").attr("stroke-width", 1)
        .attr("stroke-dasharray", "4 3");
      g.append("path").attr("d", V.SIMPLEX)
        .attr("fill", "none").attr("stroke", "#8d867a")
        .attr("stroke-width", 1.2);

      var wmax = d3.max(ex.marks.weights);
      g.append("g").selectAll("circle").data(ex.marks.q).join("circle")
        .attr("cx", function (p) { return V.toXY(p)[0]; })
        .attr("cy", function (p) { return V.toXY(p)[1]; })
        .attr("r", function (p, i) {
          return V.atomRadius(ex.marks.weights[i], wmax, 6.5, 3.5);
        })
        .attr("fill", V.INK);

      V.vertexLabels(g);
      return svg.node();
    }

    return {
      renderValue: function (x) {
        var ex = JSON.parse(x.data);
        el.replaceChildren();
        el.classList.add("riprvis");
        var wrap = V.el("div", "riprvis-solo");
        wrap.append(drawProblem(ex));
        el.append(wrap);
      },

      resize: function (width, height) {
        // The SVG is fixed-aspect via its viewBox and scales with the
        // container; nothing to recompute.
      }
    };
  }
});
