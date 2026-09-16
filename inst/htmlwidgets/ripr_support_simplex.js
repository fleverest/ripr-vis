/* The support, frame by frame: one finite mixture per slider position drawn
 * on the ternary simplex over the null region and the alternative, with the
 * other frames' atoms left as ghosts so the slider reads as a path. The
 * frames are whatever the payload lines up -- the near-RIPr supports of one
 * problem as the trial count varies, say. The problem is drawn as
 * ripr_problem_simplex draws it. */
HTMLWidgets.widget({
  name: "ripr_support_simplex",
  type: "output",

  factory: function (el, width, height) {
    var V = window.RiprVis, d3 = window.d3;
    var loop = null;

    function drawFrame(ex, i) {
      var svg = d3.create("svg").attr("viewBox", "0 0 500 456")
        .attr("width", "100%").style("height", "auto")
        .style("overflow", "visible");
      var g = svg.append("g");

      g.append("path").attr("d", ex.seeds.map(V.poly).join(""))
        .attr("fill", "#dedad0").attr("stroke", "none");
      g.append("g").selectAll("path").data(ex.seeds).join("path")
        .attr("d", V.poly).attr("fill", "none")
        .attr("stroke", "#9c9488").attr("stroke-width", 1)
        .attr("stroke-dasharray", "4 3");
      g.append("path").attr("d", V.SIMPLEX)
        .attr("fill", "none").attr("stroke", "#8d867a")
        .attr("stroke-width", 1.2);

      var qmax = d3.max(ex.marks.weights);
      g.append("g").selectAll("circle").data(ex.marks.q).join("circle")
        .attr("cx", function (p) { return V.toXY(p)[0]; })
        .attr("cy", function (p) { return V.toXY(p)[1]; })
        .attr("r", function (p, j) {
          return V.atomRadius(ex.marks.weights[j], qmax, 6.5, 3.5);
        })
        .attr("fill", V.INK);

      // Every other frame's atoms, small and faint, so the current frame's
      // sit on the path the whole family traces.
      var ghosts = [];
      ex.support.frames.forEach(function (f, j) {
        if (j !== i) f.atoms.forEach(function (a) { ghosts.push(a); });
      });
      g.append("g").selectAll("circle").data(ghosts).join("circle")
        .attr("cx", function (a) { return V.toXY(a)[0]; })
        .attr("cy", function (a) { return V.toXY(a)[1]; })
        .attr("r", 2).attr("fill", V.CLAY).attr("fill-opacity", 0.25);

      var f = ex.support.frames[i];
      var wmax = d3.max(f.weights);
      g.append("g").selectAll("circle").data(f.atoms).join("circle")
        .attr("cx", function (a) { return V.toXY(a)[0]; })
        .attr("cy", function (a) { return V.toXY(a)[1]; })
        .attr("r", function (a, j) {
          return V.atomRadius(f.weights[j], wmax, 7, 3.5);
        })
        .attr("fill", V.CLAY).attr("stroke", "#fdfcf9")
        .attr("stroke-width", 1.5);

      V.vertexLabels(g);
      return svg.node();
    }

    function dim(t) { return V.el("span", "riprvis-dim", t); }
    function text(t) { return V.el("span", null, " " + t); }

    // The frame's label with its diagnostics on one line, then one line per
    // atom: its coordinates and weight.
    function readoutOf(ex, i) {
      var f = ex.support.frames[i];
      var f3 = d3.format(".3f");
      var node = V.el("div", "riprvis-readout");
      var head = V.el("span", "riprvis-line");
      head.append(V.el("span", null, ex.support.labels[i]));
      if (f.kl != null) head.append(dim("  KL"), text(d3.format(".5f")(f.kl)));
      if (f.gap != null) head.append(dim("  gap"), text(d3.format(".2e")(f.gap)));
      node.append(head);
      f.atoms.forEach(function (a, j) {
        var line = V.el("span", "riprvis-line");
        line.append(
          dim("atom " + (j + 1)),
          text("(" + a.map(f3).join(", ") + ")"),
          dim(" w"), text(f3(f.weights[j]))
        );
        node.append(line);
      });
      return node;
    }

    return {
      renderValue: function (x) {
        if (loop) loop.stop();
        var ex = JSON.parse(x.data);
        var n = ex.support.frames.length;

        el.replaceChildren();
        el.classList.add("riprvis");
        var wrap = V.el("div", "riprvis-solo");
        var readoutWrap = V.el("div");
        var knobs = V.el("div", "riprvis-knobs");
        el.append(wrap, readoutWrap, knobs);

        var i = 0;
        var slider = V.rangeInput(n - 1, "frame", function (v) {
          i = v;
          redraw();
        });
        // The slider reads the frame's label rather than its index.
        var val = slider.el.querySelector(".riprvis-val");

        function redraw() {
          wrap.replaceChildren(drawFrame(ex, i));
          readoutWrap.replaceChildren(readoutOf(ex, i));
          val.textContent = ex.support.labels[i];
        }

        loop = V.makePlayLoop(400, function () {
          var v = slider.value >= n - 1 ? 0 : slider.value + 1;
          slider.value = v;
          i = v;
          redraw();
        });
        var play = V.toggleInput("play", function (checked) {
          if (checked) loop.start(); else loop.stop();
        });
        knobs.append(slider.el, play.el);

        redraw();
      },

      resize: function (width, height) {
        // The SVG is fixed-aspect via its viewBox and scales with the
        // container; nothing to recompute.
      }
    };
  }
});
