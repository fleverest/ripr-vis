/* The one riprvis binding. Each payload names its view in `x.kind`; the views
 * live in lib/riprvis-views and register themselves on RiprVis.views. A new
 * kind replaces the previous view, stopping any play loop it left running. */
HTMLWidgets.widget({
  name: "riprvis",
  type: "output",

  factory: function (el, width, height) {
    var kind = null, view = null;

    return {
      renderValue: function (x) {
        if (x.kind !== kind) {
          if (view && view.destroy) view.destroy();
          el.replaceChildren();
          var make = window.RiprVis.views[x.kind];
          if (!make) throw new Error("riprvis: unknown view '" + x.kind + "'");
          kind = x.kind;
          view = make(el, width, height);
        }
        view.renderValue(x);
      },

      resize: function (width, height) {
        if (view) view.resize(width, height);
      }
    };
  }
});
