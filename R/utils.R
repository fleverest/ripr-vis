# Shared internal helpers: payload serialisation and access to fields that may
# live on an S7 object (a property), a data frame (an attribute), or a plain
# list (an element), so every prepare_*() function accepts either the real
# `ripr` object or an equivalently shaped stand-in.

# Split a matrix into a list of its columns; the JSON side wants points as
# arrays of coordinates, and jsonlite serialises a matrix row-major otherwise.
cols <- function(m) {
  m <- as.matrix(m)
  lapply(seq_len(ncol(m)), function(i) m[, i])
}

# A property on an S7 object, an attribute on a data frame, or an element of a
# plain list -- whichever `x` is. Computed S7 properties are not stored as
# attributes, so S7 objects go through S7::prop().
field <- function(x, name) {
  if (inherits(x, "S7_object")) {
    if (!requireNamespace("S7", quietly = TRUE)) {
      stop("an S7 object was supplied but the S7 package is not installed")
    }
    return(S7::prop(x, name))
  }
  a <- attr(x, name, exact = TRUE)
  if (!is.null(a)) {
    return(a)
  }
  x[[name]]
}

# The one serialisation the JS side is written against. `na = "null"` matters:
# the default writes NA as the string "NA", which reads as a valid number on
# the other side. Callers are responsible for I()-wrapping every vector that
# must stay an array when it has length one.
payload_json <- function(x) {
  as.character(
    jsonlite::toJSON(x, auto_unbox = TRUE, digits = 12, na = "null")
  )
}

# Every widget ships its payload as a single pre-serialised string and the JS
# binding JSON.parse()s it, mirroring the ojs_define() path the visualisations
# were developed against; htmlwidgets' own serialiser has different unboxing
# defaults and no `na = "null"`.
ripr_widget <- function(name, payload, default_height, width, height,
                        elementId) {
  htmlwidgets::createWidget(
    name,
    x = list(data = payload_json(payload)),
    width = width,
    height = height,
    package = "riprvis",
    elementId = elementId,
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth = "100%",
      defaultHeight = default_height,
      viewer.fill = FALSE,
      browser.fill = FALSE,
      knitr.figure = FALSE,
      knitr.defaultWidth = "100%",
      knitr.defaultHeight = default_height,
      padding = 4
    )
  )
}

stop_unless <- function(ok, ...) {
  if (!isTRUE(ok)) stop(..., call. = FALSE)
}

# A snapshot records the iteration counters it was taken at; the trace row
# with the same counters holds the diagnostics for the mixture that snapshot
# describes. The `gap_after*` columns describe the mixture the row
# *produced*, for every verb, so both line up with the snapshot on the same
# row.
snapshot_rows <- function(trace, snapshots) {
  key <- function(v) paste(v[c("fw", "lb", "em", "weight")], collapse = "/")
  row <- match(
    vapply(snapshots, function(s) key(s$iters), ""),
    apply(trace[, c("fw", "lb", "em", "weight")], 1L, key)
  )
  stop_unless(
    !anyNA(row),
    "a snapshot has no matching trace row; was the trace subset or reordered?"
  )
  row
}

# The per-snapshot diagnostics every fit payload carries, whatever the family.
fit_diagnostics <- function(trace, snapshots, row) {
  list(
    support = lapply(snapshots, function(s) {
      list(
        atoms = cols(do.call(cbind, s$atoms)),
        weights = I(unlist(s$weights))
      )
    }),
    kl = I(trace$kl[row]),
    gap = I(trace$gap_after[row]),
    # list column, and NA on any row that did not sweep. Scalar NA rather than
    # NULL: with na = "null" it serialises to a JSON null the JS side can
    # test, where a NULL list entry would serialise as a truthy empty object.
    gap_theta = lapply(trace$gap_after_theta[row], function(v) {
      if (all(is.na(v))) NA else as.vector(v)
    }),
    phase = I(trace$phase[row])
  )
}
