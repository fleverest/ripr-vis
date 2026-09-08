#' Planar problem payload: polyhedral null and alternative mean
#'
#' The planar analogue of [ripr_problem_simplex_data()], for two-dimensional
#' families whose parameter is a point of the plane (e.g. the mean of a
#' `ripr::gaussian_family()`). The null's parts are convex polyhedra given in
#' V-representation -- vertices plus optional recession rays and lineality
#' directions -- so unbounded parts draw correctly, clipped to the viewport.
#'
#' The payload also fixes the plotting extent: the bounding box of every
#' part's vertices and the alternative's atoms, padded on all sides by
#' `margin` times the box's larger span. Rays recede to the viewport edge,
#' so the vertices are what the eye needs framed; sizing to them keeps the
#' interesting geometry -- the pointy ends nearest the alternative -- filling
#' the view however far the null extends.
#'
#' @param null A `ripr::null_model()`, a list of `ripr` convex regions, **or**
#'   a plain list with one element per part, each either a `(2, nv)` vertex
#'   matrix or a `list(v = <2 x nv matrix>, r = <2 x nr matrix or NULL>,
#'   l = <2 x nl matrix or NULL>)` of vertices, rays and lineality
#'   directions.
#' @param q The alternative's support: a `(2, J)` matrix (or a single length-2
#'   vector) of parameter points.
#' @param weights Mixture weights over the columns of `q`; equal by default.
#' @param title Short name for the example, used to namespace SVG defs.
#' @param part_labels Optional character vector labelling the parts.
#' @param margin Fraction of the vertex bounding box's larger span added as
#'   padding on every side of the plot.
#' @return A list with elements `seeds` (per-part `v`/`r`/`l` generator
#'   lists), `marks`, `labels` and `extent` (`$x` and `$y` ranges), shaped
#'   for [ripr_problem2d()] and [ripr_fit2d()].
#' @examples
#' ripr_problem2d_data(
#'   null = list(list(
#'     v = cbind(c(1, 0), c(2, 0.5), c(2, -0.5)),
#'     r = cbind(c(1, 0))
#'   )),
#'   q = c(0, 0)
#' )
#' @export
ripr_problem2d_data <- function(null, q, weights = NULL, title = "ripr",
                                part_labels = NULL, margin = 0.2) {
  seeds <- part_generators(null)
  q <- as.matrix(q)
  stop_unless(nrow(q) == 2L, "`q` must be points of the plane (2 rows)")
  if (is.null(weights)) weights <- rep(1 / ncol(q), ncol(q))
  stop_unless(
    length(weights) == ncol(q),
    "`weights` must have one entry per column of `q`"
  )
  if (is.null(part_labels)) {
    part_labels <- paste("part", seq_along(seeds))
  }
  stop_unless(
    length(part_labels) == length(seeds),
    "`part_labels` must have one entry per part of the null"
  )
  stop_unless(
    is.numeric(margin) && length(margin) == 1L && margin >= 0,
    "`margin` must be a single non-negative number"
  )

  # The frame: every part's vertices and the alternative's atoms, padded by a
  # fraction of the larger span so the pointy ends sit clear of the edges.
  pts <- cbind(do.call(cbind, lapply(seeds, function(s) {
    do.call(cbind, s$v)
  })), q)
  xr <- range(pts[1L, ])
  yr <- range(pts[2L, ])
  pad <- margin * max(diff(xr), diff(yr), .Machine$double.eps)

  list(
    seeds = seeds,
    marks = list(q = cols(q), weights = I(as.numeric(weights))),
    labels = list(
      title = as.character(title),
      parts = I(as.character(part_labels))
    ),
    extent = list(
      x = I(xr + c(-pad, pad)),
      y = I(yr + c(-pad, pad))
    )
  )
}

# The parts of a planar null as lists of vertex/ray/lineality columns, from a
# null_model, a list of regions, or a list of plain matrices/lists.
part_generators <- function(null) {
  if (inherits(null, "S7_object")) {
    stop_unless(
      requireNamespace("ripr", quietly = TRUE),
      "`null` is an S7 object but the ripr package is not installed"
    )
    null <- ripr::parts(field(null, "region"))
  }
  stop_unless(
    is.list(null) && length(null) > 0,
    "`null` must be a null model, a list of regions or a list of generators"
  )
  lapply(null, function(part) {
    g <- if (inherits(part, "S7_object")) {
      field(part, "generators")
    } else if (is.matrix(part)) {
      list(v = part, r = NULL, l = NULL)
    } else {
      part
    }
    v <- as.matrix(g$v)
    stop_unless(
      nrow(v) == 2L && ncol(v) > 0,
      "each part needs at least one vertex in the plane (2 rows)"
    )
    block <- function(m) {
      if (is.null(m) || length(m) == 0) list() else cols(as.matrix(m))
    }
    list(v = cols(v), r = block(g$r), l = block(g$l))
  })
}
