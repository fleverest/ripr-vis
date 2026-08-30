#' Problem payload: the null region and the alternative's support
#'
#' Packages the geometry every widget draws for context: the convex parts
#' whose union is the null hypothesis, and the support of the alternative `Q`.
#' All three widgets ([ripr_problem()], [ripr_fit()], [ripr_certify()]) take
#' this as their first argument.
#'
#' @param null A `ripr::null_model()`, a list of `ripr` convex regions, **or**
#'   a plain list of vertex matrices (one per part, categories by vertices --
#'   each column a point of the simplex).
#' @param q The alternative's support: a categories-by-atoms matrix (or a
#'   single vector) of points of the simplex.
#' @param weights Mixture weights over the columns of `q`; equal by default.
#' @param title Short name for the example, used to namespace SVG defs when
#'   several widgets share a page.
#' @param part_labels Optional character vector labelling the parts of the
#'   null, e.g. the inequality each represents.
#' @return A list with elements `seeds` (per-part vertex lists), `marks` (the
#'   alternative's atoms and weights) and `labels`.
#' @examples
#' ripr_problem_data(
#'   null = list(cbind(c(.5, .5, 0), c(0, 1, 0), c(0, 0, 1))),
#'   q = c(0.4, 0.34, 0.26),
#'   part_labels = "example part"
#' )
#' @export
ripr_problem_data <- function(null, q, weights = NULL, title = "ripr",
                              part_labels = NULL) {
  seeds <- part_vertices(null)
  q <- as.matrix(q)
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
  list(
    seeds = seeds,
    marks = list(q = cols(q), weights = I(as.numeric(weights))),
    labels = list(
      title = as.character(title),
      parts = I(as.character(part_labels))
    )
  )
}

# The parts of a null as a list of vertex-column lists, from a null_model, a
# list of regions, or a list of plain matrices.
part_vertices <- function(null) {
  if (inherits(null, "S7_object")) {
    stop_unless(
      requireNamespace("ripr", quietly = TRUE),
      "`null` is an S7 object but the ripr package is not installed"
    )
    null <- ripr::parts(field(null, "region"))
  }
  stop_unless(
    is.list(null) && length(null) > 0,
    "`null` must be a null model, a list of regions or a list of matrices"
  )
  lapply(null, function(part) {
    v <- if (inherits(part, "S7_object")) field(part, "vertices") else part
    cols(as.matrix(v))
  })
}
