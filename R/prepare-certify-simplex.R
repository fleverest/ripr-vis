#' Certification payload: the branch-and-bound record
#'
#' Packages the node table and per-iteration traces recorded by
#' `ripr::certify_trace()` for [ripr_certify_simplex()]. The search runs one
#' branch-and-bound tree per *cell* (a triangulated piece of a declared part
#' of the null; a part that is already a simplex is its own single cell), so
#' the node ids, traces and enclosure windows here are all per cell, with
#' `part` retained to say which declared part each cell came from.
#'
#' @param nodes The data frame returned by `ripr::certify_trace()`, carrying
#'   per-iteration bounds in its `"trace"` attribute, incumbents in
#'   `"incumbent_trace"` and the certificate in `"certificate"` -- **or** a
#'   plain list with elements `nodes` (columns `part`, `cell`, `id`, `parent`,
#'   `depth`, `born`, `retired`, `fate`, `upper`, `volume`, `vertices`),
#'   `trace`, `incumbent_trace` and `certificate` of the same shapes.
#' @param tol The tolerance the search was run with, echoed into the payload's
#'   certificate for display. Defaults to the certificate's own `tol` when it
#'   carries one.
#' @return A list with elements `cells` (one `list(part, vertices)` per cell,
#'   the vertices being the cell's root region), `nodes` (the node table as
#'   parallel arrays), `upper`/`lower` (per-cell bound and incumbent traces)
#'   and `certificate`, shaped for [ripr_certify_simplex()].
#' @export
ripr_certify_simplex_data <- function(nodes, tol = NULL) {
  tab <- if (is.data.frame(nodes)) nodes else nodes$nodes
  needed <- c(
    "part", "cell", "id", "parent", "depth", "born", "retired", "fate",
    "upper", "volume", "vertices"
  )
  missing <- setdiff(needed, names(tab))
  stop_unless(
    length(missing) == 0,
    "the node table lacks column(s): ", paste(missing, collapse = ", "),
    if ("subnull" %in% names(tab)) {
      " (a `subnull` column suggests output from a pre-cell ripr version)"
    }
  )
  traces <- field(nodes, "trace")
  incumbents <- field(nodes, "incumbent_trace")
  certificate <- field(nodes, "certificate")
  stop_unless(
    !is.null(traces) && !is.null(incumbents) && !is.null(certificate),
    "`nodes` must carry trace, incumbent_trace and certificate ",
    "(use ripr::certify_trace(), not ripr::certify())"
  )
  if (is.null(tol)) tol <- certificate$tol

  cell_ids <- sort(unique(tab$cell))
  stop_unless(
    length(traces) == length(cell_ids) &&
      length(incumbents) == length(cell_ids),
    "expected one bound trace and one incumbent trace per cell"
  )

  # Each cell's root node (id restarts at 1 per cell) is the cell itself.
  cells <- lapply(cell_ids, function(ci) {
    root <- which(tab$cell == ci & tab$id == 1L)[1L]
    stop_unless(!is.na(root), "cell ", ci, " has no root node (id 1)")
    list(
      part = tab$part[root],
      vertices = cols(as.matrix(tab$vertices[[root]]))
    )
  })

  cert <- list(
    sup_ub = certificate$sup_ub,
    sup_lb = certificate$sup_lb,
    iterations = I(certificate$iterations)
  )
  if (!is.null(tol)) cert$tol <- tol

  list(
    cells = cells,
    nodes = list(
      part = I(tab$part),
      cell = I(tab$cell),
      id = I(tab$id),
      parent = I(tab$parent),
      depth = I(tab$depth),
      born = I(tab$born),
      retired = I(tab$retired),
      fate = I(tab$fate),
      upper = I(tab$upper),
      volume = I(tab$volume),
      vertices = lapply(tab$vertices, cols)
    ),
    # `upper[[c]][t]` is cell c's certified bound after iteration t and
    # `lower[[c]][t]` the best value attained by then: the enclosure the
    # search may claim at t. `I()` throughout, or `auto_unbox` collapses a
    # single-iteration trace to a scalar where an array is expected.
    upper = lapply(traces, I),
    lower = lapply(incumbents, I),
    certificate = cert
  )
}
