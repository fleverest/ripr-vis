#' Certifying the fit: the branch-and-bound search
#'
#' An animated view of the certification that turns a fitted projection into
#' an e-variable. The left panel shows the evolving cell partition of the
#' selected cell's region -- repeated bisection, shading each live node by its
#' certified upper bound, greying nodes that have been pruned -- with an
#' optional zoom that follows the live cells as they shrink. The right panels
#' show the enclosure window \eqn{[\mathrm{incumbent}, \mathrm{bound}]}
#' closing (with its width on a log strip) and the search tree as depth
#' against birth iteration. Cells are searched independently, so a tab strip
#' selects which cell's search to watch; a slider and play toggle drive the
#' iteration.
#'
#' @param problem The payload from [ripr_problem_simplex_data()].
#' @param certify The payload from [ripr_certify_simplex_data()].
#' @inheritParams ripr_problem_simplex
#' @return An htmlwidget.
#' @export
ripr_certify_simplex <- function(problem, certify, width = NULL, height = NULL,
                         elementId = NULL) {
  check_problem_simplex(problem)
  stop_unless(
    is.list(certify) &&
      all(c("cells", "nodes", "upper", "lower") %in% names(certify)),
    "`certify` must be a payload from ripr_certify_simplex_data()"
  )
  parts <- vapply(certify$cells, function(cl) as.integer(cl$part), 1L)
  stop_unless(
    all(parts >= 1 & parts <= length(problem$seeds)),
    "`certify` references parts the `problem` payload does not have"
  )

  # One tab per cell, labelled by the part it came from; a triangulated part
  # contributes several cells, so those get a counter to stay distinct.
  part_labels <- as.character(problem$labels$parts)
  labels <- part_labels[parts]
  for (p in unique(parts[duplicated(parts)])) {
    at <- which(parts == p)
    labels[at] <- paste0(labels[at], " \u00b7 ", seq_along(at))
  }
  payload <- c(problem, certify)
  payload$labels$cells <- I(labels)

  ripr_widget(
    "ripr_certify_simplex", payload,
    default_height = 560,
    width = width, height = height, elementId = elementId
  )
}

#' @rdname riprvis-shiny
#' @export
riprCertifySimplexOutput <- function(outputId, width = "100%",
                                     height = "560px") {
  htmlwidgets::shinyWidgetOutput(
    outputId, "ripr_certify_simplex", width, height,
    package = "riprvis"
  )
}

#' @rdname riprvis-shiny
#' @export
renderRiprCertifySimplex <- function(expr, env = parent.frame(),
                                     quoted = FALSE) {
  if (!quoted) expr <- substitute(expr)
  htmlwidgets::shinyRenderWidget(
    expr, riprCertifySimplexOutput, env, quoted = TRUE
  )
}
