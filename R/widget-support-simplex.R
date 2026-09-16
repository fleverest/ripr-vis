#' The support, frame by frame: mixtures on the simplex under a slider
#'
#' A ternary plot of the two-simplex with the null region and the
#' alternative drawn as in [ripr_problem_simplex()], and over them the atoms
#' of one finite mixture per slider position, with atom area proportional to
#' weight. The other frames' atoms stay as faint ghosts, so the slider reads
#' as a path: how the support moves as whatever indexes the frames varies.
#' A readout gives the frame's label, each atom with its weight, and the
#' frame's KL and gap where the payload carries them. Play steps through
#' the frames.
#'
#' @param problem The payload from [ripr_problem_simplex_data()].
#' @param support The payload from [ripr_support_simplex_data()].
#' @inheritParams ripr_problem_simplex
#' @return An htmlwidget.
#' @export
ripr_support_simplex <- function(problem, support, width = NULL, height = NULL,
                         elementId = NULL) {
  check_problem_simplex(problem)
  stop_unless(
    is.list(support) &&
      all(c("frames", "labels") %in% names(support)) &&
      length(support$frames) > 0,
    "`support` must be a payload from ripr_support_simplex_data()"
  )
  ripr_widget(
    "ripr_support_simplex", c(problem, list(support = support)),
    default_height = 520,
    width = width, height = height, elementId = elementId
  )
}

#' @rdname riprvis-shiny
#' @export
riprSupportSimplexOutput <- function(outputId, width = "100%",
                                     height = "520px") {
  htmlwidgets::shinyWidgetOutput(
    outputId, "ripr_support_simplex", width, height,
    package = "riprvis"
  )
}

#' @rdname riprvis-shiny
#' @export
renderRiprSupportSimplex <- function(expr, env = parent.frame(),
                                     quoted = FALSE) {
  if (!quoted) expr <- substitute(expr)
  htmlwidgets::shinyRenderWidget(
    expr, riprSupportSimplexOutput, env, quoted = TRUE
  )
}
