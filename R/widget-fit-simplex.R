#' Fitting the projection: the Bernstein field and its diagnostics
#'
#' An animated view of a RIPr fit. The left panel draws the field
#' \eqn{G_i(\theta) = E_\theta[Q/P_i]} as filled contour bands over the
#' simplex -- red where it exceeds one, with the \eqn{G = 1} level set drawn
#' solid -- together with the support of the current iterate and the point
#' where the duality gap is attained. The right panels trace KL divergence,
#' gap, and the guaranteed log-growth rate \eqn{KL - \log(1 + gap)} across the
#' fit, with the current step picked out. A step slider and play toggle drive
#' the animation.
#'
#' @param problem The payload from [ripr_problem_simplex_data()].
#' @param fit The payload from [ripr_fit_simplex_data()].
#' @param lattice The payload from [ripr_lattice_data()]; the widget rebuilds
#'   the Bernstein basis from it in the browser.
#' @inheritParams ripr_problem_simplex
#' @return An htmlwidget.
#' @export
ripr_fit_simplex <- function(problem, fit, lattice, width = NULL, height = NULL,
                     elementId = NULL) {
  check_problem_simplex(problem)
  stop_unless(
    is.list(fit) && all(c("ratio", "support", "kl") %in% names(fit)),
    "`fit` must be a payload from ripr_fit_simplex_data()"
  )
  stop_unless(
    is.list(lattice) &&
      all(c("n", "outcomes", "log_choose") %in% names(lattice)),
    "`lattice` must be a payload from ripr_lattice_data()"
  )
  stop_unless(
    all(lengths(fit$ratio) == length(lattice$log_choose)),
    "each ratio vector in `fit` must have one entry per lattice outcome"
  )
  ripr_widget(
    "ripr_fit_simplex", c(problem, list(fit = fit, lattice = lattice)),
    default_height = 520,
    width = width, height = height, elementId = elementId
  )
}

#' @rdname riprvis-shiny
#' @export
riprFitSimplexOutput <- function(outputId, width = "100%", height = "520px") {
  htmlwidgets::shinyWidgetOutput(
    outputId, "ripr_fit_simplex", width, height,
    package = "riprvis"
  )
}

#' @rdname riprvis-shiny
#' @export
renderRiprFitSimplex <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) expr <- substitute(expr)
  htmlwidgets::shinyRenderWidget(
    expr, riprFitSimplexOutput, env, quoted = TRUE
  )
}
