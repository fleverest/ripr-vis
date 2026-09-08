#' Fitting a planar projection: the field over the parameter plane
#'
#' The planar analogue of [ripr_fit_simplex()], for two-dimensional families
#' such as `ripr::gaussian_family()`. The left panel draws the field
#' \eqn{G_i(\theta) = E_\theta[Q/P_i]} as filled contour bands over the
#' parameter plane -- red where it exceeds one, with the \eqn{G = 1} level
#' set drawn solid -- the null's polyhedral parts dashed over it (receding
#' past the viewport along their rays), the support of the current iterate,
#' and the point where the duality gap is attained. The right panels trace
#' KL, gap and guaranteed log-growth across the fit. A step slider and play
#' toggle drive the animation. The viewport frames the null's vertices with
#' a margin, per [ripr_problem2d_data()].
#'
#' @param problem The payload from [ripr_problem2d_data()].
#' @param fit The payload from [ripr_fit2d_data()].
#' @inheritParams ripr_problem_simplex
#' @return An htmlwidget.
#' @export
ripr_fit2d <- function(problem, fit, width = NULL, height = NULL,
                       elementId = NULL) {
  check_problem2d(problem)
  stop_unless(
    is.list(fit) && all(c("field", "z", "kl") %in% names(fit)),
    "`fit` must be a payload from ripr_fit2d_data()"
  )
  stop_unless(
    all(lengths(fit$z) == fit$field$nx * fit$field$ny),
    "each field grid in `fit` must have nx * ny values"
  )
  ripr_widget(
    "ripr_fit2d", c(problem, list(fit = fit)),
    default_height = 520,
    width = width, height = height, elementId = elementId
  )
}

#' @rdname riprvis-shiny
#' @export
riprFit2dOutput <- function(outputId, width = "100%", height = "520px") {
  htmlwidgets::shinyWidgetOutput(
    outputId, "ripr_fit2d", width, height,
    package = "riprvis"
  )
}

#' @rdname riprvis-shiny
#' @export
renderRiprFit2d <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) expr <- substitute(expr)
  htmlwidgets::shinyRenderWidget(expr, riprFit2dOutput, env, quoted = TRUE)
}
