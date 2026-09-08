#' The planar problem, stated: polyhedral null and alternative mean
#'
#' A plot of the parameter plane showing a null hypothesis that is a union of
#' convex polyhedra -- unbounded parts recede past the viewport edge along
#' their rays -- and the alternative's support. The viewport frames the
#' parts' vertices with a margin (see [ripr_problem2d_data()]), so the
#' geometry nearest the alternative fills the view however far the null
#' extends.
#'
#' @param problem The payload from [ripr_problem2d_data()].
#' @inheritParams ripr_problem_simplex
#' @return An htmlwidget.
#' @examples
#' ripr_problem2d(ripr_problem2d_data(
#'   null = list(list(
#'     v = cbind(c(1, 0), c(2, 0.5), c(2, -0.5)),
#'     r = cbind(c(1, 0))
#'   )),
#'   q = c(0, 0)
#' ))
#' @export
ripr_problem2d <- function(problem, width = NULL, height = NULL,
                           elementId = NULL) {
  check_problem2d(problem)
  ripr_widget(
    "ripr_problem2d", problem,
    default_height = 460,
    width = width, height = height, elementId = elementId
  )
}

check_problem2d <- function(problem) {
  stop_unless(
    is.list(problem) &&
      all(c("seeds", "marks", "labels", "extent") %in% names(problem)),
    "`problem` must be a payload from ripr_problem2d_data()"
  )
}

#' @rdname riprvis-shiny
#' @export
riprProblem2dOutput <- function(outputId, width = "100%", height = "460px") {
  htmlwidgets::shinyWidgetOutput(
    outputId, "ripr_problem2d", width, height,
    package = "riprvis"
  )
}

#' @rdname riprvis-shiny
#' @export
renderRiprProblem2d <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) expr <- substitute(expr)
  htmlwidgets::shinyRenderWidget(expr, riprProblem2dOutput, env, quoted = TRUE)
}
