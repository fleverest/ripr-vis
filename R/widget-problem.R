#' The problem, stated: null region and alternative support
#'
#' A ternary plot of the two-simplex showing the null hypothesis -- a union of
#' convex parts, filled as one region with each part's boundary dashed -- and
#' the support of the alternative `Q`, with atom area proportional to weight.
#'
#' @param problem The payload from [ripr_problem_data()].
#' @param width,height,elementId Standard [htmlwidgets::createWidget()]
#'   arguments.
#' @return An htmlwidget.
#' @examples
#' ripr_problem(ripr_problem_data(
#'   null = list(
#'     cbind(c(0.5, 0.5, 0), c(0, 1, 0), c(0, 0, 1)),
#'     cbind(c(0.5, 0, 0.5), c(0, 1, 0), c(0, 0, 1))
#'   ),
#'   q = c(0.40, 0.34, 0.26)
#' ))
#' @export
ripr_problem <- function(problem, width = NULL, height = NULL,
                         elementId = NULL) {
  check_problem(problem)
  ripr_widget(
    "ripr_problem", problem,
    default_height = 460,
    width = width, height = height, elementId = elementId
  )
}

check_problem <- function(problem) {
  stop_unless(
    is.list(problem) &&
      all(c("seeds", "marks", "labels") %in% names(problem)),
    "`problem` must be a payload from ripr_problem_data()"
  )
}

#' Shiny bindings for riprvis widgets
#'
#' Output and render functions for using the riprvis widgets within Shiny
#' applications and interactive R Markdown documents.
#'
#' @param outputId Output variable to read from.
#' @param width,height Must be a valid CSS unit (like `"100%"`, `"400px"`) or
#'   a number, which will be coerced to a string and have `"px"` appended.
#' @param expr An expression that generates the widget.
#' @param env The environment in which to evaluate `expr`.
#' @param quoted Is `expr` a quoted expression (with `quote()`)?
#' @name riprvis-shiny
NULL

#' @rdname riprvis-shiny
#' @export
riprProblemOutput <- function(outputId, width = "100%", height = "460px") {
  htmlwidgets::shinyWidgetOutput(
    outputId, "ripr_problem", width, height,
    package = "riprvis"
  )
}

#' @rdname riprvis-shiny
#' @export
renderRiprProblem <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) expr <- substitute(expr)
  htmlwidgets::shinyRenderWidget(expr, riprProblemOutput, env, quoted = TRUE)
}
