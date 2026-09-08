#' Comparing fits: KL, gap and growth against step or clock time
#'
#' Three stacked panels -- KL divergence, duality gap (log scale) and the
#' guaranteed growth rate \eqn{KL - \log(1 + gap)} -- with one line per
#' run, so two fitting algorithms, or two schedules of the same one, can be
#' read against each other. Tabs switch the horizontal axis between the step
#' count and the clock time `ripr` records in the trace; a toggle puts that
#' axis on a log scale. Hovering reads every run off at the cursor.
#'
#' @param compare The payload from [ripr_compare_data()].
#' @inheritParams ripr_problem_simplex
#' @return An htmlwidget.
#' @export
ripr_compare <- function(compare, width = NULL, height = NULL,
                         elementId = NULL) {
  stop_unless(
    is.list(compare) && all(c("runs", "has_time") %in% names(compare)) &&
      length(compare$runs) > 0,
    "`compare` must be a payload from ripr_compare_data()"
  )
  ripr_widget(
    "ripr_compare", compare,
    default_height = 520,
    width = width, height = height, elementId = elementId
  )
}

#' @rdname riprvis-shiny
#' @export
riprCompareOutput <- function(outputId, width = "100%", height = "520px") {
  htmlwidgets::shinyWidgetOutput(
    outputId, "ripr_compare", width, height,
    package = "riprvis"
  )
}

#' @rdname riprvis-shiny
#' @export
renderRiprCompare <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) expr <- substitute(expr)
  htmlwidgets::shinyRenderWidget(expr, riprCompareOutput, env, quoted = TRUE)
}
