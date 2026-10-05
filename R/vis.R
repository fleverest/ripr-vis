# The interactive verbs: one htmlwidget, `riprvis`, whose view follows the
# payload's kind.

#' Draw a RIPr problem, fit, certification or comparison
#'
#' Interactive views of the objects \pkg{ripr} produces, as htmlwidgets. Each
#' verb accepts the ripr object directly, or a saved payload (see
#' [riprvis_payload()]), which draws without ripr installed. The family picks
#' the geometry: a three-category multinomial is drawn on the ternary simplex,
#' a two-dimensional Gaussian on the plane. [plot_problem()] and friends draw
#' the same views as static plots.
#'
#' * `vis_problem()`: the null's parts and the alternative's support.
#' * `vis_fit()`: the fit step by step: the field
#'   \eqn{G(\theta) = E_\theta[Q/P_i]}, the mixture's atoms, and the KL and
#'   gap traces. The fit must have been run with
#'   `ripr::ripr_control(snapshot = "all")` (or `"step"`).
#' * `vis_certify()`: the branch and bound of `ripr::certify_trace()`, cell by
#'   cell, with its enclosure window and search tree. Simplex only.
#' * `vis_compare()`: KL and gap traces of several runs against step or time.
#'
#' @param x For `vis_problem()`, a `ripr::null_model()`, or a ripr state or
#'   fit (whose null and alternative are used). For `vis_fit()`, a ripr state
#'   or `ripr::ripr_finish()` result. For `vis_certify()`, the result of
#'   `ripr::certify_trace()`. Or, for each, a payload of the matching kind.
#' @param alternative The alternative \eqn{Q}, whose atoms are marked. Taken
#'   from `x` where it carries one.
#' @param title Short name for the example, used to namespace SVG defs when
#'   several widgets share a page.
#' @param part_labels Labels for the null's parts, e.g. the inequality each
#'   represents. Defaults to "part 1", "part 2", ...
#' @param nx Grid columns for a planar fit's field.
#' @param runs A ripr state or fit, or a (named) list of them, or a payload.
#' @param labels Labels for the runs; defaults to `names(runs)`.
#' @param colour_by,dash_by Optional groupings of the runs, one entry each:
#'   runs sharing a level share a colour (at most five) or dash pattern.
#'   Dash levels take, in order, a dotted, a dashed, a solid and a dash-dot
#'   line, so put the level the eye should rest on third.
#' @param dashes Optional line pattern per dash level, each `"dotted"`,
#'   `"dashed"`, `"solid"` or `"dash-dot"`: one per level in level order, or
#'   named by level, in which case names for levels not present are ignored.
#'   Needs `dash_by`.
#' @param width,height,elementId Passed to [htmlwidgets::createWidget()].
#' @return An htmlwidget.
#' @examplesIf requireNamespace("ripr", quietly = TRUE)
#' family <- ripr::multinomial_family(n_trials = 10L, k = 3L)
#' part <- function(j) {
#'   v <- diag(3)
#'   v[1L, ] <- replace(numeric(3), c(1L, j), 0.5)
#'   ripr::simplex_region(vertices = v)
#' }
#' null <- ripr::null_model(family, part(2) | part(3))
#' state <- ripr::ripr_init(
#'   family(c(0.4, 0.34, 0.26)), null,
#'   control = ripr::ripr_control(snapshot = "all")
#' ) |>
#'   ripr::fw_step(times = 5L, record_gap = TRUE)
#' vis_problem(state)
#' vis_fit(state)
#' @name vis
NULL


#' @rdname vis
#' @export
vis_problem <- function(x, alternative = NULL, title = "ripr",
                        part_labels = NULL, width = NULL, height = NULL,
                        elementId = NULL) {
  p <- resolve_problem(x, alternative, title, part_labels)
  riprvis_widget(p, 460, width, height, elementId)
}

#' @rdname vis
#' @export
vis_fit <- function(x, title = "ripr", part_labels = NULL, nx = 112L,
                    width = NULL, height = NULL, elementId = NULL) {
  p <- resolve_fit(x, title, part_labels, nx)
  riprvis_widget(p, 520, width, height, elementId)
}

#' @rdname vis
#' @export
vis_certify <- function(x, alternative = NULL, title = "ripr",
                        part_labels = NULL, width = NULL, height = NULL,
                        elementId = NULL) {
  p <- resolve_certify(x, alternative, title, part_labels)
  riprvis_widget(p, 560, width, height, elementId)
}

#' @rdname vis
#' @export
vis_compare <- function(runs, labels = NULL, colour_by = NULL, dash_by = NULL,
                        dashes = NULL, width = NULL, height = NULL,
                        elementId = NULL) {
  p <- resolve_compare(runs, labels, colour_by, dash_by, dashes)
  riprvis_widget(p, 520, width, height, elementId)
}


#' The payload behind a riprvis widget
#'
#' The plain list a `vis_*()` widget draws. Save it with [saveRDS()] and pass
#' it back to the same `vis_*()` or `plot_*()` verb to draw it again, with or
#' without ripr installed.
#'
#' @param x A widget from a `vis_*()` verb, or a payload.
#' @return A `riprvis_payload`.
#' @examplesIf requireNamespace("ripr", quietly = TRUE)
#' family <- ripr::multinomial_family(n_trials = 10L, k = 3L)
#' null <- ripr::null_model(family, ripr::simplex_region(vertices = diag(3)))
#' p <- riprvis_payload(vis_problem(null))
#' vis_problem(p)
#' @export
riprvis_payload <- function(x) {
  if (is_payload(x)) return(x)
  p <- attr(x, "riprvis_payload")
  stop_unless(!is.null(p), "`x` must be a riprvis widget or payload")
  p
}


#' Shiny bindings for riprvis
#'
#' One output and one render function serve every `vis_*()` verb.
#'
#' @param outputId Output variable to read from.
#' @param width,height Size of the output, as valid CSS units.
#' @param expr An expression returning a `vis_*()` widget.
#' @param env The environment in which to evaluate `expr`.
#' @param quoted Is `expr` a quoted expression?
#' @return A Shiny output or render function.
#' @export
riprvisOutput <- function(outputId, width = "100%", height = "520px") {
  htmlwidgets::shinyWidgetOutput(
    outputId, "riprvis", width, height,
    package = "riprvis"
  )
}

#' @rdname riprvisOutput
#' @export
renderRiprvis <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) expr <- substitute(expr)
  htmlwidgets::shinyRenderWidget(expr, riprvisOutput, env, quoted = TRUE)
}


# --- Internals ------------------------------------------------------------------

# Each verb takes a ripr object or an already-built payload of its kind.
resolve_problem <- function(x, alternative, title, part_labels) {
  if (is_payload(x)) {
    stop_unless(
      is_payload(x, c("problem_simplex", "problem2d")),
      "`x` is a ", x$kind, " payload, not a problem"
    )
    return(x)
  }
  need_ripr()
  x <- unfinish(x)
  if (inherits(x, "S7_object") && "null" %in% S7::prop_names(x) &&
        !is_s7(x, ripr::null_model)) {
    if (is.null(alternative) && "alternative" %in% S7::prop_names(x)) {
      alternative <- x@alternative
    }
    x <- x@null
  }
  problem_payload(x, alternative, title, part_labels)
}

resolve_fit <- function(x, title, part_labels, nx) {
  if (is_payload(x)) {
    stop_unless(
      is_payload(x, c("fit_simplex", "fit2d")),
      "`x` is a ", x$kind, " payload, not a fit"
    )
    return(x)
  }
  fit_payload(x, title, part_labels, nx)
}

resolve_certify <- function(x, alternative, title, part_labels) {
  if (is_payload(x)) {
    stop_unless(
      is_payload(x, "certify_simplex"),
      "`x` is a ", x$kind, " payload, not a certification"
    )
    return(x)
  }
  certify_payload(x, alternative, title, part_labels)
}

resolve_compare <- function(runs, labels, colour_by, dash_by, dashes = NULL) {
  if (is_payload(runs)) {
    stop_unless(
      is_payload(runs, "compare"),
      "`runs` is a ", runs$kind, " payload, not a comparison"
    )
    return(runs)
  }
  compare_payload(runs, labels, colour_by, dash_by, dashes)
}

# The payload crosses as one pre-serialised string the JS side JSON.parse()s:
# htmlwidgets' own serialiser has different unboxing defaults and no
# `na = "null"`.
riprvis_widget <- function(payload, default_height, width, height,
                           elementId) {
  w <- htmlwidgets::createWidget(
    "riprvis",
    x = list(kind = payload$kind, data = payload_json(payload)),
    width = width,
    height = height,
    package = "riprvis",
    elementId = elementId,
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth = "100%",
      defaultHeight = default_height,
      viewer.fill = FALSE,
      browser.fill = FALSE,
      knitr.figure = FALSE,
      knitr.defaultWidth = "100%",
      knitr.defaultHeight = default_height,
      padding = 4
    )
  )
  attr(w, "riprvis_payload") <- payload
  w
}
