#' Comparison payload: the diagnostics of several fits, side by side
#'
#' Lines up the KL, gap and guaranteed growth traces of several fits of
#' the same problem -- Frank--Wolfe against Li--Barron, say, or one step
#' schedule against another -- so [ripr_compare()] can draw them against
#' each other by step and by clock time. Family-agnostic: only the trace is
#' read, so simplex and planar fits alike (and a saved trace with no `ripr`
#' in sight) can be compared.
#'
#' Each run's rows are its trace rows in order. `step` counts oracle steps,
#' `fw + lb` from the trace, so the initial state is step 0 and the rows an
#' EM or weight sweep adds between two oracle steps share the step of the
#' one they refine: a fully-corrective run's many cheap weight rows do not
#' inflate its step count. A trace without those columns is numbered by
#' row. The clock time of a row is the cumulative `elapsed` up to it --
#' `ripr` records the wall-clock seconds each row cost, the init row
#' included -- so a run's time axis is `cumsum(trace$elapsed)`. A trace
#' without an `elapsed` column has no time and is left out of the time view.
#'
#' With a handful of runs each gets its own colour. Two crossed factors --
#' four direction sets by three step-size rules, say -- are better told
#' apart by two encodings: `colour_by` colours the runs by one factor and
#' `dash_by` patterns the lines by the other, and the legend then lists the
#' levels of each rather than every run. Dash levels take, in order, a
#' dotted, a dashed, a solid and a dash-dot line; put the rule the eye
#' should rest on third.
#'
#' @param runs A list of fits, each a `ripr` fit state, a plain list with a
#'   `trace` element (such as the result of `ripr::ripr_finish()`), or a
#'   trace data frame with columns `phase`, `kl` and `gap` and, for the time
#'   view, `elapsed`. Names, if any, label the runs.
#' @param labels Run labels; default the names of `runs`, else `run 1`,
#'   `run 2`, ...
#' @param colour_by,dash_by Optional factors over the runs (one entry each,
#'   in the order of `runs`) giving the level each run is coloured, or
#'   dashed, by. Levels are taken in order of first appearance, or a
#'   factor's own level order. At most five colour levels are told apart.
#' @return A list with elements `runs` (one entry per run, each with `label`,
#'   `step`, `phase`, `kl`, `gap`, `time` vectors and `colour`/`dash` level
#'   indices), `legend` (the level names behind those indices, or `NULL`
#'   where the runs are not grouped) and `has_time`, shaped for
#'   [ripr_compare()].
#' @examples
#' \dontrun{
#' set.seed(1L)
#' fw <- ripr::ripr_init(Q, null, record_gap = TRUE) |>
#'   ripr::fw_step(times = 20L)
#' set.seed(1L)
#' lb <- ripr::ripr_init(Q, null, record_gap = TRUE) |>
#'   ripr::lb_step(times = 20L, record_gap = TRUE)
#' ripr_compare(ripr_compare_data(list("Frank–Wolfe" = fw, "Li–Barron" = lb)))
#' }
#' @export
ripr_compare_data <- function(runs, labels = NULL, colour_by = NULL,
                              dash_by = NULL) {
  stop_unless(is.list(runs) && length(runs) > 0, "`runs` must be a list of fits")
  if (is.data.frame(runs) || !is.null(run_trace_or_null(runs))) {
    runs <- list(runs)
  }
  if (is.null(labels)) {
    labels <- names(runs)
    if (is.null(labels)) labels <- rep("", length(runs))
    blank <- !nzchar(labels)
    labels[blank] <- paste("run", which(blank))
  }
  stop_unless(
    length(labels) == length(runs),
    "`labels` must have one entry per run"
  )
  colour <- run_levels(colour_by, length(runs), "colour_by")
  dash <- run_levels(dash_by, length(runs), "dash_by")
  n_colours <- if (is.null(colour)) length(runs) else length(colour$levels)
  if (n_colours > 5L) {
    warning(
      "ripr_compare() distinguishes at most five colours; ",
      "the rest are drawn in grey",
      call. = FALSE
    )
  }

  out <- Map(function(run, label, i) {
    trace <- run_trace(run)
    stop_unless(
      is.data.frame(trace) &&
        all(c("phase", "kl", "gap_after") %in% names(trace)),
      "each run needs a trace with `phase`, `kl` and `gap_after` columns"
    )
    n <- nrow(trace)
    stop_unless(n > 0L, "a run has an empty trace")
    time <- if ("elapsed" %in% names(trace)) {
      cumsum(as.numeric(trace$elapsed))
    } else {
      rep(NA_real_, n)
    }
    phase <- as.character(trace$phase)
    step <- if (all(c("fw", "lb") %in% names(trace))) {
      as.integer(trace$fw) + as.integer(trace$lb)
    } else {
      seq_len(n) - as.integer(identical(phase[1L], "init"))
    }
    list(
      label = label,
      step = I(step),
      phase = I(phase),
      kl = I(as.numeric(trace$kl)),
      gap = I(as.numeric(trace$gap_after)),
      time = I(time),
      colour = if (is.null(colour)) i else colour$index[i],
      dash = if (is.null(dash)) 1L else dash$index[i]
    )
  }, runs, labels, seq_along(runs))
  names(out) <- NULL

  # Only the groupings that exist: a NULL element would serialise as an
  # empty object, which is truthy on the JS side.
  legend <- list()
  if (!is.null(colour)) legend$colour <- I(colour$levels)
  if (!is.null(dash)) legend$dash <- I(dash$levels)
  list(
    runs = out,
    legend = legend,
    has_time = any(vapply(out, function(r) any(!is.na(r$time)), TRUE))
  )
}

# A grouping factor over the runs as level names plus a 1-based index per
# run, or NULL when there is none.
run_levels <- function(by, n, what) {
  if (is.null(by)) return(NULL)
  stop_unless(length(by) == n, "`", what, "` must have one entry per run")
  f <- if (is.factor(by)) droplevels(by) else factor(by, levels = unique(by))
  list(levels = as.character(levels(f)), index = as.integer(f))
}

# A run's trace: the object itself when it is one, else its `trace` field.
run_trace <- function(run) {
  trace <- run_trace_or_null(run)
  stop_unless(!is.null(trace), "a run must be a trace or carry one")
  trace
}

run_trace_or_null <- function(run) {
  if (is.data.frame(run)) return(run)
  if (inherits(run, "S7_object") || is.list(run)) {
    return(tryCatch(field(run, "trace"), error = function(e) NULL))
  }
  NULL
}
