#' Fit payload: the trace of a RIPr fit, one field per step
#'
#' \eqn{G_i(\theta) = E_\theta[Q/P_i]} is a polynomial in \eqn{\theta} whose
#' Bernstein coefficients on the simplex are exactly the pointwise ratios
#' \eqn{Q(y)/P_i(y)}, so that vector is all the widget needs to draw the field
#' at step \eqn{i}. This helper walks the fit's snapshots, computes the ratio
#' vector and support of every recorded iterate, and lines each up with the
#' `kl`/`gap` diagnostics on the matching trace row.
#'
#' The fit must have been run with `ripr::ripr_control(snapshot = "all")` (or
#' `"step"`), and `record_gap = TRUE` on the steps whose gap should appear.
#'
#' @param state A `ripr` fit state (the result of `ripr::ripr_init()` advanced
#'   by `ripr::fw_step()`/`ripr::em_step()`), **or** a plain list with
#'   elements `trace` (a data frame with columns `fw`, `lb`, `em`, `weight`,
#'   `phase`, `kl`, `gap` and a `gap_theta` list column) and `snapshots` (a
#'   list of `list(iters, phase, atoms, weights)` as `ripr` records them).
#' @param lattice The lattice payload from [ripr_lattice_data()].
#' @param q,weights The alternative, as in [ripr_problem_data()]: a
#'   categories-by-atoms matrix (or vector) and mixture weights over its
#'   columns. Used to evaluate `Q`'s pmf over the lattice. Alternatively pass
#'   `q_pmf` directly and leave these `NULL`.
#' @param q_pmf `Q`'s pmf over the lattice outcomes, if already computed.
#' @return A list with elements `ratio`, `support`, `kl`, `gap`, `gap_theta`
#'   and `phase`, one entry per snapshot, shaped for [ripr_fit()].
#' @export
ripr_fit_data <- function(state, lattice, q = NULL, weights = NULL,
                          q_pmf = NULL) {
  trace <- field(state, "trace")
  snapshots <- field(state, "snapshots")
  stop_unless(
    is.data.frame(trace) || is.list(trace),
    "`state` must carry a fit trace"
  )
  stop_unless(
    is.list(snapshots) && length(snapshots) > 0,
    "`state` carries no snapshots; run the fit with ",
    "ripr_control(snapshot = \"all\")"
  )
  if (is.null(q_pmf)) {
    stop_unless(!is.null(q), "supply either `q` (and `weights`) or `q_pmf`")
    q <- as.matrix(q)
    if (is.null(weights)) weights <- rep(1 / ncol(q), ncol(q))
    q_pmf <- lattice_mixture_pmf(lattice, q, weights)
  }
  stop_unless(
    length(q_pmf) == length(lattice$log_choose),
    "`q_pmf` must have one entry per lattice outcome"
  )

  # A snapshot records the iteration counters it was taken at; the trace row
  # with the same counters holds the diagnostics for the mixture that snapshot
  # describes. `gap` and `gap_theta` describe the mixture the row *produced*,
  # for every verb, so both line up with the snapshot on the same row.
  key <- function(v) paste(v[c("fw", "lb", "em", "weight")], collapse = "/")
  row <- match(
    vapply(snapshots, function(s) key(s$iters), ""),
    apply(trace[, c("fw", "lb", "em", "weight")], 1L, key)
  )
  stop_unless(
    !anyNA(row),
    "a snapshot has no matching trace row; was the trace subset or reordered?"
  )

  list(
    ratio = lapply(snapshots, function(s) {
      I(signif(
        q_pmf / lattice_mixture_pmf(
          lattice,
          do.call(cbind, s$atoms),
          unlist(s$weights)
        ),
        7
      ))
    }),
    support = lapply(snapshots, function(s) {
      list(
        atoms = cols(do.call(cbind, s$atoms)),
        weights = I(unlist(s$weights))
      )
    }),
    kl = I(trace$kl[row]),
    gap = I(trace$gap[row]),
    # list column, and NA on any row that did not sweep. Scalar NA rather than
    # NULL: with na = "null" it serialises to a JSON null the JS side can
    # test, where a NULL list entry would serialise as a truthy empty object.
    gap_theta = lapply(trace$gap_theta[row], function(v) {
      if (all(is.na(v))) NA else as.vector(v)
    }),
    phase = I(trace$phase[row])
  )
}
