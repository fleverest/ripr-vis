# Payloads: the plain lists every view draws from, built from ripr objects.
#
# A payload is a list with a `kind` naming its view ("problem_simplex",
# "problem2d", "fit_simplex", "fit2d", "certify_simplex" or "compare"),
# classed `riprvis_payload`. The widgets and the static plots both draw from
# one, so a saved payload renders without ripr installed.

new_payload <- function(kind, ...) {
  structure(list(kind = kind, ...), class = "riprvis_payload")
}

is_payload <- function(x, kinds = NULL) {
  inherits(x, "riprvis_payload") && (is.null(kinds) || x$kind %in% kinds)
}

#' @export
print.riprvis_payload <- function(x, ...) {
  cat("<riprvis_payload> ", x$kind, "\n", sep = "")
  invisible(x)
}

# Which view a family's parameter space gets: the ternary simplex for a
# three-category multinomial, the plane for a two-dimensional Gaussian.
geometry <- function(family) {
  if (is_s7(family, ripr::multinomial_family) && family@k == 3L) {
    return("simplex")
  }
  if (is_s7(family, ripr::gaussian_family) && family@d == 2L) {
    return("plane")
  }
  stop(
    "riprvis draws three-category multinomial and two-dimensional Gaussian ",
    "families only",
    call. = FALSE
  )
}

# --- The problem --------------------------------------------------------------

# The null's parts and the alternative's support, in the geometry the family
# implies. `alternative` may be NULL, which draws no support.
problem_payload <- function(null, alternative, title, part_labels,
                            margin = 0.2) {
  need_ripr()
  stop_unless(is_s7(null, ripr::null_model), "`null` must be a ripr null_model")
  geom <- geometry(null@family)
  parts <- ripr::parts(null@region)
  if (is.null(part_labels)) part_labels <- paste("part", seq_along(parts))
  stop_unless(
    length(part_labels) == length(parts),
    "`part_labels` must have one entry per part of the null"
  )
  marks <- alternative_marks(alternative)
  labels <- list(
    title = as.character(title),
    parts = I(as.character(part_labels))
  )

  if (geom == "simplex") {
    seeds <- lapply(parts, function(p) {
      stop_unless(
        ripr::is_bounded(p),
        "every part of a simplex null must be bounded to be drawn"
      )
      rows(p@generators$v)
    })
    return(new_payload(
      "problem_simplex",
      seeds = seeds, marks = marks, labels = labels
    ))
  }

  seeds <- lapply(parts, function(p) {
    g <- p@generators
    block <- function(m) if (nrow(m) == 0L) list() else rows(m)
    list(v = rows(g$v), r = block(g$r), l = block(g$l))
  })
  # The frame: every part's vertices and the alternative's atoms, padded by a
  # fraction of the larger span so the pointy ends sit clear of the edges.
  pts <- rbind(
    do.call(rbind, lapply(parts, function(p) p@generators$v)),
    unrows(marks$q)
  )
  xr <- range(pts[, 1L])
  yr <- range(pts[, 2L])
  pad <- margin * max(diff(xr), diff(yr), .Machine$double.eps)
  new_payload(
    "problem2d",
    seeds = seeds, marks = marks, labels = labels,
    extent = list(x = I(xr + c(-pad, pad)), y = I(yr + c(-pad, pad)))
  )
}

# The alternative's atoms and weights when its mixing measure is discrete;
# nothing otherwise (a Dirichlet alternative has no atoms to mark).
alternative_marks <- function(alternative) {
  none <- list(q = list(), weights = I(numeric(0)))
  if (is.null(alternative) || !is_s7(alternative, ripr::mixture)) {
    return(none)
  }
  mixing <- alternative@mixing
  if (!is_s7(mixing, ripr::discrete_dist)) return(none)
  list(
    q = rows(ripr::atoms(mixing)),
    weights = I(as.numeric(stats::weights(mixing)))
  )
}

# --- The fit ------------------------------------------------------------------

fit_payload <- function(state, title, part_labels, nx = 112L) {
  need_ripr()
  state <- unfinish(state)
  stop_unless(
    inherits(state, "S7_object") && "snapshots" %in% S7::prop_names(state),
    "`state` must be a ripr state (from ripr_init() and the step verbs) ",
    "or a ripr_finish() result"
  )
  snapshots <- state@snapshots
  stop_unless(
    length(snapshots) > 0L,
    "the state carries no snapshots; run the fit with ",
    "ripr_control(snapshot = \"all\")"
  )
  problem <- problem_payload(
    state@null, state@alternative, title, part_labels
  )
  diagnostics <- fit_diagnostics(state@trace, snapshots)
  family <- state@null@family

  if (problem$kind == "problem_simplex") {
    lattice <- lattice_payload(family)
    y <- unrows(lattice$outcomes)
    q_pmf <- exp(ripr::log_density(state@alternative, y))
    ratio <- lapply(snapshots, function(s) {
      p_pmf <- exp(ripr::log_density(family(s$mixing), y))
      I(signif(ifelse(q_pmf == 0, 0, q_pmf / p_pmf), 7))
    })
    fit <- c(list(ratio = ratio), diagnostics)
    return(do.call(new_payload, c(
      list("fit_simplex"), problem[-1L], list(fit = fit, lattice = lattice)
    )))
  }

  fit <- c(field2d(state, problem$extent, nx), diagnostics)
  do.call(new_payload, c(list("fit2d"), problem[-1L], list(fit = fit)))
}

# The outcome lattice: the multinomial's Bernstein basis is rebuilt in the
# browser from it, so each step crosses as one ratio vector.
lattice_payload <- function(family) {
  y <- as.matrix(ripr::enumerate_space(family@sample_space))
  n <- as.integer(family@n_trials)
  list(
    n = n,
    outcomes = rows(y),
    log_choose = I(lfactorial(n) - rowSums(lfactorial(y)))
  )
}

# G over a grid of means, by the identity G(theta) = E_Q[P_theta / P_i]
# under the quadrature the fit itself ran on, so the field agrees with the
# fit's gaps by construction. Grid: `nx` columns across the extent, square
# cells; `z` is log10 G, row-major from the bottom left.
field2d <- function(state, extent, nx) {
  engine <- state@engine
  family <- state@null@family
  ll <- ripr::compile_loglik(family, engine@nodes)
  ex <- lapply(extent, as.numeric)
  x0 <- ex$x[1L]
  y0 <- ex$y[1L]
  dx <- (ex$x[2L] - x0) / (nx - 1L)
  ny <- max(2L, as.integer(round((ex$y[2L] - y0) / dx)) + 1L)
  dy <- (ex$y[2L] - y0) / (ny - 1L)
  theta <- cbind(
    rep(x0 + (seq_len(nx) - 1L) * dx, times = ny),
    rep(y0 + (seq_len(ny) - 1L) * dy, each = nx)
  )
  ld <- ll(theta)
  z <- lapply(state@snapshots, function(s) {
    log_p <- row_lse(sweep(
      ll(ripr::atoms(s$mixing)), 2L, log(stats::weights(s$mixing)), "+"
    ))
    log_g <- col_lse(ld - log_p + engine@log_w)
    I(signif(pmin(pmax(log_g / log(10), -6), 6), 4))
  })
  list(field = list(nx = nx, ny = ny, x0 = x0, y0 = y0, dx = dx, dy = dy),
       z = z)
}

# Per-snapshot diagnostics from the trace row the snapshot was taken at. The
# `gap_after*` columns describe the mixture the row produced, which is the
# mixture the snapshot holds.
fit_diagnostics <- function(trace, snapshots) {
  row <- match(
    vapply(snapshots, function(s) as.integer(s$step), 0L),
    as.integer(trace$step)
  )
  stop_unless(!anyNA(row), "a snapshot has no matching trace row")
  list(
    support = lapply(snapshots, function(s) {
      list(
        atoms = rows(ripr::atoms(s$mixing)),
        weights = I(as.numeric(stats::weights(s$mixing)))
      )
    }),
    kl = I(trace$kl[row]),
    gap = I(trace$gap_after[row]),
    # NA rather than NULL on rows that did not sweep: with na = "null" it
    # crosses as a JSON null, where a NULL entry would become a truthy {}.
    gap_theta = lapply(trace$gap_after_theta[row], function(v) {
      if (all(is.na(v))) NA else as.vector(v)
    }),
    phase = I(trace$phase[row])
  )
}

# --- The certification --------------------------------------------------------

certify_payload <- function(nodes, alternative, title, part_labels) {
  need_ripr()
  certificate <- attr(nodes, "certificate")
  stop_unless(
    is.data.frame(nodes) && is_s7(certificate, ripr::ripr_certificate),
    "`nodes` must be the result of ripr::certify_trace()"
  )
  problem <- problem_payload(
    certificate@null, alternative, title, part_labels
  )
  stop_unless(
    problem$kind == "problem_simplex",
    "certification is drawn on the simplex only"
  )
  traces <- attr(nodes, "trace")
  incumbents <- attr(nodes, "incumbent_trace")
  cell_ids <- sort(unique(nodes$cell))

  # Each cell's root node (id restarts at 1 per cell) is the cell itself.
  cells <- lapply(cell_ids, function(ci) {
    root <- which(nodes$cell == ci & nodes$id == 1L)[1L]
    list(part = nodes$part[root], vertices = rows(nodes$vertices[[root]]))
  })

  # One tab per cell, labelled by its part; a triangulated part contributes
  # several cells, so those get a counter to stay distinct.
  parts <- vapply(cells, function(cl) as.integer(cl$part), 1L)
  cell_labels <- as.character(problem$labels$parts)[parts]
  for (p in unique(parts[duplicated(parts)])) {
    at <- which(parts == p)
    cell_labels[at] <- paste0(cell_labels[at], " \u00b7 ", seq_along(at))
  }
  labels <- problem$labels
  labels$cells <- I(cell_labels)

  new_payload(
    "certify_simplex",
    seeds = problem$seeds, marks = problem$marks, labels = labels,
    cells = cells,
    nodes = list(
      part = I(nodes$part), cell = I(nodes$cell), id = I(nodes$id),
      parent = I(nodes$parent), depth = I(nodes$depth),
      born = I(nodes$born), retired = I(nodes$retired),
      fate = I(nodes$fate), upper = I(nodes$upper),
      volume = I(nodes$volume), vertices = lapply(nodes$vertices, rows)
    ),
    # `upper[[c]][t]` is cell c's certified bound after iteration t and
    # `lower[[c]][t]` the best value attained by then.
    upper = lapply(traces, I),
    lower = lapply(incumbents, I),
    certificate = list(
      sup_ub = certificate@sup_ub,
      sup_lb = certificate@sup_lb,
      iterations = I(certificate@iterations)
    )
  )
}

# --- Comparing runs -------------------------------------------------------------

compare_payload <- function(runs, labels, colour_by, dash_by) {
  if (is.data.frame(runs) || inherits(runs, "S7_object")) runs <- list(runs)
  stop_unless(
    is.list(runs) && length(runs) > 0L,
    "`runs` must be a ripr state or fit, or a list of them"
  )
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
      "riprvis distinguishes at most five colours; the rest are drawn in grey",
      call. = FALSE
    )
  }

  out <- Map(function(run, label, i) {
    trace <- run_trace(run)
    n <- nrow(trace)
    stop_unless(n > 0L, "a run has an empty trace")
    phase <- as.character(trace$phase)
    list(
      label = label,
      step = I(cumsum(phase %in% c("fw", "lb"))),
      phase = I(phase),
      kl = I(as.numeric(trace$kl)),
      gap = I(as.numeric(trace$gap_after)),
      time = I(cumsum(as.numeric(trace$elapsed))),
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
  new_payload(
    "compare",
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

# A ripr_finish() result's state; anything else as is.
unfinish <- function(x) {
  if (inherits(x, "S7_object") && "state" %in% S7::prop_names(x)) x@state else x
}

# A run's trace: a ripr state's, a finished fit's state's, or a trace data
# frame as is.
run_trace <- function(run) {
  run <- unfinish(run)
  if (inherits(run, "S7_object") && "trace" %in% S7::prop_names(run)) {
    run <- run@trace
  }
  stop_unless(
    is.data.frame(run) &&
      all(c("phase", "kl", "gap_after", "elapsed") %in% names(run)),
    "each run must be a ripr state, a ripr_finish() result or a trace"
  )
  run
}
