#' Planar fit payload: the field of a RIPr fit over a grid of means
#'
#' The planar analogue of [ripr_fit_data()], for two-dimensional families
#' such as `ripr::gaussian_family()`. On the simplex the field
#' \eqn{G_i(\theta) = E_\theta[Q/P_i]} crosses to the browser as Bernstein
#' coefficients; a continuous family has no such finite basis, so this helper
#' evaluates the field in R over the plotting grid, one grid of values per
#' recorded snapshot, using the same identity and the same quadrature the fit
#' itself ran on: \eqn{G(\theta) = E_Q[P_\theta/P_i]}, an importance-weighted
#' sum over the engine's frozen nodes. The gap markers in the trace therefore
#' agree with the drawn field by construction.
#'
#' The grid covers `problem$extent` (see [ripr_problem2d_data()]) with `nx`
#' columns and square cells, so the field is computed exactly where the
#' widget will draw.
#'
#' @param state A `ripr` fit state (the result of `ripr::ripr_init()` with a
#'   quadrature engine, advanced by `ripr::fw_step()`/`ripr::em_step()`),
#'   **or** a plain list with elements `trace` and `snapshots` as in
#'   [ripr_fit_data()] plus an `engine` element `list(nodes = <M x 2
#'   matrix>, log_w = <length-M vector>)` of quadrature nodes and log
#'   weights.
#' @param problem The payload from [ripr_problem2d_data()]; fixes the grid
#'   extent.
#' @param sigma The family's known covariance. Defaults to the covariance of
#'   `state`'s own family when `state` is a `ripr` object, else the identity.
#' @param nx Grid columns; rows follow from the extent's aspect ratio.
#' @return A list with elements `field` (the grid: `nx`, `ny`, `x0`, `y0`,
#'   `dx`, `dy`), `z` (one length-`nx*ny` vector of `log10` field values per
#'   snapshot, row-major from the bottom-left), and the usual `support`,
#'   `kl`, `gap`, `gap_theta`, `phase` diagnostics, shaped for
#'   [ripr_fit2d()].
#' @export
ripr_fit2d_data <- function(state, problem, sigma = NULL, nx = 112L) {
  trace <- field(state, "trace")
  snapshots <- field(state, "snapshots")
  stop_unless(
    is.list(snapshots) && length(snapshots) > 0,
    "`state` carries no snapshots; run the fit with ",
    "ripr_control(snapshot = \"all\")"
  )
  engine <- field(state, "engine")
  stop_unless(
    !is.null(engine),
    "`state` must carry its quadrature engine (nodes and log weights)"
  )
  nodes <- as.matrix(field(engine, "nodes"))
  log_w <- as.numeric(field(engine, "log_w"))
  stop_unless(
    ncol(nodes) == 2L && nrow(nodes) == length(log_w),
    "the engine's nodes must be an M x 2 matrix aligned with its log weights"
  )
  if (is.null(sigma) && inherits(engine, "S7_object")) {
    sigma <- field(field(engine, "family"), "sigma")
  }
  if (is.null(sigma)) sigma <- diag(2)
  sigma <- as.matrix(sigma)
  stop_unless(
    identical(dim(sigma), c(2L, 2L)),
    "`sigma` must be a 2 x 2 covariance matrix"
  )
  row <- snapshot_rows(trace, snapshots)

  # The plotting grid: nx columns across the extent, square cells.
  # `problem$extent` carries `I()`-marked (AsIs) vectors so jsonlite ships
  # them unboxed to JS; `[` and arithmetic on an AsIs vector both keep that
  # class, so it is stripped here before x0/y0/dx/dy go into `fit$field` --
  # otherwise each crosses as a one-element array instead of a number and
  # every `f.x0 + x * f.dx` in the JS contour transform is `NaN`.
  ex <- lapply(problem$extent, as.numeric)
  x0 <- ex$x[1L]
  y0 <- ex$y[1L]
  dx <- (ex$x[2L] - x0) / (nx - 1L)
  ny <- max(2L, as.integer(round((ex$y[2L] - y0) / dx)) + 1L)
  dy <- (ex$y[2L] - y0) / (ny - 1L)
  theta <- rbind(
    rep(x0 + (seq_len(nx) - 1L) * dx, times = ny),
    rep(y0 + (seq_len(ny) - 1L) * dy, each = nx)
  )

  # log N(node_m; theta_c, sigma) for every node and grid point at once. The
  # quadratic form splits so the grid sweep is one matrix multiply; this is
  # the step-independent part, computed once.
  sigma_inv <- chol2inv(chol(sigma))
  const <- -log(2 * pi) - 0.5 * determinant(sigma)$modulus[1L]
  ns <- nodes %*% sigma_inv
  ld <- ns %*% theta
  ld <- ld - 0.5 * rowSums(ns * nodes) + const
  ld <- sweep(ld, 2L, 0.5 * colSums(theta * (sigma_inv %*% theta)))

  # Per step only the mixture P_i at the nodes changes: G over the whole grid
  # is then one log-sum-exp reduction of ld - log_p + log_w down the columns.
  log_p_at_nodes <- function(atoms, weights) {
    a <- ns %*% atoms
    a <- a - 0.5 * rowSums(ns * nodes) + const
    a <- sweep(a, 2L, 0.5 * colSums(atoms * (sigma_inv %*% atoms)))
    row_lse(sweep(a, 2L, log(weights), "+"))
  }

  z <- lapply(snapshots, function(s) {
    log_p <- log_p_at_nodes(do.call(cbind, s$atoms), unlist(s$weights))
    log_g <- col_lse(ld - log_p + log_w)
    I(signif(pmin(pmax(log_g / log(10), -6), 6), 4))
  })

  c(
    list(
      field = list(nx = nx, ny = ny, x0 = x0, y0 = y0, dx = dx, dy = dy),
      z = z
    ),
    fit_diagnostics(trace, snapshots, row)
  )
}

col_lse <- function(x) {
  m <- apply(x, 2L, max)
  m + log(colSums(exp(sweep(x, 2L, m))))
}

row_lse <- function(x) {
  m <- apply(x, 1L, max)
  m + log(rowSums(exp(x - m)))
}
