#' Lattice payload for the Bernstein field
#'
#' The field \eqn{G_i(\theta) = E_\theta[Q/P_i]} drawn by [ripr_fit_simplex()]
#' is a polynomial whose Bernstein coefficients on the simplex are the pointwise
#' ratios \eqn{Q(y)/P_i(y)}, so the widget rebuilds the Bernstein basis in the
#' browser from the outcome lattice and each fitted iterate then crosses as one
#' coefficient vector rather than a grid of evaluations. This helper packages
#' the lattice once, for every step to share.
#'
#' @param family A `ripr::multinomial_family()` (any object whose
#'   `sample_space` property enumerates to an outcomes-by-categories count
#'   matrix), **or** a plain list with elements `n_trials` (integer) and
#'   `outcomes` (a matrix with one row per outcome and one column per
#'   category, each row summing to `n_trials`).
#' @return A list with elements `n` (number of trials), `outcomes` (a list of
#'   per-outcome count vectors) and `log_choose` (log multinomial
#'   coefficients, aligned with `outcomes`), shaped for [ripr_fit_simplex()].
#' @examplesIf requireNamespace("ripr", quietly = TRUE)
#' ripr_lattice_data(ripr::multinomial_family(n_trials = 4L, k = 3L))
#' @export
ripr_lattice_data <- function(family) {
  if (inherits(family, "S7_object")) {
    stop_unless(
      requireNamespace("ripr", quietly = TRUE),
      "`family` is an S7 object but the ripr package is not installed"
    )
    y <- as.matrix(ripr::enumerate_space(field(family, "sample_space")))
    n <- as.integer(field(family, "n_trials"))
  } else {
    stop_unless(
      is.list(family) && !is.null(family$outcomes) &&
        !is.null(family$n_trials),
      "`family` must be a ripr family or a list with $n_trials and $outcomes"
    )
    y <- as.matrix(family$outcomes)
    n <- as.integer(family$n_trials)
  }
  stop_unless(
    all(rowSums(y) == n),
    "every row of the outcome matrix must sum to n_trials"
  )
  list(
    n = n,
    outcomes = cols(t(y)),
    log_choose = I(lfactorial(n) - rowSums(lfactorial(y)))
  )
}

# The outcomes-by-categories count matrix back out of a lattice payload.
lattice_outcomes <- function(lattice) {
  do.call(rbind, lattice$outcomes)
}

# Multinomial pmf over the lattice at a single parameter vector.
lattice_pmf <- function(lattice, theta) {
  y <- lattice_outcomes(lattice)
  as.vector(exp(lattice$log_choose + y %*% log(pmax(theta, 1e-300))))
}

# Pmf of a finite mixture of multinomials: `atoms` is a categories-by-atoms
# matrix and `weights` a vector over its columns.
lattice_mixture_pmf <- function(lattice, atoms, weights) {
  atoms <- as.matrix(atoms)
  p <- vapply(
    seq_len(ncol(atoms)),
    function(j) lattice_pmf(lattice, atoms[, j]),
    numeric(length(lattice$log_choose))
  )
  as.vector(p %*% weights)
}
