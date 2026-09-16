#' Support payload: one mixture's atoms per frame, for a slider
#'
#' Lines up several finite mixtures on the two-simplex -- the near-RIPr
#' supports of one problem as the trial count varies, say -- so
#' [ripr_support_simplex()] can step through them with a slider, each drawn
#' over the null region and the alternative. Family-agnostic: a frame is a
#' matrix of points of the simplex and their weights, however they were fit.
#'
#' @param frames A list, one entry per slider position, each a list with
#'   `atoms` (a categories-by-atoms matrix, or a vector for a single atom)
#'   and optionally `weights` (equal by default), `kl` and `gap` (read out
#'   beside the frame where present).
#' @param labels What each slider position reads, e.g. `"n = 7"`; default
#'   the names of `frames`, else `frame 1`, `frame 2`, ...
#' @return A list with elements `frames` (one entry per frame, each with
#'   `atoms` as a list of points, `weights`, `kl` and `gap`) and `labels`,
#'   shaped for [ripr_support_simplex()].
#' @examples
#' ripr_support_simplex_data(
#'   list(
#'     list(atoms = c(0.2, 0.5, 0.3)),
#'     list(atoms = cbind(c(0.2, 0.5, 0.3), c(0.1, 0.3, 0.6)), weights = c(0.7, 0.3))
#'   ),
#'   labels = c("n = 1", "n = 2")
#' )
#' @export
ripr_support_simplex_data <- function(frames, labels = NULL) {
  stop_unless(
    is.list(frames) && length(frames) > 0,
    "`frames` must be a non-empty list of frames"
  )
  if (is.null(labels)) {
    labels <- names(frames)
    if (is.null(labels)) labels <- rep("", length(frames))
    blank <- !nzchar(labels)
    labels[blank] <- paste("frame", which(blank))
  }
  stop_unless(
    length(labels) == length(frames),
    "`labels` must have one entry per frame"
  )
  out <- lapply(frames, function(f) {
    stop_unless(is.list(f) && !is.null(f$atoms), "each frame needs `atoms`")
    atoms <- as.matrix(f$atoms)
    stop_unless(
      nrow(atoms) == 3L,
      "`atoms` must have three rows, one per category"
    )
    weights <- if (is.null(f$weights)) {
      rep(1 / ncol(atoms), ncol(atoms))
    } else {
      as.numeric(f$weights)
    }
    stop_unless(
      length(weights) == ncol(atoms),
      "`weights` must have one entry per atom"
    )
    list(
      atoms = cols(atoms),
      weights = I(weights),
      kl = if (is.null(f$kl)) NA_real_ else as.numeric(f$kl),
      gap = if (is.null(f$gap)) NA_real_ else as.numeric(f$gap)
    )
  })
  names(out) <- NULL
  list(frames = out, labels = I(as.character(labels)))
}
