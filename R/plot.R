# Static plots: the same views as the widgets, drawn with base graphics for
# print. Each draws one frame on the current device and returns its payload
# invisibly.

#' Static plots of a RIPr problem, fit, certification or comparison
#'
#' Base-graphics counterparts of the [vis] widgets, for publications: each
#' draws one frame on the current device, so `pdf()`, `svg()`, `png()`,
#' `par(mfrow = )` and `layout()` all apply. They take the same inputs as the
#' `vis_*()` verbs, including a saved payload.
#'
#' * `plot_problem()`: the null's parts and the alternative's support.
#' * `plot_fit()`: the field \eqn{G(\theta) = E_\theta[Q/P_i]} at one step,
#'   with its \eqn{G = 1} contour, the mixture's atoms and, where recorded,
#'   the point attaining the gap.
#' * `plot_certify()`: the branch-and-bound partition at one iteration, over
#'   every cell: live cells shaded by their upper bound, pruned cells grey.
#' * `plot_compare()`: KL and gap traces of several runs.
#'
#' @inheritParams vis
#' @param step Which recorded step to draw, counting snapshots from 1;
#'   defaults to the last.
#' @param iteration Which branch-and-bound iteration to draw; defaults to the
#'   last.
#' @param y Which traces `plot_compare()` draws: `"both"` (two panels),
#'   `"kl"` or `"gap"`.
#' @param x What `plot_compare()` puts on the horizontal axis: oracle steps
#'   or cumulative seconds.
#' @param legend Draw a legend?
#' @return The payload drawn, invisibly.
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
#' op <- par(mfrow = c(1, 2))
#' plot_problem(state)
#' plot_fit(state)
#' par(op)
#' plot_compare(state)
#' @name plots
NULL

# The deck's palette, shared with the widgets.
INK <- "#1f1d1a"
CLAY <- "#b8452f"
COOL <- "#2c6e8f"
EDGE <- "#8d867a"
PART_FILL <- "#dedad0"
PART_EDGE <- "#9c9488"
RUN_COLOURS <- c("#2a6aa8", "#b8452f", "#2f8f6f", "#8455b0", "#b5820a")
# The widget's dash patterns (dotted, dashed, solid, dash-dot) as R `lty`s.
RUN_DASHES <- c(
  dotted = "13", dashed = "63", solid = "solid", "dash-dot" = "8323"
)

#' @rdname plots
#' @export
plot_problem <- function(x, alternative = NULL, title = "ripr",
                         part_labels = NULL) {
  p <- resolve_problem(x, alternative, title, part_labels)
  if (p$kind == "problem_simplex") {
    ternary_frame()
    draw_parts_simplex(p$seeds, fill = TRUE)
    ternary_outline()
    draw_atoms(p$marks$q, p$marks$weights, to_xy = ternary_xy)
    ternary_labels()
  } else {
    op <- plane_frame(p$extent)
    on.exit({
      graphics::box()
      graphics::par(op)
    })
    draw_parts_plane(p$seeds, p$extent, fill = TRUE)
    draw_atoms(p$marks$q, p$marks$weights, to_xy = identity_xy)
  }
  invisible(p)
}

#' @rdname plots
#' @export
plot_fit <- function(x, step = NULL, title = "ripr", part_labels = NULL,
                     nx = 112L) {
  p <- resolve_fit(x, title, part_labels, nx)
  n_steps <- length(p$fit$support)
  if (is.null(step)) step <- n_steps
  stop_unless(
    length(step) == 1L && step >= 1L && step <= n_steps,
    "`step` must be between 1 and ", n_steps
  )
  support <- p$fit$support[[step]]
  target <- p$fit$gap_theta[[step]]

  if (p$kind == "fit_simplex") {
    ternary_frame()
    simplex_field(p$lattice, p$fit$ratio[[step]])
    draw_parts_simplex(p$seeds, fill = FALSE, border = INK)
    ternary_outline()
    to_xy <- ternary_xy
  } else {
    op <- plane_frame(p$extent)
    on.exit({
      graphics::box()
      graphics::par(op)
    })
    plane_field(p$fit$field, p$fit$z[[step]])
    draw_parts_plane(p$seeds, p$extent, fill = FALSE, border = INK)
    # Base graphics clips to the region plot.new() set, before the frame
    # was fitted to the extent, so marks off the extent are dropped here.
    to_xy <- function(b) {
      b <- as_points(b)
      ok <- b[, 1L] >= p$extent$x[1L] & b[, 1L] <= p$extent$x[2L] &
        b[, 2L] >= p$extent$y[1L] & b[, 2L] <= p$extent$y[2L]
      b[ok, , drop = FALSE]
    }
  }
  draw_atoms(
    support$atoms, support$weights, to_xy = to_xy,
    pch = 1, col = COOL, lwd = 1.4
  )
  draw_atoms(p$marks$q, NULL, to_xy = to_xy, cex = 0.7)
  if (length(target) > 1L || !is.na(target[[1L]])) {
    xy <- to_xy(as_points(as.numeric(unlist(target))))
    graphics::points(xy, pch = 1, cex = 2, col = CLAY, lwd = 2)
  }
  if (p$kind == "fit_simplex") ternary_labels()
  invisible(p)
}

#' @rdname plots
#' @export
plot_certify <- function(x, iteration = NULL, alternative = NULL,
                         title = "ripr", part_labels = NULL) {
  p <- resolve_certify(x, alternative, title, part_labels)
  N <- p$nodes
  born <- ifelse(is.na(N$born), 0L, N$born)
  t_max <- max(vapply(p$upper, length, 1L))
  if (is.null(iteration)) iteration <- t_max
  t <- iteration
  stop_unless(
    length(t) == 1L && t >= 0L && t <= t_max,
    "`iteration` must be between 0 and ", t_max
  )
  retired <- N$retired
  split_by_t <- N$fate == "split" & !is.na(retired) & retired <= t
  alive <- born <= t & !split_by_t
  settled <- alive & N$fate == "pruned" & !is.na(retired) & retired <= t
  live <- alive & !settled
  upper <- as.numeric(N$upper)
  ramp <- grDevices::colorRampPalette(c("#f6f2e8", CLAY))(64)
  rng <- range(upper)
  shade <- ramp[1L + floor(63 * (upper - rng[1L]) /
                              max(diff(rng), .Machine$double.eps))]

  ternary_frame()
  draw_polys(N$vertices[settled], ternary_xy, col = "#e9e5db",
             border = "#dcd6ca", lwd = 0.35)
  draw_polys(N$vertices[live], ternary_xy, col = shade[live],
             border = "#948d80", lwd = 0.5)
  draw_parts_simplex(p$seeds, fill = FALSE, border = INK)
  ternary_outline()
  draw_atoms(p$marks$q, NULL, to_xy = ternary_xy, cex = 0.7)
  ternary_labels()
  invisible(p)
}

#' @rdname plots
#' @export
plot_compare <- function(runs, labels = NULL, colour_by = NULL, dash_by = NULL,
                         dashes = NULL, y = c("both", "kl", "gap"),
                         x = c("step", "time"), legend = TRUE) {
  y <- match.arg(y)
  x <- match.arg(x)
  p <- resolve_compare(runs, labels, colour_by, dash_by, dashes)
  panels <- if (y == "both") c("kl", "gap") else y
  if (length(panels) == 2L) {
    op <- graphics::par(mfrow = c(2L, 1L), mar = c(4, 4.5, 1, 1))
    on.exit(graphics::par(op))
  }
  colour <- function(r) {
    if (r$colour <= length(RUN_COLOURS)) RUN_COLOURS[r$colour] else EDGE
  }
  dashed <- !is.null(p$legend$dash)
  # The pattern of dash level i: the one the payload names for it, else the
  # positional default, as in the widget.
  level_lty <- function(i) {
    named <- as.character(p$legend$dashes)
    if (i <= length(named)) {
      unname(RUN_DASHES[named[i]])
    } else {
      unname(RUN_DASHES[(i - 1L) %% length(RUN_DASHES) + 1L])
    }
  }
  lty <- function(r) if (dashed) level_lty(r$dash) else "solid"
  for (what in panels) {
    xs <- lapply(p$runs, function(r) as.numeric(if (x == "step") r$step else r$time))
    ys <- lapply(p$runs, function(r) as.numeric(r[[what]]))
    ok <- unlist(Map(function(a, b) is.finite(a) & is.finite(b) &
                       (what == "kl" | b > 0), xs, ys))
    all_x <- unlist(xs)[ok]
    all_y <- unlist(ys)[ok]
    stop_unless(length(all_y) > 0L, "no finite ", what, " values to draw")
    graphics::plot(
      range(all_x), range(all_y), type = "n",
      log = if (what == "gap") "y" else "",
      xlab = if (x == "step") "oracle step" else "seconds",
      ylab = if (what == "kl") "KL(Q || P)" else "Frank-Wolfe gap",
      bty = "l", las = 1
    )
    for (i in seq_along(p$runs)) {
      keep <- is.finite(xs[[i]]) & is.finite(ys[[i]]) &
        (what == "kl" | ys[[i]] > 0)
      graphics::lines(
        xs[[i]][keep], ys[[i]][keep],
        col = colour(p$runs[[i]]), lty = lty(p$runs[[i]]), lwd = 1.5
      )
    }
  }
  if (legend) {
    # Grouped runs are keyed by their groups, as in the widget; ungrouped
    # runs by their labels.
    if (is.null(p$legend$colour) && !dashed) {
      lbl <- vapply(p$runs, function(r) r$label, "")
      col <- vapply(p$runs, colour, "")
      lt <- rep("solid", length(lbl))
    } else {
      lc <- as.character(p$legend$colour)
      ld <- as.character(p$legend$dash)
      lbl <- c(lc, ld)
      col <- c(utils::head(c(RUN_COLOURS, rep(EDGE, length(lc))), length(lc)),
               rep(INK, length(ld)))
      lt <- c(rep("solid", length(lc)),
              vapply(seq_along(ld), level_lty, ""))
    }
    graphics::legend(
      "topright", legend = lbl, bty = "n",
      col = col, lty = lt, lwd = 1.5, cex = 0.8, seg.len = 3
    )
  }
  invisible(p)
}


# --- Ternary drawing -------------------------------------------------------------

# theta_1 at the apex, theta_2 bottom left, theta_3 bottom right.
TERNARY <- rbind(c(0.5, sqrt(3) / 2), c(0, 0), c(1, 0))

ternary_xy <- function(b) as_points(b) %*% TERNARY

identity_xy <- function(b) as_points(b)

ternary_frame <- function() {
  graphics::plot.new()
  graphics::plot.window(
    xlim = c(-0.06, 1.06), ylim = c(-0.08, sqrt(3) / 2 + 0.06), asp = 1
  )
}

ternary_outline <- function() {
  graphics::polygon(TERNARY, border = EDGE, lwd = 1.2)
}

ternary_labels <- function() {
  graphics::text(TERNARY[1L, 1L], TERNARY[1L, 2L], quote(theta[1]),
                 pos = 3, font = 3, col = INK)
  graphics::text(TERNARY[2L, 1L], TERNARY[2L, 2L], quote(theta[2]),
                 pos = 1, font = 3, col = INK)
  graphics::text(TERNARY[3L, 1L], TERNARY[3L, 2L], quote(theta[3]),
                 pos = 1, font = 3, col = INK)
}

draw_polys <- function(vertex_lists, to_xy, ...) {
  if (length(vertex_lists) == 0L) return(invisible())
  xy <- do.call(rbind, lapply(vertex_lists, function(v) {
    rbind(to_xy(unrows(v)), c(NA, NA))
  }))
  graphics::polygon(xy, ...)
}

draw_parts_simplex <- function(seeds, fill, border = PART_EDGE) {
  if (fill) {
    draw_polys(seeds, ternary_xy, col = PART_FILL, border = NA)
  }
  draw_polys(seeds, ternary_xy, border = border, lty = 2, lwd = 1)
}

# Atom area proportional to weight, with a floor so light atoms stay visible.
draw_atoms <- function(q, weights, to_xy, pch = 19, col = INK, cex = NULL,
                       lwd = 1) {
  if (length(q) == 0L) return(invisible())
  pts <- unrows(q)
  if (is.null(cex)) {
    w <- as.numeric(weights)
    cex <- if (length(w)) pmax(0.4, 1.4 * sqrt(w / max(w))) else 1
  }
  # Sizes go with their points through any filtering `to_xy` does.
  cex <- rep_len(cex, nrow(pts))
  keep <- vapply(seq_len(nrow(pts)), function(i) nrow(to_xy(pts[i, ])) > 0L, TRUE)
  graphics::points(to_xy(pts), pch = pch, col = col, cex = cex[keep], lwd = lwd)
}

# The deck's diverging colours about G = 1, on log10 G clamped to +-1.2.
field_colours <- function(z) {
  stops <- c(-1.2, -0.4, 0, 0.4, 1.2)
  cols <- grDevices::col2rgb(
    c("#e7edf1", "#cbdae3", "#f7f4ec", "#dd9b83", CLAY)
  ) / 255
  z <- pmin(pmax(z, -1.2), 1.2)
  grDevices::rgb(
    stats::approx(stops, cols[1L, ], z)$y,
    stats::approx(stops, cols[2L, ], z)$y,
    stats::approx(stops, cols[3L, ], z)$y
  )
}

# `inside` (same shape as `z`) blanks the field outside the region drawn; the
# G = 1 contour comes from the full field and is cut at the same boundary, so
# it runs cleanly to the edge.
draw_field <- function(xs, ys, z, inside = NULL) {
  shown <- pmin(pmax(z, -1.2), 1.2)
  if (!is.null(inside)) shown[!inside] <- NA
  breaks <- seq(-1.2, 1.2, length.out = 49L)
  graphics::image(
    xs, ys, shown,
    breaks = breaks, col = field_colours(utils::head(breaks, -1L) + 0.025),
    add = TRUE, useRaster = TRUE
  )
  for (cl in grDevices::contourLines(xs, ys, z, levels = 0)) {
    keep <- is.null(inside) | in_simplex(cbind(cl$x, cl$y))
    graphics::lines(ifelse(keep, cl$x, NA), cl$y, lwd = 1.5, col = INK)
  }
}

# Barycentric coordinates of plotted points, inverting `ternary_xy()`:
# x = b1 / 2 + b3 and y = b1 sqrt(3) / 2.
ternary_bary <- function(xy) {
  b1 <- xy[, 2L] * 2 / sqrt(3)
  b3 <- xy[, 1L] - b1 / 2
  cbind(b1, 1 - b1 - b3, b3)
}

in_simplex <- function(xy) {
  b <- ternary_bary(xy)
  b[, 1L] >= 0 & b[, 2L] >= 0 & b[, 3L] >= 0
}

# G over a rectangle covering the simplex: a polynomial in the barycentric
# coordinates, so it is evaluated outside the simplex too and only drawn
# inside.
simplex_field <- function(lattice, ratio, n = 300L) {
  usr <- graphics::par("usr")
  xs <- seq(usr[1L], usr[2L], length.out = n)
  ys <- seq(usr[3L], usr[4L], length.out = n)
  grid <- as.matrix(expand.grid(x = xs, y = ys))
  b <- ternary_bary(grid)
  y <- unrows(lattice$outcomes)
  basis <- exp(as.numeric(lattice$log_choose)) *
    t(vapply(seq_len(nrow(y)), function(k) {
      b[, 1L]^y[k, 1L] * b[, 2L]^y[k, 2L] * b[, 3L]^y[k, 3L]
    }, numeric(nrow(b))))
  g <- as.numeric(crossprod(basis, as.numeric(ratio)))
  z <- rep(-99, length(g))
  z[g > 0] <- log10(g[g > 0])
  draw_field(xs, ys, matrix(z, n, n), matrix(in_simplex(grid), n, n))
}


# --- Planar drawing --------------------------------------------------------------

# The plot region is shrunk to the extent's aspect ratio, so the axes are
# equally scaled and the field, computed over the extent, fills the frame.
# Returns the `par()` to restore once the plot is drawn.
plane_frame <- function(extent) {
  x <- as.numeric(extent$x)
  y <- as.numeric(extent$y)
  graphics::plot.new()
  pin <- graphics::par("pin")
  plt <- graphics::par("plt")
  ratio <- diff(y) / diff(x)
  if (pin[2L] / pin[1L] > ratio) {
    h <- (plt[4L] - plt[3L]) * (pin[1L] * ratio / pin[2L])
    mid <- mean(plt[3:4])
    new <- c(plt[1:2], mid - h / 2, mid + h / 2)
  } else {
    w <- (plt[2L] - plt[1L]) * (pin[2L] / ratio / pin[1L])
    mid <- mean(plt[1:2])
    new <- c(mid - w / 2, mid + w / 2, plt[3:4])
  }
  op <- graphics::par(plt = new)
  graphics::plot.window(x, y, xaxs = "i", yaxs = "i")
  graphics::axis(1)
  graphics::axis(2, las = 1)
  graphics::box()
  graphics::title(xlab = quote(theta[1]), ylab = quote(theta[2]))
  op
}

plane_field <- function(field, z) {
  xs <- field$x0 + (seq_len(field$nx) - 1L) * field$dx
  ys <- field$y0 + (seq_len(field$ny) - 1L) * field$dy
  draw_field(xs, ys, matrix(as.numeric(z), field$nx, field$ny))
}

# A polyhedron conv(V) + cone(R) + span(L), drawn as the hull of its vertices
# pushed far along every ray and line; the plot region clips it.
draw_parts_plane <- function(seeds, extent, fill, border = PART_EDGE) {
  far <- 10 * max(diff(as.numeric(extent$x)), diff(as.numeric(extent$y)))
  hulls <- lapply(seeds, function(s) {
    v <- unrows(s$v)
    dirs <- rbind(
      if (length(s$r)) unrows(s$r),
      if (length(s$l)) rbind(unrows(s$l), -unrows(s$l))
    )
    pts <- v
    if (!is.null(dirs)) {
      dirs <- dirs / sqrt(rowSums(dirs^2))
      pts <- rbind(pts, do.call(rbind, lapply(seq_len(nrow(dirs)), function(i) {
        sweep(v, 2L, far * dirs[i, ], "+")
      })))
    }
    h <- grDevices::chull(pts)
    rows(pts[h, , drop = FALSE])
  })
  if (fill) draw_polys(hulls, identity_xy, col = PART_FILL, border = NA)
  draw_polys(hulls, identity_xy, border = border, lty = 2, lwd = 1)
}
