# The planar (Gaussian/polyhedral) payloads, tested from plain-list fixtures
# so no ripr is needed: a two-part null (one dart with a ray, one bare
# triangle) and a two-snapshot state whose engine is a tiny hand-made
# quadrature rule.

dart_null <- function() {
  list(
    list(
      v = cbind(c(1, 0), c(1.75, 0.6), c(1.75, -0.6), c(2.5, 0)),
      r = cbind(c(1, 0))
    ),
    cbind(c(-1, 0), c(-2, 0.5), c(-2, -0.5))
  )
}

plane_problem <- function(margin = 0.2) {
  ripr_problem2d_data(
    dart_null(),
    q = c(0, 0),
    title = "darts",
    part_labels = c("+x", "-x"),
    margin = margin
  )
}

# Nine-node product rule; the log weights sum to one, which is all the
# G(theta) = 1-at-its-own-atom identity needs.
plane_state <- function() {
  nodes <- as.matrix(expand.grid(x = c(-1, 0, 1), y = c(-1, 0, 1)))
  list(
    trace = data.frame(
      fw = c(1L, 1L),
      lb = c(0L, 0L),
      em = c(0L, 1L),
      weight = c(0L, 0L),
      phase = c("fw", "em"),
      kl = c(0.3, 0.2),
      gap = c(0.1, NA),
      gap_theta = I(list(c(1, 0), NA))
    ),
    snapshots = list(
      list(
        iters = c(fw = 1L, lb = 0L, em = 0L, weight = 0L),
        phase = "fw",
        atoms = list(cbind(c(1, 0))),
        weights = list(1)
      ),
      list(
        iters = c(fw = 1L, lb = 0L, em = 1L, weight = 0L),
        phase = "em",
        atoms = list(cbind(c(1, 0)), cbind(c(-1, 0))),
        weights = list(0.5, 0.5)
      )
    ),
    engine = list(
      nodes = unname(nodes),
      log_w = rep(log(1 / 9), 9L)
    )
  )
}

test_that("problem2d payload frames the vertices with the margin", {
  pr <- plane_problem(margin = 0.2)
  # vertex bbox is x in [-2, 2.5], y in [-0.6, 0.6]; larger span 4.5
  expect_equal(as.numeric(pr$extent$x), c(-2, 2.5) + c(-0.9, 0.9))
  expect_equal(as.numeric(pr$extent$y), c(-0.6, 0.6) + c(-0.9, 0.9))

  expect_length(pr$seeds, 2L)
  expect_length(pr$seeds[[1]]$v, 4L)
  expect_equal(pr$seeds[[1]]$r[[1]], c(1, 0))
  # a bare matrix part gets empty ray/lineality blocks
  expect_length(pr$seeds[[2]]$r, 0L)
  expect_length(pr$seeds[[2]]$l, 0L)
})

test_that("problem2d validates dimensions and margin", {
  expect_error(
    ripr_problem2d_data(list(diag(3)), q = c(0, 0, 0)),
    "2 rows"
  )
  expect_error(plane_problem(margin = -1), "non-negative")
})

test_that("fit2d payload evaluates the field on the extent grid", {
  pr <- plane_problem()
  fit <- ripr_fit2d_data(plane_state(), pr, sigma = diag(2), nx = 21L)

  f <- fit$field
  expect_equal(f$nx, 21L)
  # square cells: dy within one cell of dx
  expect_equal(f$dx, f$dy, tolerance = 0.1)
  expect_true(all(lengths(fit$z) == f$nx * f$ny))
  expect_length(fit$z, 2L)
  expect_equal(as.numeric(fit$kl), c(0.3, 0.2))
  expect_equal(fit$gap_theta[[1]], c(1, 0))
  expect_identical(fit$gap_theta[[2]], NA)
  # clamped log10 values
  expect_true(all(abs(unlist(fit$z)) <= 6))
})

test_that("the field is exactly one at the iterate's own atom", {
  # With P the single atom at theta*, G(theta*) = sum of the quadrature
  # weights = 1, whatever the nodes: put theta* on a grid point and read it.
  null <- list(cbind(c(1, 0), c(2, 1), c(2, -1)))
  pr <- ripr_problem2d_data(null, q = c(1, 0), margin = 0.25)
  # vertex bbox x in [1, 2], y in [-1, 1]; the larger span is 2, so the pad
  # is 0.5 and the extent is x in [0.5, 2.5], y in [-1.5, 1.5]. nx = 9 gives
  # dx = 0.25, putting grid points exactly at x = 1 and y = 0.
  st <- plane_state()
  st$snapshots <- st$snapshots[1L]
  st$trace <- st$trace[1L, ]
  fit <- ripr_fit2d_data(st, pr, sigma = diag(2), nx = 9L)
  f <- fit$field
  gx <- f$x0 + (seq_len(f$nx) - 1) * f$dx
  gy <- f$y0 + (seq_len(f$ny) - 1) * f$dy
  i <- which(abs(gx - 1) < 1e-9)
  j <- which(abs(gy) < 1e-9)
  expect_length(i, 1L)
  expect_length(j, 1L)
  z <- as.numeric(fit$z[[1L]])
  expect_equal(z[(j - 1) * f$nx + i], 0, tolerance = 1e-3)
})

test_that("fit2d validates its inputs", {
  pr <- plane_problem()
  st <- plane_state()
  st$engine <- NULL
  expect_error(ripr_fit2d_data(st, pr), "quadrature engine")

  st <- plane_state()
  st$engine$nodes <- st$engine$nodes[, 1L, drop = FALSE]
  expect_error(ripr_fit2d_data(st, pr), "M x 2")

  expect_error(
    ripr_fit2d_data(plane_state(), pr, sigma = diag(3)),
    "2 x 2"
  )
})

test_that("plane widgets build and their payloads survive the wire", {
  pr <- plane_problem()
  fit <- ripr_fit2d_data(plane_state(), pr, sigma = diag(2), nx = 15L)

  wp <- ripr_problem2d(pr)
  expect_s3_class(wp, "htmlwidget")
  xp <- payload_of(wp)
  expect_length(xp$extent$x, 2L)
  expect_length(xp$seeds[[1]]$r, 1L)
  expect_length(xp$seeds[[2]]$r, 0L)

  wf <- ripr_fit2d(pr, fit)
  expect_s3_class(wf, "htmlwidget")
  xf <- payload_of(wf)
  expect_length(xf$fit$z, 2L)
  expect_length(xf$fit$z[[1]], fit$field$nx * fit$field$ny)
  expect_null(xf$fit$gap[[2]])
  expect_null(xf$fit$gap_theta[[2]])

  bad <- fit
  bad$z[[1]] <- bad$z[[1]][-1]
  expect_error(ripr_fit2d(pr, bad), "nx \\* ny")
})

test_that("fit2d builds from a real gaussian ripr state", {
  skip_if_not_installed("ripr")

  fam <- ripr::gaussian_family(dim = 2L)
  null <- ripr::null_model(
    fam,
    list(ripr::polyhedron_region(
      vertices = cbind(c(1, 0), c(2, 0.7), c(2, -0.7)),
      rays = cbind(c(1, 0))
    ))
  )
  set.seed(1L)
  state <- ripr::ripr_init(
    fam(c(0, 0)), null,
    engine = ripr::gh_engine(8L),
    control = ripr::ripr_control(snapshot = "all")
  )
  state <- ripr::em_step(ripr::fw_step(state, record_gap = TRUE),
    record_gap = TRUE
  )

  pr <- ripr_problem2d_data(null, q = c(0, 0), title = "gauss")
  expect_equal(pr$seeds[[1]]$r[[1]], c(1, 0))

  fit <- ripr_fit2d_data(state, pr, nx = 41L)
  expect_true(all(lengths(fit$z) == fit$field$nx * fit$field$ny))
  expect_true(all(is.finite(as.numeric(fit$kl))))
  w <- ripr_fit2d(pr, fit)
  expect_s3_class(w, "htmlwidget")
})
