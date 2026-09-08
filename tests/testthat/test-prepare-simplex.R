test_that("lattice payload has one aligned entry per outcome", {
  lat <- tiny_lattice(2L)
  expect_equal(lat$n, 2L)
  expect_length(lat$outcomes, 6L)
  expect_length(lat$log_choose, 6L)
  expect_true(all(vapply(lat$outcomes, sum, 0) == 2))
  # the multinomial coefficients of n = 2: choose(2; y) in {1, 2}
  expect_setequal(round(exp(lat$log_choose)), c(1, 2))
})

test_that("lattice rejects rows that do not sum to n_trials", {
  bad <- list(n_trials = 2L, outcomes = rbind(c(1L, 1L, 1L)))
  expect_error(ripr_lattice_data(bad), "sum to n_trials")
})

test_that("lattice pmf sums to one and matches dmultinom", {
  lat <- tiny_lattice(3L)
  theta <- c(0.2, 0.5, 0.3)
  p <- riprvis:::lattice_pmf(lat, theta)
  expect_equal(sum(p), 1, tolerance = 1e-12)
  y <- riprvis:::lattice_outcomes(lat)
  expect_equal(
    p,
    apply(y, 1L, function(row) dmultinom(row, prob = theta))
  )
  # a mixture pmf is the weighted sum of its components'
  atoms <- cbind(c(0.2, 0.5, 0.3), c(0.6, 0.2, 0.2))
  pm <- riprvis:::lattice_mixture_pmf(lat, atoms, c(0.25, 0.75))
  expect_equal(
    pm,
    0.25 * riprvis:::lattice_pmf(lat, atoms[, 1]) +
      0.75 * riprvis:::lattice_pmf(lat, atoms[, 2])
  )
})

test_that("problem payload splits vertices into columns and defaults labels", {
  pr <- tiny_problem()
  expect_length(pr$seeds, 2L)
  expect_equal(pr$seeds[[1]][[1]], c(0.5, 0.5, 0))
  expect_equal(as.numeric(pr$marks$weights), 1)
  expect_equal(as.character(pr$labels$parts), c("a", "b"))

  defaulted <- ripr_problem_simplex_data(
    null = list(diag(3)),
    q = cbind(c(0.6, 0.2, 0.2), c(0.2, 0.6, 0.2))
  )
  expect_equal(as.character(defaulted$labels$parts), "part 1")
  expect_equal(as.numeric(defaulted$marks$weights), c(0.5, 0.5))
})

test_that("problem payload validates weights and labels lengths", {
  expect_error(
    ripr_problem_simplex_data(list(diag(3)), q = diag(3), weights = 1),
    "one entry per column"
  )
  expect_error(
    ripr_problem_simplex_data(
      list(diag(3)), q = c(1, 0, 0), part_labels = c("a", "b")
    ),
    "one entry per part"
  )
})

test_that("fit payload lines snapshots up with their trace rows", {
  lat <- tiny_lattice(2L)
  fit <- ripr_fit_simplex_data(tiny_state(), lat, q = c(0.4, 0.34, 0.26))
  expect_length(fit$ratio, 2L)
  expect_length(fit$ratio[[1]], 6L)
  expect_equal(as.numeric(fit$kl), c(0.5, 0.4))
  expect_equal(as.numeric(fit$gap), c(0.2, NA))
  expect_equal(fit$gap_theta[[1]], c(0.2, 0.5, 0.3))
  expect_identical(fit$gap_theta[[2]], NA)
  expect_equal(as.character(fit$phase), c("fw", "em"))

  # the ratio really is Q's pmf over the snapshot mixture's
  q_pmf <- riprvis:::lattice_pmf(lat, c(0.4, 0.34, 0.26))
  p1 <- riprvis:::lattice_pmf(lat, c(0.2, 0.5, 0.3))
  expect_equal(as.numeric(fit$ratio[[1]]), signif(q_pmf / p1, 7))
})

test_that("fit payload accepts a precomputed q_pmf and validates lengths", {
  lat <- tiny_lattice(2L)
  q_pmf <- riprvis:::lattice_pmf(lat, c(0.4, 0.34, 0.26))
  fit <- ripr_fit_simplex_data(tiny_state(), lat, q_pmf = q_pmf)
  expect_length(fit$ratio, 2L)
  expect_error(
    ripr_fit_simplex_data(tiny_state(), lat, q_pmf = 1:3),
    "one entry per lattice outcome"
  )
  expect_error(ripr_fit_simplex_data(tiny_state(), lat), "supply either")
})

test_that("certify payload is organised per cell", {
  cert <- ripr_certify_simplex_data(tiny_nodes(), tol = 1e-9)
  expect_length(cert$cells, 2L)
  expect_equal(cert$cells[[1]]$part, 1L)
  expect_equal(cert$cells[[2]]$part, 2L)
  # each cell's geometry is its root node's
  expect_equal(cert$cells[[2]]$vertices[[1]], c(0.5, 0, 0.5))
  expect_length(cert$upper, 2L)
  expect_equal(as.numeric(cert$upper[[2]]), 1.05)
  expect_equal(cert$certificate$sup_ub, 1.2)
  expect_equal(cert$certificate$tol, 1e-9)
})

test_that("certify payload flags pre-cell node tables", {
  old <- tiny_nodes()
  names(old$nodes)[names(old$nodes) == "cell"] <- "subnull"
  expect_error(ripr_certify_simplex_data(old), "pre-cell ripr version")
})
