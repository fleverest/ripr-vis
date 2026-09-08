# The regression net against ripr API drift: run a genuinely tiny fit and
# certification through the real package and check the payload contracts the
# JS side depends on. Skipped when ripr is not installed.

test_that("payloads build from real ripr objects", {
  skip_if_not_installed("ripr")

  n_trials <- 4L
  family <- ripr::multinomial_family(n_trials = n_trials, k = 3L)
  null <- ripr::null_model(
    family,
    lapply(2:3, function(j) {
      vertices <- diag(3)
      vertices[, 1L] <- replace(numeric(3), c(1L, j), 0.5)
      ripr::simplex_region(vertices = vertices)
    })
  )
  q <- c(0.40, 0.34, 0.26)
  set.seed(1L)
  state <- ripr::ripr_init(
    family(q), null,
    control = ripr::ripr_control(snapshot = "all")
  )
  for (i in 1:3) {
    state <- state |>
      ripr::fw_step(record_gap = TRUE) |>
      ripr::em_step(record_gap = TRUE)
  }

  lattice <- ripr_lattice_data(family)
  m <- choose(n_trials + 2L, 2L)
  expect_length(lattice$outcomes, m)

  problem <- ripr_problem_simplex_data(null, q, title = "tiny")
  expect_length(problem$seeds, 2L)
  expect_length(problem$seeds[[1]], 3L)

  fit <- ripr_fit_simplex_data(state, lattice, q = q)
  expect_gte(length(fit$ratio), 6L)
  expect_true(all(lengths(fit$ratio) == m))
  expect_length(fit$kl, length(fit$ratio))
  expect_true(all(is.finite(as.numeric(fit$kl))))

  finished <- ripr::ripr_finish(
    state,
    reoptimise = TRUE, identify = TRUE, record_gap = TRUE
  )
  x <- ripr::likelihood(family(q), label = "Q") /
    ripr::likelihood(finished$P_star, label = "P*")
  nodes <- ripr::certify_trace(x, null, tol = 1e-6)

  cert <- ripr_certify_simplex_data(nodes, tol = 1e-6)
  expect_gte(length(cert$cells), 2L)
  expect_length(cert$upper, length(cert$cells))
  expect_length(cert$lower, length(cert$cells))
  expect_true(is.numeric(cert$certificate$sup_ub))

  w <- ripr_certify_simplex(problem, cert)
  x_parsed <- payload_of(w)
  expect_length(x_parsed$labels$cells, length(cert$cells))

  w_fit <- ripr_fit_simplex(problem, fit, lattice)
  expect_s3_class(w_fit, "htmlwidget")
})
