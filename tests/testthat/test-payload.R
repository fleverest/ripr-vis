skip_if_not_installed("ripr")

test_that("a simplex problem carries its parts and the alternative", {
  f <- simplex_fixture()
  p <- riprvis_payload(vis_problem(f$null, alternative = f$Q))
  expect_s3_class(p, "riprvis_payload")
  expect_identical(p$kind, "problem_simplex")
  expect_length(p$seeds, 2L)
  expect_true(all(lengths(p$seeds) == 3L))
  expect_equal(unlist(p$marks$q), f$q)
  expect_equal(as.numeric(p$marks$weights), 1)
  expect_identical(as.character(p$labels$parts), c("part 1", "part 2"))
})

test_that("a state or fit supplies its own null and alternative", {
  f <- simplex_fixture()
  from_state <- riprvis_payload(vis_problem(f$state))
  expect_identical(from_state, riprvis_payload(vis_problem(f$fit)))
  expect_equal(unlist(from_state$marks$q), f$q)
  # Without an alternative there is simply nothing to mark.
  bare <- riprvis_payload(vis_problem(f$null))
  expect_length(bare$marks$q, 0L)
})

test_that("part labels must match the parts", {
  f <- simplex_fixture()
  p <- riprvis_payload(vis_problem(f$null, part_labels = c("a", "b")))
  expect_identical(as.character(p$labels$parts), c("a", "b"))
  expect_error(vis_problem(f$null, part_labels = "a"), "one entry per part")
})

test_that("only drawable families are accepted", {
  fam <- ripr::multinomial_family(n_trials = 3L, k = 4L)
  null <- ripr::null_model(fam, ripr::simplex_region(vertices = diag(4)))
  expect_error(vis_problem(null), "three-category multinomial")
})

test_that("each simplex fit step crosses as the ratio Q/P over the lattice", {
  f <- simplex_fixture()
  p <- riprvis_payload(vis_fit(f$state))
  expect_identical(p$kind, "fit_simplex")
  snaps <- f$state@snapshots
  expect_length(p$fit$ratio, length(snaps))
  y <- ripr::enumerate_space(f$family@sample_space)
  expect_length(p$lattice$outcomes, nrow(y))
  last <- snaps[[length(snaps)]]$mixing
  direct <- exp(ripr::log_density(f$Q, y) -
                  ripr::log_density(f$family(last), y))
  expect_equal(as.numeric(p$fit$ratio[[length(snaps)]]), direct,
               tolerance = 1e-6)
})

test_that("fit diagnostics line up with the trace rows of the snapshots", {
  f <- simplex_fixture()
  p <- riprvis_payload(vis_fit(f$state))
  rows <- match(
    vapply(f$state@snapshots, function(s) s$step, 0L),
    f$state@trace$step
  )
  expect_equal(as.numeric(p$fit$kl), f$state@trace$kl[rows])
  expect_equal(as.numeric(p$fit$gap), f$state@trace$gap_after[rows])
  expect_identical(as.character(p$fit$phase), f$state@trace$phase[rows])
  # An em row records no gap, which must cross as null, not the string "NA".
  expect_true(anyNA(p$fit$gap))
  expect_false(grepl("\"NA\"", vis_fit(f$state)$x$data, fixed = TRUE))
})

test_that("a finished fit draws as the state it came from", {
  f <- simplex_fixture()
  expect_identical(
    riprvis_payload(vis_fit(f$fit)),
    riprvis_payload(vis_fit(f$state))
  )
})

test_that("a fit without snapshots is refused with the remedy", {
  f <- simplex_fixture()
  state <- ripr::ripr_init(f$Q, f$null)
  expect_error(vis_fit(state), "snapshot = \"all\"", fixed = TRUE)
})

test_that("a planar fit crosses as a grid of log10 G per step", {
  f <- plane_fixture()
  p <- riprvis_payload(vis_fit(f$state, nx = 40L))
  expect_identical(p$kind, "fit2d")
  expect_identical(p$fit$field$nx, 40L)
  n <- p$fit$field$nx * p$fit$field$ny
  expect_true(all(lengths(p$fit$z) == n))
  z <- unlist(p$fit$z)
  expect_true(all(is.finite(z) & z >= -6 & z <= 6))
  expect_length(p$seeds, 4L)
  expect_true(all(vapply(p$seeds, function(s) length(s$r), 1L) == 1L))
})

test_that("a certification crosses as cells, nodes and per-cell traces", {
  f <- simplex_fixture()
  p <- riprvis_payload(vis_certify(f$nodes, alternative = f$Q))
  expect_identical(p$kind, "certify_simplex")
  n_cells <- length(unique(f$nodes$cell))
  expect_length(p$cells, n_cells)
  expect_length(p$upper, n_cells)
  expect_length(p$lower, n_cells)
  expect_length(p$nodes$id, nrow(f$nodes))
  expect_length(p$labels$cells, n_cells)
  expect_equal(p$certificate$sup_ub, attr(f$nodes, "certificate")@sup_ub)
  expect_error(vis_certify(data.frame()), "certify_trace")
})

test_that("compare counts oracle steps and groups runs", {
  f <- simplex_fixture()
  p <- riprvis_payload(vis_compare(list(a = f$state, b = f$fit)))
  expect_identical(p$kind, "compare")
  expect_identical(vapply(p$runs, `[[`, "", "label"), c("a", "b"))
  phase <- f$state@trace$phase
  expect_equal(as.integer(p$runs[[1]]$step), cumsum(phase %in% c("fw", "lb")))
  expect_true(p$has_time)
  # A lone run, and a bare trace, need no list.
  expect_length(riprvis_payload(vis_compare(f$state))$runs, 1L)
  expect_length(riprvis_payload(vis_compare(f$state@trace))$runs, 1L)

  runs <- rep(list(f$state), 6L)
  g <- riprvis_payload(vis_compare(
    runs,
    colour_by = rep(c("x", "y"), 3L),
    dash_by = rep(c("u", "v", "w"), each = 2L)
  ))
  expect_identical(as.character(g$legend$colour), c("x", "y"))
  expect_identical(vapply(g$runs, `[[`, 1L, "dash"), rep(1:3, each = 2L))
  expect_warning(vis_compare(runs), "at most five")
  expect_error(vis_compare(runs, labels = "a"), "one entry per run")
  expect_error(vis_compare(list(1)), "ripr state")
})
