skip_if_not_installed("ripr")

# Draw to a throwaway device; returns what the plot returned.
draws <- function(expr) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expr
}

test_that("every static view draws and returns its payload", {
  f <- simplex_fixture()
  g <- plane_fixture()
  expect_identical(draws(plot_problem(f$state))$kind, "problem_simplex")
  expect_identical(draws(plot_fit(f$state))$kind, "fit_simplex")
  expect_identical(draws(plot_fit(f$state, step = 1))$kind, "fit_simplex")
  expect_identical(draws(plot_certify(f$nodes, iteration = 1))$kind,
                   "certify_simplex")
  expect_identical(draws(plot_compare(list(f$state, f$fit)))$kind, "compare")
  expect_identical(draws(plot_problem(g$null))$kind, "problem2d")
  expect_identical(draws(plot_fit(g$state, nx = 30L))$kind, "fit2d")
})

test_that("static and interactive views share a payload", {
  f <- simplex_fixture()
  p <- riprvis_payload(vis_fit(f$state))
  expect_identical(draws(plot_fit(p)), p)
})

test_that("out-of-range frames are refused", {
  f <- simplex_fixture()
  n <- length(f$state@snapshots)
  expect_error(draws(plot_fit(f$state, step = n + 1)), "between 1 and")
  expect_error(draws(plot_certify(f$nodes, iteration = 1e6)), "between 0 and")
})

test_that("compare draws either trace against either axis", {
  f <- simplex_fixture()
  for (y in c("both", "kl", "gap")) {
    for (x in c("step", "time")) {
      expect_no_error(draws(plot_compare(f$state, y = y, x = x)))
    }
  }
  p <- draws(plot_compare(
    list(f$state, f$fit), dash_by = c("a", "b"), dashes = c(b = "dotted", a = "solid")
  ))
  expect_identical(as.character(p$legend$dashes), c("solid", "dotted"))
})
