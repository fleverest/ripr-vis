skip_if_not_installed("ripr")

test_that("every verb builds the one riprvis widget, tagged by kind", {
  f <- simplex_fixture()
  g <- plane_fixture()
  widgets <- list(
    problem_simplex = vis_problem(f$state),
    fit_simplex = vis_fit(f$state),
    certify_simplex = vis_certify(f$nodes),
    compare = vis_compare(f$state),
    problem2d = vis_problem(g$state),
    fit2d = vis_fit(g$state, nx = 30L)
  )
  for (kind in names(widgets)) {
    w <- widgets[[kind]]
    expect_s3_class(w, "htmlwidget")
    expect_identical(class(w)[1L], "riprvis")
    expect_identical(w$x$kind, kind)
    expect_identical(payload_of(w)$kind, kind)
    expect_false(grepl("\"NA\"", w$x$data, fixed = TRUE))
  }
})

test_that("a saved payload redraws, and only as its own kind", {
  f <- simplex_fixture()
  p <- riprvis_payload(vis_fit(f$state))
  path <- tempfile(fileext = ".rds")
  saveRDS(p, path)
  q <- readRDS(path)
  expect_identical(riprvis_payload(vis_fit(q)), p)
  expect_identical(riprvis_payload(q), q)
  expect_error(vis_problem(q), "fit_simplex payload, not a problem")
  expect_error(vis_compare(q), "not a comparison")
  expect_error(riprvis_payload(list()), "riprvis widget or payload")
})

test_that("a single-step payload still crosses arrays", {
  f <- simplex_fixture()
  one <- ripr::ripr_init(
    f$Q, f$null, control = ripr::ripr_control(snapshot = "all")
  ) |>
    ripr::fw_step()
  x <- payload_of(vis_fit(one))
  expect_type(x$fit$kl, "list")
  expect_type(x$fit$ratio[[1]], "list")
})

test_that("the widgets render to tags", {
  skip_if_not_installed("htmltools")
  f <- simplex_fixture()
  expect_no_error(htmltools::as.tags(vis_fit(f$state)))
})

test_that("one Shiny output and render pair serves every view", {
  skip_if_not_installed("shiny")
  out <- riprvisOutput("x")
  expect_match(as.character(out), "riprvis")
  expect_true(is.function(renderRiprvis(vis_problem(simplex_fixture()$null))))
})
