test_that("ripr_problem builds a widget whose payload survives the wire", {
  w <- ripr_problem(tiny_problem())
  expect_s3_class(w, "htmlwidget")
  expect_type(w$x$data, "character")

  x <- payload_of(w)
  # a single-atom alternative must still cross as arrays of arrays
  expect_length(x$marks$q, 1L)
  expect_length(x$marks$q[[1]], 3L)
  expect_length(x$marks$weights, 1L)
  expect_length(x$seeds, 2L)
  expect_equal(x$labels$title, "tiny")
})

test_that("ripr_fit combines problem, fit and lattice payloads", {
  lat <- tiny_lattice(2L)
  fit <- ripr_fit_data(tiny_state(), lat, q = c(0.4, 0.34, 0.26))
  w <- ripr_fit(tiny_problem(), fit, lat)
  expect_s3_class(w, "htmlwidget")

  x <- payload_of(w)
  expect_length(x$fit$ratio, 2L)
  expect_length(x$lattice$outcomes, 6L)
  # NA gap and all-NA gap_theta cross as null, never as the string "NA"
  expect_null(x$fit$gap[[2]])
  expect_null(x$fit$gap_theta[[2]])
  expect_false(grepl("\"NA\"", w$x$data, fixed = TRUE))
})

test_that("ripr_fit validates ratio length against the lattice", {
  lat3 <- tiny_lattice(3L)
  lat2 <- tiny_lattice(2L)
  fit <- ripr_fit_data(tiny_state(), lat2, q = c(0.4, 0.34, 0.26))
  expect_error(ripr_fit(tiny_problem(), fit, lat3), "per lattice outcome")
})

test_that("ripr_certify labels one tab per cell and keeps arrays arrays", {
  cert <- ripr_certify_data(tiny_nodes())
  w <- ripr_certify(tiny_problem(), cert)
  expect_s3_class(w, "htmlwidget")

  x <- payload_of(w)
  expect_equal(unlist(x$labels$cells), c("a", "b"))
  # cell 2 ran one iteration; its traces must still be arrays
  expect_length(x$upper, 2L)
  expect_length(x$upper[[2]], 1L)
  expect_length(x$lower[[2]], 1L)
  # retired NA crosses as null
  expect_null(x$nodes$retired[[2]])
  expect_length(x$certificate$iterations, 2L)
})

test_that("ripr_certify labels several cells of one part distinctly", {
  fx <- tiny_nodes()
  fx$nodes$part <- c(1L, 1L, 1L, 1L)
  cert <- ripr_certify_data(fx)
  w <- ripr_certify(tiny_problem(), cert)
  labs <- unlist(payload_of(w)$labels$cells)
  expect_length(unique(labs), 2L)
  expect_true(all(startsWith(labs, "a")))
})

test_that("ripr_certify rejects a certify payload pointing past the parts", {
  fx <- tiny_nodes()
  fx$nodes$part <- c(1L, 1L, 1L, 3L)
  cert <- ripr_certify_data(fx)
  expect_error(ripr_certify(tiny_problem(), cert), "does not have")
})

test_that("widgets render to tags without error", {
  skip_if_not_installed("htmltools")
  lat <- tiny_lattice(2L)
  fit <- ripr_fit_data(tiny_state(), lat, q = c(0.4, 0.34, 0.26))
  cert <- ripr_certify_data(tiny_nodes())
  for (w in list(
    ripr_problem(tiny_problem()),
    ripr_fit(tiny_problem(), fit, lat),
    ripr_certify(tiny_problem(), cert)
  )) {
    expect_no_error(htmltools::as.tags(w))
  }
})
