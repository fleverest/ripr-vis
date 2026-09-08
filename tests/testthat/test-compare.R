test_that("compare payload lines runs up by step and accumulates the clock", {
  untimed <- tiny_state()
  untimed$trace$elapsed <- NULL
  cmp <- ripr_compare_data(list(fw = untimed, lb = tiny_state()))
  expect_length(cmp$runs, 2L)
  expect_equal(vapply(cmp$runs, `[[`, "", "label"), c("fw", "lb"))
  # steps count oracle steps: the em row shares the fw row's step
  expect_equal(as.integer(cmp$runs[[1]]$step), c(1L, 1L))
  # ungrouped runs are coloured by position, all solid
  expect_equal(vapply(cmp$runs, `[[`, 1L, "colour"), c(1L, 2L))
  expect_equal(vapply(cmp$runs, `[[`, 1L, "dash"), c(1L, 1L))
  expect_null(cmp$legend$colour)
  expect_null(cmp$legend$dash)
  # and nothing truthy crosses for them: no "colour":{} in the JSON
  expect_false(grepl("\"colour\":{}", ripr_compare(cmp)$x$data, fixed = TRUE))
  expect_true(all(is.na(cmp$runs[[1]]$time)))
  expect_equal(as.numeric(cmp$runs[[2]]$time), c(0.25, 0.75))
  expect_true(cmp$has_time)
  expect_false(ripr_compare_data(list(untimed))$has_time)
})

test_that("compare payload numbers a trace without counters by row", {
  tr <- tiny_state()$trace[, c("phase", "kl", "gap")]
  expect_equal(as.integer(ripr_compare_data(tr)$runs[[1]]$step), c(1L, 2L))
  tr$phase[1] <- "init"
  expect_equal(as.integer(ripr_compare_data(tr)$runs[[1]]$step), c(0L, 1L))
})

test_that("compare payload groups runs by colour and dash", {
  runs <- rep(list(tiny_state()), 6L)
  cmp <- ripr_compare_data(
    runs,
    colour_by = rep(c("a", "b"), 3L),
    dash_by = factor(rep(c("x", "y", "z"), each = 2L), levels = c("z", "y", "x"))
  )
  expect_equal(as.character(cmp$legend$colour), c("a", "b"))
  expect_equal(as.character(cmp$legend$dash), c("z", "y", "x"))
  expect_equal(vapply(cmp$runs, `[[`, 1L, "colour"), rep(1:2, 3L))
  expect_equal(vapply(cmp$runs, `[[`, 1L, "dash"), rep(3:1, each = 2L))
  expect_error(
    ripr_compare_data(runs, colour_by = "a"), "one entry per run"
  )
  # two colour levels over six runs is not too many colours
  expect_no_warning(ripr_compare_data(runs, colour_by = rep(c("a", "b"), 3L)))
})

test_that("compare payload accepts a bare trace or a finish()-style list", {
  tr <- tiny_state()$trace
  cmp <- ripr_compare_data(list(a = tr))
  expect_equal(as.numeric(cmp$runs[[1]]$kl), c(0.5, 0.4))
  expect_equal(as.numeric(cmp$runs[[1]]$time), c(0.25, 0.75))
  expect_length(ripr_compare_data(list(trace = tr))$runs, 1L)
  # a single run need not be wrapped in a list
  expect_length(ripr_compare_data(tr)$runs, 1L)
})

test_that("compare payload defaults and validates labels", {
  cmp <- ripr_compare_data(list(tiny_state(), tiny_state()))
  expect_equal(vapply(cmp$runs, `[[`, "", "label"), c("run 1", "run 2"))
  expect_error(
    ripr_compare_data(list(tiny_state()), labels = c("a", "b")),
    "one entry per run"
  )
  expect_error(ripr_compare_data(list(list(kl = 1))), "carry one")
  expect_error(
    ripr_compare_data(list(data.frame(kl = 1))),
    "`phase`, `kl` and `gap`"
  )
  expect_warning(
    ripr_compare_data(rep(list(tiny_state()), 6L)),
    "at most five"
  )
})

test_that("ripr_compare builds a widget whose nulls survive", {
  untimed <- tiny_state()
  untimed$trace$elapsed <- NULL
  cmp <- ripr_compare_data(list(fw = untimed, lb = tiny_state()))
  w <- ripr_compare(cmp)
  expect_s3_class(w, "htmlwidget")
  x <- payload_of(w)
  expect_length(x$runs, 2L)
  expect_null(x$runs[[1]]$time[[1]])
  expect_null(x$runs[[1]]$gap[[2]])
  expect_true(x$has_time)
  expect_false(grepl("\"NA\"", w$x$data, fixed = TRUE))
  # a one-row run still crosses as arrays
  one <- tiny_state()
  one$trace <- one$trace[1, ]
  x1 <- payload_of(ripr_compare(ripr_compare_data(list(one))))
  expect_length(x1$runs[[1]]$kl, 1L)
  expect_error(ripr_compare(list(runs = list())), "payload from")
})

test_that("ripr_compare renders to tags", {
  skip_if_not_installed("htmltools")
  expect_no_error(htmltools::as.tags(ripr_compare(ripr_compare_data(list(
    fw = tiny_state(), lb = tiny_state()
  )))))
})
