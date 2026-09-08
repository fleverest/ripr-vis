# Plain-list stand-ins shaped like the ripr objects the prepare_*() helpers
# duck-type against, so the payload contracts are tested without ripr
# installed. The shapes mirror ripr::ripr_state (trace + snapshots),
# ripr::certify_trace() (node table + trace/incumbent_trace/certificate
# attributes) and ripr::multinomial_family (n_trials + enumerated outcomes).

# All K = 3 compositions of n: the outcome lattice of a tiny multinomial.
tiny_outcomes <- function(n = 2L) {
  g <- expand.grid(y1 = 0:n, y2 = 0:n)
  g <- g[g$y1 + g$y2 <= n, ]
  unname(as.matrix(cbind(g$y1, g$y2, n - g$y1 - g$y2)))
}

tiny_family <- function(n = 2L) {
  list(n_trials = n, outcomes = tiny_outcomes(n))
}

tiny_lattice <- function(n = 2L) {
  ripr_lattice_data(tiny_family(n))
}

# Two snapshots of a fit, with trace rows for a fw and an em step. The second
# trace row records no gap sweep, so its gap is NA and gap_theta all-NA --
# both must serialise as null, not the string "NA".
tiny_state <- function() {
  list(
    trace = data.frame(
      fw = c(1L, 1L),
      lb = c(0L, 0L),
      em = c(0L, 1L),
      weight = c(0L, 0L),
      phase = c("fw", "em"),
      kl = c(0.5, 0.4),
      gap = c(0.2, NA),
      gap_theta = I(list(c(0.2, 0.5, 0.3), NA))
    ),
    snapshots = list(
      list(
        iters = c(fw = 1L, lb = 0L, em = 0L, weight = 0L),
        phase = "fw",
        atoms = list(c(0.2, 0.5, 0.3)),
        weights = list(1)
      ),
      list(
        iters = c(fw = 1L, lb = 0L, em = 1L, weight = 0L),
        phase = "em",
        atoms = list(c(0.2, 0.5, 0.3), c(0.1, 0.6, 0.3)),
        weights = list(0.7, 0.3)
      )
    )
  )
}

# A two-cell branch-and-bound record: cell 1 split once (three nodes), cell 2
# converged immediately (one node). Ids restart per cell, as certify_trace()
# now records them.
tiny_nodes <- function() {
  tri <- function(a, b, c) cbind(a, b, c)
  list(
    nodes = list(
      part = c(1L, 1L, 1L, 2L),
      cell = c(1L, 1L, 1L, 2L),
      id = c(1L, 2L, 3L, 1L),
      parent = c(NA, 1L, 1L, NA),
      depth = c(0L, 1L, 1L, 0L),
      born = c(0L, 1L, 1L, 0L),
      retired = c(1L, NA, 2L, NA),
      fate = c("split", "active", "pruned", "active"),
      upper = c(1.5, 1.2, 1.1, 1.05),
      volume = c(1, 0.5, 0.5, 1),
      vertices = list(
        tri(c(0.5, 0.5, 0), c(0, 1, 0), c(0, 0, 1)),
        tri(c(0.5, 0.5, 0), c(0.25, 0.5, 0.25), c(0, 1, 0)),
        tri(c(0.25, 0.5, 0.25), c(0, 1, 0), c(0, 0, 1)),
        tri(c(0.5, 0, 0.5), c(0, 1, 0), c(0, 0, 1))
      )
    ),
    trace = list(c(1.5, 1.2), c(1.05)),
    incumbent_trace = list(c(1.0, 1.05), c(1.02)),
    certificate = list(
      sup_ub = 1.2,
      sup_lb = 1.05,
      iterations = c(2L, 1L)
    )
  )
}

tiny_problem <- function() {
  ripr_problem_simplex_data(
    null = list(
      cbind(c(0.5, 0.5, 0), c(0, 1, 0), c(0, 0, 1)),
      cbind(c(0.5, 0, 0.5), c(0, 1, 0), c(0, 0, 1))
    ),
    q = c(0.4, 0.34, 0.26),
    title = "tiny",
    part_labels = c("a", "b")
  )
}

# Parse a payload the way the JS binding does.
payload_of <- function(widget) {
  jsonlite::fromJSON(widget$x$data, simplifyVector = FALSE)
}
