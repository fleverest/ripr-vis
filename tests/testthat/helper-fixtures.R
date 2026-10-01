# Small real ripr fits, built once per test run and shared.

fixtures <- new.env()

cached <- function(name, build) {
  if (is.null(fixtures[[name]])) fixtures[[name]] <- build()
  fixtures[[name]]
}

# The K = 3 plurality null: candidate 1 does not win outright.
simplex_fixture <- function() {
  cached("simplex", function() {
    family <- ripr::multinomial_family(n_trials = 6L, k = 3L)
    part <- function(j) {
      v <- diag(3)
      v[1L, ] <- replace(numeric(3), c(1L, j), 0.5)
      ripr::simplex_region(vertices = v)
    }
    null <- ripr::null_model(family, part(2) | part(3))
    q <- c(0.4, 0.34, 0.26)
    Q <- family(q)
    set.seed(1L)
    state <- ripr::ripr_init(
      Q, null,
      record_gap = TRUE,
      control = ripr::ripr_control(snapshot = "all")
    )
    for (i in 1:3) {
      state <- state |>
        ripr::fw_step(record_gap = TRUE) |>
        ripr::em_step()
    }
    fit <- ripr::ripr_finish(state, reoptimise = TRUE, record_gap = TRUE)
    x <- ripr::likelihood(Q) / ripr::likelihood(fit@P_star)
    nodes <- ripr::certify_trace(x, null, tol = 1e-6)
    list(
      family = family, null = null, q = q, Q = Q,
      state = state, fit = fit, nodes = nodes
    )
  })
}

# Four darts in the plane under a Gaussian family.
plane_fixture <- function() {
  cached("plane", function() {
    gauss <- ripr::gaussian_family(d = 2L)
    dart <- function(axis, sgn) {
      e <- c(0, 0)
      e[axis] <- sgn
      perp <- c(0, 0)
      perp[3L - axis] <- 1
      ripr::polyhedron_region(
        vertices = rbind(e, 1.75 * e + 0.6 * perp, 1.75 * e - 0.6 * perp),
        rays = rbind(e)
      )
    }
    null <- ripr::null_model(
      gauss, list(dart(1, 1), dart(1, -1), dart(2, 1), dart(2, -1))
    )
    set.seed(2L)
    state <- ripr::ripr_init(
      gauss(c(0, 0)), null,
      engine = ripr::gh_engine(8L),
      record_gap = TRUE,
      control = ripr::ripr_control(snapshot = "all")
    ) |>
      ripr::fw_step(times = 2L, record_gap = TRUE)
    list(family = gauss, null = null, state = state)
  })
}

# The payload a widget would ship, parsed back the way the browser reads it.
payload_of <- function(w) {
  jsonlite::fromJSON(w$x$data, simplifyVector = FALSE)
}
