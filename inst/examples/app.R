# A minimal Shiny app exercising all three widgets and, in particular, the
# re-render path: switching the number of fit iterations re-renders ripr_fit,
# which must tear down its play loop rather than leave it accelerating.
# Run with: shiny::runApp(system.file("examples", package = "riprvis"))

library(shiny)
library(ripr)
library(riprvis)

family <- multinomial_family(n_trials = 20L, k = 3L)
plurality <- null_model(
  family,
  lapply(2:3, function(j) {
    vertices <- diag(3)
    vertices[, 1L] <- replace(numeric(3), c(1L, j), 0.5)
    simplex_region(vertices = vertices)
  })
)
q <- c(0.40, 0.34, 0.26)
lattice <- ripr_lattice_data(family)
problem <- ripr_problem_data(
  plurality,
  q = q,
  title = "plurality",
  part_labels = c("θ₁ ≤ θ₂", "θ₁ ≤ θ₃")
)

fit_state <- function(iters) {
  set.seed(1L)
  state <- ripr_init(
    family(q), plurality,
    control = ripr_control(snapshot = "all")
  )
  for (i in seq_len(iters)) {
    state <- state |>
      fw_step(record_gap = TRUE) |>
      em_step(record_gap = TRUE)
  }
  state
}

ui <- fluidPage(
  titlePanel("riprvis"),
  sliderInput("iters", "fit iterations", min = 2L, max = 15L, value = 8L),
  riprFitOutput("fit", height = "540px"),
  riprCertifyOutput("certify", height = "580px"),
  riprProblemOutput("problem", height = "440px")
)

server <- function(input, output, session) {
  state <- reactive(fit_state(input$iters))

  output$problem <- renderRiprProblem(ripr_problem(problem))

  output$fit <- renderRiprFit({
    fit <- ripr_fit_data(state(), lattice, q = q)
    ripr_fit(problem, fit, lattice)
  })

  output$certify <- renderRiprCertify({
    finished <- ripr_finish(
      state(),
      reoptimise = TRUE, identify = TRUE, record_gap = TRUE
    )
    x <- likelihood(family(q), label = "Q") /
      likelihood(finished$P_star, label = "P*")
    nodes <- certify_trace(x, plurality, tol = 1e-9)
    ripr_certify(problem, ripr_certify_data(nodes, tol = 1e-9))
  })
}

shinyApp(ui, server)
