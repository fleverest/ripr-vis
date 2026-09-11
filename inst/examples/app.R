# A minimal Shiny app exercising the three simplex widgets and the comparison
# widget and, in particular, the re-render path: switching the number of fit
# iterations re-renders ripr_fit_simplex, which must tear down its play loop
# rather than leave it accelerating.
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
problem <- ripr_problem_simplex_data(
  plurality,
  q = q,
  title = "plurality",
  part_labels = c("θ₁ ≤ θ₂", "θ₁ ≤ θ₃")
)

# A Li--Barron step costs several Frank--Wolfe steps, which is the point of
# the comparison and also why the slider tops out where it does.
fit_state <- function(iters, add = fw_step) {
  set.seed(1L)
  state <- ripr_init(
    family(q), plurality,
    record_gap = TRUE,
    control = ripr_control(snapshot = "all")
  )
  for (i in seq_len(iters)) {
    state <- state |>
      add() |>
      em_step(record_gap = TRUE)
  }
  state
}

ui <- fluidPage(
  titlePanel("riprvis"),
  sliderInput("iters", "fit iterations", min = 2L, max = 15L, value = 8L),
  riprFitSimplexOutput("fit", height = "540px"),
  riprCompareOutput("compare", height = "520px"),
  riprCertifySimplexOutput("certify", height = "580px"),
  riprProblemSimplexOutput("problem", height = "440px")
)

server <- function(input, output, session) {
  state <- reactive(fit_state(input$iters))

  output$problem <- renderRiprProblemSimplex(ripr_problem_simplex(problem))

  output$fit <- renderRiprFitSimplex({
    fit <- ripr_fit_simplex_data(state(), lattice, q = q)
    ripr_fit_simplex(problem, fit, lattice)
  })

  output$compare <- renderRiprCompare({
    ripr_compare(ripr_compare_data(list(
      "Frank–Wolfe + EM" = state(),
      "Li–Barron + EM" = fit_state(input$iters, add = lb_step)
    )))
  })

  output$certify <- renderRiprCertifySimplex({
    finished <- ripr_finish(
      state(),
      reoptimise = TRUE, identify = TRUE, record_gap = TRUE
    )
    x <- likelihood(family(q), label = "Q") /
      likelihood(finished$P_star, label = "P*")
    nodes <- certify_trace(x, plurality, tol = 1e-9)
    ripr_certify_simplex(problem, ripr_certify_simplex_data(nodes, tol = 1e-9))
  })
}

shinyApp(ui, server)
