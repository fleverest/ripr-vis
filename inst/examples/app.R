# A minimal Shiny app exercising every riprvis view and, in particular, the
# re-render path: switching the number of fit iterations re-renders the fit,
# which must tear down its play loop rather than leave it accelerating.
# Run with: shiny::runApp(system.file("examples", package = "riprvis"))

library(shiny)
library(ripr)
library(riprvis)

family <- multinomial_family(n_trials = 20L, k = 3L)
part <- function(j) {
  vertices <- diag(3)
  vertices[1L, ] <- replace(numeric(3), c(1L, j), 0.5)
  simplex_region(vertices = vertices)
}
plurality <- null_model(family, part(2) | part(3))
Q <- family(c(0.40, 0.34, 0.26))
labels <- c("θ₁ ≤ θ₂", "θ₁ ≤ θ₃")

# A Li--Barron step costs several Frank--Wolfe steps, which is the point of
# the comparison and also why the slider tops out where it does.
fit_state <- function(iters, add = fw_step) {
  set.seed(1L)
  state <- ripr_init(
    Q, plurality,
    record_gap = TRUE,
    control = ripr_control(snapshot = "all")
  )
  for (i in seq_len(iters)) {
    state <- state |>
      add(record_gap = TRUE) |>
      em_step(record_gap = TRUE)
  }
  state
}

ui <- fluidPage(
  titlePanel("riprvis"),
  sliderInput("iters", "fit iterations", min = 2L, max = 15L, value = 8L),
  riprvisOutput("fit", height = "540px"),
  riprvisOutput("compare", height = "520px"),
  riprvisOutput("certify", height = "580px"),
  riprvisOutput("problem", height = "440px")
)

server <- function(input, output, session) {
  state <- reactive(fit_state(input$iters))

  output$problem <- renderRiprvis(
    vis_problem(plurality, alternative = Q, part_labels = labels)
  )

  output$fit <- renderRiprvis(vis_fit(state(), part_labels = labels))

  output$compare <- renderRiprvis({
    vis_compare(list(
      "Frank–Wolfe + EM" = state(),
      "Li–Barron + EM" = fit_state(input$iters, add = lb_step)
    ))
  })

  output$certify <- renderRiprvis({
    finished <- ripr_finish(
      state(),
      reoptimise = TRUE, identify = TRUE, record_gap = TRUE
    )
    x <- likelihood(Q, label = "Q") / likelihood(finished@P_star, label = "P*")
    nodes <- certify_trace(x, plurality, tol = 1e-9)
    vis_certify(nodes, alternative = Q, part_labels = labels)
  })
}

shinyApp(ui, server)
