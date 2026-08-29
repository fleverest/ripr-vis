# ripr-vis

Interactive visualisations of reverse information projection (RIPr) fits and
their branch-and-bound certification, driven by
[`ripr`](https://github.com/fleverest/ripr). This repository holds two
things:

## The `riprvis` R package

An [htmlwidgets](https://www.htmlwidgets.org) package (at the repository
root) that renders the visualisations in the RStudio viewer, R Markdown and
Quarto documents, and Shiny:

- `ripr_problem()` — the null region and the alternative's support on the
  ternary simplex.
- `ripr_fit()` — the Bernstein field \(G_i(\theta) = E_\theta[Q/P_i]\) as an
  animated contour plot, with KL / gap / log-growth diagnostics.
- `ripr_certify()` — the branch-and-bound search: the evolving cell
  partition, the closing enclosure window, and the search tree.

Each widget takes a plain-list payload, built either by hand or from real
`ripr` objects with `ripr_problem_data()`, `ripr_lattice_data()`,
`ripr_fit_data()` and `ripr_certify_data()`; only the data helpers touch
`ripr`, so saved payloads render without it.

```r
# install.packages("remotes")
remotes::install_github("fleverest/ripr")     # for computing new payloads
remotes::install_github("fleverest/ripr-vis")
```

See `vignette("riprvis")` for a full worked example, and
`system.file("examples", "app.R", package = "riprvis")` for a Shiny app.

## The slide deck

Six revealjs slides (`search-slides.qmd`) driven by live `ripr` runs. Two
examples, each stated, fitted, then certified:

1. The plurality problem
2. Fitting the projection (plurality)
3. Certifying the fit (plurality)
4. The medial-triangle problem
5. Fitting the projection (medial)
6. Certifying the fit (medial)

View the slides on the
[GitHub Pages site](https://fleverest.github.io/ripr-vis).
