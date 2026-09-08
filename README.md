# ripr-vis

Interactive visualisations of reverse information projection (RIPr) fits and
their branch-and-bound certification, driven by
[`ripr`](https://github.com/fleverest/ripr). This repository holds two
things:

## The `riprvis` R package

An [htmlwidgets](https://www.htmlwidgets.org) package (at the repository
root) that renders the visualisations in the RStudio viewer, R Markdown and
Quarto documents, and Shiny:

- `ripr_problem_simplex()` — the null region and the alternative's support
  on the ternary simplex.
- `ripr_fit_simplex()` — the Bernstein field \(G_i(\theta) = E_\theta[Q/P_i]\)
  as an animated contour plot, with KL / gap / growth diagnostics.
- `ripr_certify_simplex()` — the branch-and-bound search: the evolving cell
  partition, the closing enclosure window, and the search tree.
- `ripr_compare()` — several fits of one problem (Frank–Wolfe against
  Li–Barron, say) as overlaid KL / gap / growth traces, against the oracle
  step count or the clock time `ripr` records in the trace; two crossed
  factors can be told apart by colour and line style.

Planar analogues cover two-dimensional families such as
`ripr::gaussian_family()` with polyhedral nulls, including unbounded parts
given by vertices plus recession rays:

- `ripr_problem2d()` — the null's polyhedra and the alternative on the
  parameter plane, with the viewport sized to the parts' vertices (plus a
  margin) so rays simply run off the edge.
- `ripr_fit2d()` — the same animated field view as `ripr_fit_simplex()`, with
  the field evaluated in R on the plotting grid using the fit's own
  quadrature.

Each widget takes a plain-list payload, built either by hand or from real
`ripr` objects with `ripr_problem_simplex_data()`, `ripr_lattice_data()`,
`ripr_fit_simplex_data()`, `ripr_certify_simplex_data()`, `ripr_compare_data()`,
`ripr_problem2d_data()` and `ripr_fit2d_data()`; only the data helpers touch
`ripr`, so saved payloads render without it.

```r
# install.packages("remotes")
remotes::install_github("fleverest/ripr")     # for computing new payloads
remotes::install_github("fleverest/ripr-vis")
```

See `vignette("riprvis")` for a full worked example, and
`system.file("examples", "app.R", package = "riprvis")` for a Shiny app.

## The slide deck

Ten revealjs slides (`search-slides.qmd`) driven by live `ripr` runs. Two
multinomial examples, each stated, fitted, then certified -- the first also
fitted several other ways, for comparison -- and a Gaussian example with an
unbounded polyhedral null, stated and fitted:

1. The plurality problem
2. Fitting the projection (plurality)
3. Certifying the fit (plurality)
4. Comparing the oracle rules (plurality, Frank–Wolfe against Li–Barron)
5. Comparing Frank–Wolfe variants (plurality: direction sets by step sizes)
6. The medial-triangle problem
7. Fitting the projection (medial)
8. Certifying the fit (medial)
9. The cup problem
10. Fitting the projection (cup)

View the slides on the
[GitHub Pages site](https://fleverest.github.io/ripr-vis).
