# ripr-vis

Interactive visualisations of reverse information projection (RIPr) fits and
their branch-and-bound certification, driven by
[`ripr`](https://github.com/fleverest/ripr). This repository holds two
things:

## The `riprvis` R package

Visualisations of the objects `ripr` produces, at the repository root. Each
view has two verbs: a `vis_*()` htmlwidget for exploring, which works in the
RStudio viewer, R Markdown and Quarto documents, and Shiny; and a `plot_*()`
base-graphics plot of one frame, for print.

- `vis_problem()` / `plot_problem()`: the null's parts and the alternative's
  support.
- `vis_fit()` / `plot_fit()`: the field \(G_i(\theta) = E_\theta[Q/P_i]\)
  step by step, with the mixture's atoms and KL / gap / growth diagnostics.
- `vis_certify()` / `plot_certify()`: the branch-and-bound search: the
  evolving cell partition, the closing enclosure window, and the search tree.
- `vis_compare()` / `plot_compare()`: several fits of one problem
  (Frank–Wolfe against Li–Barron, say) as overlaid KL / gap traces, against
  oracle steps or the clock; two crossed factors can be told apart by colour
  and line style.

The family picks the geometry: a three-category multinomial is drawn on the
ternary simplex, a two-dimensional Gaussian (with polyhedral nulls, rays and
all) on the plane. Each verb takes the `ripr` object itself:

```r
vis_fit(state)          # a ripr state, run with ripr_control(snapshot = "all")
plot_fit(state, step = 3)
```

`riprvis_payload()` extracts the plain list a view draws; saved with
`saveRDS()`, it redraws with any `vis_*()` or `plot_*()` verb of the same
kind, without `ripr` installed. In Shiny, `riprvisOutput()` and
`renderRiprvis()` serve every view.

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
