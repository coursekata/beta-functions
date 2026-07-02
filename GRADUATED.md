# Graduated functions

Functions listed here started as beta experiments in this repo and have since been
merged into the [coursekata-r](https://github.com/coursekata/coursekata-r) package.

Once you have `library(coursekata)` in your notebook, you no longer need to
`source()` these files. The `.R` files in this repo have been converted to stubs
that emit a reminder message if sourced.

---

| Function(s) | Beta file | Graduated into coursekata-r | Date |
|---|---|---|---|
| `gf_squareplot()` | `gf_squareplot.R` | `R/gf_squareplot.R` | 2026-02-25 |
| `show_cutoffs()` | `show_cutoffs.R` | `R/show_cutoffs.R` | 2026-02-25 |
| `gf_sd_ruler()` | `gf_sd_ruler.R` | `R/gf_sd_ruler.R` | 2026-03-04 |
| `gf_resid()`, `gf_square_resid()`, `gf_squaresid()` | `gf_resid.R`, `gf_square_resid.R` | `R/gf_resid_gf_squaresid.R` | 2026-03-09 |
| `gf_resid_fun()` | `gf_resid_fun.R` | `R/gf_resid_fun.R` | 2026-03-09 |
| `gf_square_resid_fun()` | `gf_square_resid_fun.R` | `R/gf_square_resid_fun.R` | 2026-03-09 |

---

## Still in beta

These functions are not yet in the coursekata-r package. Use `source()` to load them:

| Function(s) | File | Notes |
|---|---|---|
| `gf_lm()` | `gf_lm.R` | Extended version with categorical x support and shuffle-safe continuous x. PR candidate for ggformula. |
| `gf_lm_cat()` | `gf_lm_cat.R` | Categorical x only; explicit alternative to the dispatch in `gf_lm()`. |
| `gf_coef()`, `gf_b()` | `gf_coef.R` | Overlays b0, b1, b2 … annotations on a plot. |
| `gf_shuffle_grid()` | `gf_shuffle_grid.R` | "Spot the real data" randomization grid. |
| `add_sd_ruler()` | `add_sd_ruler.R` | — |
| `outer()` | `outer.R` | — |
