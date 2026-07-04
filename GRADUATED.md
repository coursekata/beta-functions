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
| `gf_resid()`, `gf_square_resid()`, `gf_squaresid()` | `gf_resid.R`, `gf_square_resid.R` | `R/gf_resid_gf_squaresid.R` | 2026-03-09 (see note below) |
| `gf_resid_fun()` | `gf_resid_fun.R` | `R/gf_resid_fun.R` | 2026-03-09 |
| `gf_square_resid_fun()` | `gf_square_resid_fun.R` | `R/gf_square_resid_fun.R` | 2026-03-09 |
| `outer()` | `outer.R` | `R/distribution_parts.R` | 2026-02-25 |

---

## Still in beta

These functions are not yet in the coursekata-r package. Use `source()` to load them:

| Function(s) | File | Notes |
|---|---|---|
| `gf_lm()` | `gf_lm.R` | Extended version with categorical x support and shuffle-safe continuous x. PR candidate for ggformula. |
| `gf_lm_cat()` | `gf_lm_cat.R` | Categorical x only; explicit alternative to the dispatch in `gf_lm()`. |
| `gf_coef()`, `gf_b()` | `gf_coef.R` | Overlays b0, b1, b2 … annotations on a plot. |
| `gf_shuffle_grid()` | `gf_shuffle_grid.R` | "Spot the real data" randomization grid. |
| `gf_reduce()`, `gf_square_reduce()`, `gf_squareduce()` | `gf_reduce.R` | Not yet submitted. |
| `gf_resid()`, `gf_square_resid()` (working copy) | `gf_resid_gf_squaresid.R` | Temporary copy of the graduated coursekata-r code, brought back to develop the jitter-alignment fix together with `gf_reduce.R` (see `tests/test_resid_square_alignment.ipynb`). Sourcing it masks the package versions. Will become a PR to coursekata-r; the stub files `gf_resid.R` / `gf_square_resid.R` are untouched. |
| `gf_sd_ruler()` (extended) | `gf_sd_ruler.R` | Extends coursekata-r version with histogram support (auto-detects axis orientation). PR candidate. Supersedes `add_sd_ruler.R`. |
| `gf_cutoffs()` | `gf_cutoffs.R` | Cleaner replacement for `show_cutoffs()`: explicit expression arg, auto-detect fallback, stackable. PR candidate. |
