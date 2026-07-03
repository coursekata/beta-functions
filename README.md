# CourseKata `beta-functions`

This repository hosts experimental R functions being tested in CourseKata class materials before graduating to the [coursekata-r](https://github.com/coursekata/coursekata-r) package.

Think of these as **beta features**: lightweight, pedagogically focused, subject to change, and open to feedback.

---

## How to use

No package install needed. Load any function directly in a Jupyter notebook:

```r
source("https://raw.githubusercontent.com/coursekata/beta-functions/refs/heads/main/gf_lm.R")
```

---

## Active beta functions

These are available now via `source()`. Each has a reference page with examples, argument descriptions, and teaching tips.

| Function(s) | Source file | Reference page |
|---|---|---|
| `gf_lm()` | [`gf_lm.R`](gf_lm.R) | [`reference/gf_lm.md`](reference/gf_lm.md) |
| `gf_lm_cat()` | [`gf_lm_cat.R`](gf_lm_cat.R) | — |
| `gf_coef()`, `gf_b()` | [`gf_coef.R`](gf_coef.R) | [`reference/gf_coef.md`](reference/gf_coef.md) |
| `gf_shuffle_grid()` | [`gf_shuffle_grid.R`](gf_shuffle_grid.R) | [`reference/gf_shuffle_grid.md`](reference/gf_shuffle_grid.md) |
| `gf_reduce()`, `gf_square_reduce()`, `gf_squareduce()` | [`gf_reduce.R`](gf_reduce.R) | [`reference/gf_reduce.md`](reference/gf_reduce.md) |
| `gf_sd_ruler()` (extended) | [`gf_sd_ruler.R`](gf_sd_ruler.R) | [`reference/gf_sd_ruler.md`](reference/gf_sd_ruler.md) |

> **`gf_sd_ruler()` note:** the base version is already in `library(coursekata)`. Source `gf_sd_ruler.R` here to get the extended version with histogram support.

---

## Graduated functions

These started here and have since moved into the [coursekata-r](https://github.com/coursekata/coursekata-r) package. They are available automatically after `library(coursekata)` — you no longer need to `source()` them.

| Function(s) | Now in coursekata-r | Reference page |
|---|---|---|
| `gf_squareplot()` | [`R/gf_squareplot.R`](https://github.com/coursekata/coursekata-r/blob/main/R/gf_squareplot.R) | [`reference/gf_squareplot.md`](reference/gf_squareplot.md) |
| `gf_resid()`, `gf_square_resid()`, `gf_squaresid()` | [`R/gf_resid_gf_squaresid.R`](https://github.com/coursekata/coursekata-r/blob/main/R/gf_resid_gf_squaresid.R) | — |
| `gf_resid_fun()` | [`R/gf_resid_fun.R`](https://github.com/coursekata/coursekata-r/blob/main/R/gf_resid_fun.R) | — |
| `gf_square_resid_fun()` | [`R/gf_square_resid_fun.R`](https://github.com/coursekata/coursekata-r/blob/main/R/gf_square_resid_fun.R) | — |
| `gf_sd_ruler()` (base) | [`R/gf_sd_ruler.R`](https://github.com/coursekata/coursekata-r/blob/main/R/gf_sd_ruler.R) | [`reference/gf_sd_ruler.md`](reference/gf_sd_ruler.md) |
| `show_cutoffs()` | [`R/show_cutoffs.R`](https://github.com/coursekata/coursekata-r/blob/main/R/show_cutoffs.R) | — |
| `outer()` | [`R/distribution_parts.R`](https://github.com/coursekata/coursekata-r/blob/main/R/distribution_parts.R) | — |

See [GRADUATED.md](GRADUATED.md) for the full graduation history.

---

## Contributing

If you have ideas for a new beta function:
1. Open an issue with a short description and use case.
2. Or submit a pull request with the function in its own `.R` file.

Keep functions small, focused, and pedagogically motivated. A reference page in `reference/` is encouraged — see the existing ones for the format.
