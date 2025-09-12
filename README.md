# CourseKata `beta-functions`

Welcome to **CourseKata beta-functions** 

This repository hosts small, experimental R functions that we are testing out in our in-class materials before deciding whether to add them to the [coursekata-r](https://github.com/coursekata/coursekata-r) package.

Think of these as **beta features**:
- lightweight
- work-in-progress
- subject to change or removal
- feedback welcome (if things don't work right or you love it, please let us know!)

They are here so instructors, students, and collaborators can try them out early and give feedback.

---

## What are beta-functions?

In CourseKata, we often prototype little helper functions to make statistical ideas more concrete in class:
- Extra visualization helpers for basic statistics concepts
- Adding residual lines or squared residuals to `gf_point()` plots  
- Other teaching-oriented tools that might be useful, but aren’t ready for prime time  

Instead of waiting until these are fully polished, we’re sharing them here so they can be used **immediately** in class materials.

---

## How to Use

You don’t need to install a package. You can just load the functions directly into your notebook with `source()`.

For example, adding these lines to a Jupyter notebook will make `gf_resid()` and `gf_square_resid()` available:

```r
source("https://raw.githubusercontent.com/coursekata/beta-functions/refs/heads/main/gf_resid.R")
source("https://raw.githubusercontent.com/coursekata/beta-functions/refs/heads/main/gf_square_resid.R")
```

We’ll keep each function in its own file so it’s easy to drop into class notebooks or Jupyter cells.

---

## Contributing

If you have ideas for a new beta function:
1. Open an issue with a short description and use case.
2. Or submit a pull request with the function in its own .R file.

Keep in mind:
- Functions should be small, focused, and pedagogical.
- Add a short comment block at the top (like #' @param, #' @examples) so we can easily convert to documentation later.

---

## Current Functions

Here’s what’s available so far:

| Function               | Description                                                                 | Example Usage |
|------------------------|-----------------------------------------------------------------------------|---------------|
| `add_sd_ruler()`       | Adds a vertical or horizontal “**SD ruler**” (red line) to a plot, with optional text label. | ```gf_point(Thumb ~ Height, data = Fingers) %>%  add_sd_ruler(Thumb, legend_x = 65, legend_y = 50)\n``` |
| `gf_sd_ruler()`        | Adds a vertical SD ruler to a `gf_point()` plot, positioned at the middle/mean/median of the x-axis. | ```r\ngf_point(Thumb ~ Height, data = Fingers) %>%\n  gf_sd_ruler(y = Thumb, where = "mean")\n``` |
| `gf_resid()`           | Adds vertical residual lines from an **lm()** model to a `gf_point()`/`gf_jitter()` plot. | ```r\ngf_point(Thumb ~ Height, data = Fingers) %>%\n  gf_model(lm(Thumb ~ Height, data = Fingers)) %>%\n  gf_resid(lm(Thumb ~ Height, data = Fingers))\n``` |
| `gf_resid_fun()`       | Adds vertical residual lines using a **function** (e.g., `function(x) 2 + 5*x`) instead of a model. | ```r\ngf_point(Y ~ X, data = df) %>%\n  gf_function(function(x) 2 + 5*x) %>%\n  gf_resid_fun(function(x) 2 + 5*x)\n``` |
| `gf_square_resid()`    | Visualizes **squared residuals** (as polygons) from an **lm()** model. Useful for teaching squared error. | ```r\ngf_point(Thumb ~ Height, data = Fingers) %>%\n  gf_model(lm(Thumb ~ Height, data = Fingers)) %>%\n  gf_square_resid(lm(Thumb ~ Height, data = Fingers))\n``` |
| `gf_square_resid_fun()`| Visualizes **squared residuals** from a **function-based curve** (e.g., `gf_function()`). | ```r\ngf_point(Y ~ X, data = df) %>%\n  gf_function(function(x) 2 + 5*x) %>%\n  gf_square_resid_fun(function(x) 2 + 5*x)\n``` |


---



