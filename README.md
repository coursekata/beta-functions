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

<table>
  <thead>
    <tr>
      <th>Function</th>
      <th>Description</th>
      <th>Example Usage</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td><code>gf_resid()</code></td>
      <td>Adds vertical residual lines from an <code>lm()</code> model to a <code>gf_point()</code> or <code>gf_jitter()</code> plot.</td>
      <td>
        <pre lang="r"><code>gf_point(Thumb ~ Height, data = Fingers) %>%
  gf_model(lm(Thumb ~ Height, data = Fingers)) %>%
  gf_resid(lm(Thumb ~ Height, data = Fingers))</code></pre>
      </td>
    </tr>
    <tr>
      <td><code>gf_resid_fun()</code></td>
      <td>Adds vertical residual lines using a <b>function</b> (e.g., <code>function(x){ 2 + 5*x}</code>) instead of a model.</td>
      <td>
        <pre lang="r"><code>my_function <- function(X){-3.3295 + 0.9619*X }

gf_jitter(Thumb ~ Height, data = Fingers) %>%
   gf_function(my_function) %>%
   gf_resid_fun(my_function, color = "red", alpha = 0.5)</code></pre>
      </td>
    </tr>
    <tr>
      <td><code>gf_square_resid()</code></td>
      <td>Visualizes <b>squared residuals</b> (as polygons) from an <code>lm()</code> model. Useful for teaching squared error.</td>
      <td>
        <pre lang="r"><code>gf_point(Thumb ~ Height, data = Fingers) %>%
  gf_model(lm(Thumb ~ Height, data = Fingers)) %>%
  gf_square_resid(lm(Thumb ~ Height, data = Fingers))</code></pre>
      </td>
    </tr>
    <tr>
      <td><code>gf_square_resid_fun()</code></td>
      <td>Visualizes <b>squared residuals</b> from a function (e.g., <code>gf_function()</code>).</td>
      <td>
        <pre lang="r"><code>my_function <- function(X){-3.3295 + 0.9619*X }

gf_jitter(Thumb ~ Height, data = Fingers) %>%
   gf_function(my_function) %>%
   gf_squaresid_fun(my_function, color = "red", alpha = 0.2)</code></pre>
      </td>
    </tr>
    <tr>
      <td><code>add_sd_ruler()</code></td>
      <td>Adds a vertical or horizontal <b>SD ruler</b> (red line) to a plot.</td>
      <td>
        <pre lang="r"><code>tba</code></pre>
      </td>
    </tr>
    <tr>
      <td><code>gf_sd_ruler()</code></td>
      <td>Adds a vertical SD ruler to a <code>gf_point()</code> plot, positioned at the middle, mean, or median of the x-axis.</td>
      <td>
        <pre lang="r"><code>gf_point(Thumb ~ Height, data = Fingers) %>%
  gf_sd_ruler(y = Thumb, where = "mean")</code></pre>
      </td>
    </tr>
  </tbody>
</table>

