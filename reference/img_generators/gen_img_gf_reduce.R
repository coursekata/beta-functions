#pak::pak("coursekata/coursekata-r")


library(coursekata)
library(ggformula)
library(ggplot2)
library(here)

here::i_am("reference/img_generators/gen_img_gf_reduce.R")
img_dir <- here("reference", "img")

source(here("gf_reduce.R"))

save_plot <- function(p, filename, width = 4.5, height = 3) {
  ggsave(file.path(img_dir, filename), plot = p, width = width, height = height, dpi = 150)
  cat("saved:", filename, "\n")
}

# Small continuous dataset used throughout
set.seed(1)
d <- Fingers[sample(nrow(Fingers), 10), ]
m_empty   <- lm(Thumb ~ NULL,   data = d)
m_complex <- lm(Thumb ~ Height, data = d)

# ── 1. SS Model lines ─────────────────────────────────────────────────────────
p1 <- gf_point(Thumb ~ Height, data = d) %>%
  gf_model(m_empty) %>%
  gf_model(m_complex) %>%
  gf_reduce(m_complex, color = "darkgreen") +
  labs(title = "SS Model lines: grand mean → model predictions")
save_plot(p1, "gf_reduce_lines.png")

# ── 2. SS Model squares ───────────────────────────────────────────────────────
p2 <- gf_point(Thumb ~ Height, data = d) %>%
  gf_model(m_empty) %>%
  gf_model(m_complex) %>%
  gf_square_reduce(m_complex, fill = "darkgreen", color = "darkgreen", alpha = 0.3) +
  labs(title = "SS Model squares")
save_plot(p2, "gf_reduce_squares.png")

# ── 3. Full SS decomposition ──────────────────────────────────────────────────
p3 <- gf_point(Thumb ~ Height, data = d) %>%
  gf_model(m_empty) %>%
  gf_model(m_complex) %>%
  gf_square_resid(m_empty,    fill = "blue",  color = "blue",  alpha = 0.15) %>%
  gf_square_resid(m_complex,  fill = "red",   color = "red",   alpha = 0.15) %>%
  gf_square_reduce(m_complex, fill = "green", color = "green", alpha = 0.15) +
  labs(title = "SS Total (blue) = SS Error (red) + SS Model (green)")
save_plot(p3, "gf_reduce_decomp.png")

# ── 4. Categorical x: full decomposition ─────────────────────────────────────
set.seed(1)
tip_small <- TipExperiment[sample(nrow(TipExperiment), 20), ]
m_cat_empty   <- lm(Tip ~ NULL,      data = tip_small)
m_cat_complex <- lm(Tip ~ Condition, data = tip_small)

p4 <- gf_point(Tip ~ Condition, data = tip_small, alpha = 0.5) %>%
  gf_model(m_cat_empty) %>%
  gf_model(m_cat_complex) %>%
  gf_square_resid(m_cat_empty,    fill = "blue",  color = "blue",  alpha = 0.1) %>%
  gf_square_resid(m_cat_complex,  fill = "red",   color = "red",   alpha = 0.1) %>%
  gf_square_reduce(m_cat_complex, fill = "green", color = "green", alpha = 0.1) +
  labs(title = "Categorical x: full decomposition")
save_plot(p4, "gf_reduce_cat.png")

cat("\nAll images saved to", img_dir, "\n")
