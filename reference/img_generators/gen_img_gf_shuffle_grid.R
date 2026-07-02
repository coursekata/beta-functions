library(coursekata)
library(ggformula)
library(ggplot2)
library(here)

here::i_am("reference/img_generators/gen_img_gf_shuffle_grid.R")
img_dir <- here("reference", "img")

source(here("gf_lm_cat.R"))
source(here("gf_coef.R"))
source(here("gf_shuffle_grid.R"))

save_plot <- function(p, filename, width = 7, height = 7) {
  ggsave(file.path(img_dir, filename), plot = p, width = width, height = height, dpi = 150)
  cat("saved:", filename, "\n")
}

# ── 1. Categorical x: default (show_model = TRUE, show_coef = FALSE) ─────────
set.seed(7)
p1 <- gf_shuffle_grid(later_anxiety ~ condition, data = er)
save_plot(p1, "gf_sg_cat_default.png")

# ── 2. Categorical x: with coefficient annotations ───────────────────────────
set.seed(7)
p3 <- gf_shuffle_grid(later_anxiety ~ condition, data = er,
                      show_model = TRUE, show_coef = TRUE)
save_plot(p3, "gf_sg_cat_coef.png")

# ── 4. Categorical x: reveal = TRUE ──────────────────────────────────────────
set.seed(7)
p4 <- gf_shuffle_grid(later_anxiety ~ condition, data = er,
                      show_model = TRUE, reveal = TRUE)
save_plot(p4, "gf_sg_cat_reveal.png")

# ── 5. Continuous x: default ─────────────────────────────────────────────────
set.seed(7)
p5 <- gf_shuffle_grid(Thumb ~ Height, data = Fingers, plot = "point")
save_plot(p5, "gf_sg_cont_default.png")

# ── 6. Continuous x: with model overlay ──────────────────────────────────────
set.seed(7)
p6 <- gf_shuffle_grid(Thumb ~ Height, data = Fingers,
                      plot = "point", show_model = TRUE)
save_plot(p6, "gf_sg_cont_model.png")

# ── 7. Larger grid (4x4) ─────────────────────────────────────────────────────
set.seed(7)
p7 <- gf_shuffle_grid(later_anxiety ~ condition, data = er, nrow = 4, ncol = 4)
save_plot(p7, "gf_sg_4x4.png", width = 9, height = 9)

cat("\nAll images saved to", img_dir, "\n")
