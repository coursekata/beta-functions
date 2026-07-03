#pak::pak("coursekata/coursekata-r")

library(coursekata)
library(ggformula)
library(ggplot2)
library(here)

here::i_am("reference/img_generators/gen_img_gf_squareplot.R")
img_dir <- here("reference", "img")

save_plot <- function(p, filename, width = 4.5, height = 3) {
  ggsave(file.path(img_dir, filename), plot = p, width = width, height = height, dpi = 150)
  cat("saved:", filename, "\n")
}

# ── 1. Basic countable histogram ───────────────────────────────────────────────
p1 <- gf_squareplot(~ Thumb, data = Fingers, binwidth = 3)
save_plot(p1, "gf_squareplot_1.png")

# ── 2. With bar outlines ───────────────────────────────────────────────────────
p2 <- gf_squareplot(~ Thumb, data = Fingers, bars = "outline", binwidth = 3)
save_plot(p2, "gf_squareplot_2.png")

# ── 3. Sampling distribution: DGP overlay, small n ────────────────────────────
# Simulate a null world: shuffle group labels and compute b1 each time.
# TipExperiment: Tip ~ Condition (two groups), so b1 = group difference.
set.seed(42)
sdob1_small <- do(10) * b1(shuffle(Tip) ~ Condition, data = TipExperiment)

p3 <- gf_squareplot(~ b1, data = sdob1_small,
  show_dgp  = TRUE,
  show_mean = TRUE,
  xrange    = c(-30, 30),
  mincount  = 10,
  binwidth  = 2)
save_plot(p3, "gf_squareplot_3.png")

# ── 4. Sampling distribution: DGP overlay, larger n ───────────────────────────
set.seed(42)
sdob1_large <- do(100) * b1(shuffle(Tip) ~ Condition, data = TipExperiment)

p4 <- gf_squareplot(~ b1, data = sdob1_large,
  show_dgp  = TRUE,
  show_mean = TRUE,
  xrange    = c(-30, 30),
  mincount  = 10,
  binwidth  = 2)
save_plot(p4, "gf_squareplot_4.png")

cat("\nAll images saved to", img_dir, "\n")
