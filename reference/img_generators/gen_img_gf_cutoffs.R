#pak::pak("coursekata/coursekata-r")

library(coursekata)
library(ggformula)
library(ggplot2)
library(here)

source(here("gf_cutoffs.R"))

here::i_am("reference/img_generators/gen_img_gf_cutoffs.R")
img_dir <- here("reference", "img")

save_plot <- function(p, filename, width = 4.5, height = 3) {
  ggsave(file.path(img_dir, filename), plot = p, width = width, height = height, dpi = 150)
  cat("saved:", filename, "\n")
}

# Shared sampling distribution used throughout
set.seed(42)
sdob1  <- do(1000) * b1(shuffle(Tip) ~ Condition, data = TipExperiment)
obs_b1 <- b1(Tip ~ Condition, data = TipExperiment)

# ── 1. Auto-detect mode: fill drives the cutoffs ──────────────────────────────
p1 <- gf_histogram(~ b1, data = sdob1, fill = ~ middle(b1, .95)) %>%
  gf_cutoffs()
save_plot(p1, "gf_cutoffs_autodetect.png")

# ── 2. Explicit mode: expression passed directly, no fill needed ──────────────
p2 <- gf_histogram(~ b1, data = sdob1) %>%
  gf_cutoffs(middle(b1, .95))
save_plot(p2, "gf_cutoffs_explicit.png")

# ── 3. With labels ────────────────────────────────────────────────────────────
p3 <- gf_histogram(~ b1, data = sdob1, fill = ~ middle(b1, .95)) %>%
  gf_cutoffs(labels = TRUE)
save_plot(p3, "gf_cutoffs_labels.png")

# ── 4. Overlay observed b1 ────────────────────────────────────────────────────
p4 <- gf_histogram(~ b1, data = sdob1, fill = ~ middle(b1, .95)) %>%
  gf_cutoffs() %>%
  gf_point(0 ~ obs_b1, color = "red", size = 4)
save_plot(p4, "gf_cutoffs_observed.png")

# ── 5. One-tailed upper ───────────────────────────────────────────────────────
p5 <- gf_histogram(~ b1, data = sdob1, fill = ~ upper(b1, .05)) %>%
  gf_cutoffs(labels = TRUE)
save_plot(p5, "gf_cutoffs_upper.png")

# ── 6. Stacking two alpha levels ──────────────────────────────────────────────
p6 <- gf_histogram(~ b1, data = sdob1) %>%
  gf_cutoffs(middle(b1, .95), color = "#009d9a") %>%
  gf_cutoffs(middle(b1, .99), color = "#6929c4")
save_plot(p6, "gf_cutoffs_stacked.png")

cat("\nAll images saved to", img_dir, "\n")
