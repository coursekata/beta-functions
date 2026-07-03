#pak::pak("coursekata/coursekata-r")

library(coursekata)
library(ggformula)
library(ggplot2)
library(here)

here::i_am("reference/img_generators/gen_img_show_cutoffs.R")
img_dir <- here("reference", "img")

save_plot <- function(p, filename, width = 4.5, height = 3) {
  ggsave(file.path(img_dir, filename), plot = p, width = width, height = height, dpi = 150)
  cat("saved:", filename, "\n")
}

# Shared sampling distribution used throughout
set.seed(42)
sdob1   <- do(1000) * b1(shuffle(Tip) ~ Condition, data = TipExperiment)
obs_b1  <- b1(Tip ~ Condition, data = TipExperiment)

# ── 1. Middle 95% with cutoff markers ─────────────────────────────────────────
p1 <- gf_histogram(~ b1, data = sdob1, fill = ~ middle(b1, .95)) %>%
  show_cutoffs()
save_plot(p1, "show_cutoffs_middle.png")

# ── 2. With labels ────────────────────────────────────────────────────────────
p2 <- gf_histogram(~ b1, data = sdob1, fill = ~ middle(b1, .95)) %>%
  show_cutoffs(labels = TRUE)
save_plot(p2, "show_cutoffs_labels.png")

# ── 3. Overlay observed b1 ────────────────────────────────────────────────────
p3 <- gf_histogram(~ b1, data = sdob1, fill = ~ middle(b1, .95)) %>%
  show_cutoffs() %>%
  gf_point(0 ~ obs_b1, color = "red", size = 4)
save_plot(p3, "show_cutoffs_observed.png")

# ── 4. One-tailed upper ───────────────────────────────────────────────────────
p4 <- gf_histogram(~ b1, data = sdob1, fill = ~ upper(b1, .05)) %>%
  show_cutoffs(labels = TRUE)
save_plot(p4, "show_cutoffs_upper.png")

# ── 5. Lenient alpha (.30) ────────────────────────────────────────────────────
p5 <- gf_histogram(~ b1, data = sdob1, fill = ~ middle(b1, .70)) %>%
  show_cutoffs()
save_plot(p5, "show_cutoffs_alpha30.png")

cat("\nAll images saved to", img_dir, "\n")
