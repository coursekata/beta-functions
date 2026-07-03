#pak::pak("coursekata/coursekata-r")

library(coursekata)
library(ggformula)
library(ggplot2)
library(here)

here::i_am("reference/img_generators/gen_img_distribution_parts.R")
img_dir <- here("reference", "img")

save_plot <- function(p, filename, width = 4.5, height = 3) {
  ggsave(file.path(img_dir, filename), plot = p, width = width, height = height, dpi = 150)
  cat("saved:", filename, "\n")
}

# Shared sampling distribution used throughout
set.seed(42)
sdob1  <- do(1000) * b1(shuffle(Tip) ~ Condition, data = TipExperiment)
obs_b1 <- b1(Tip ~ Condition, data = TipExperiment)

# ── 1. middle() ────────────────────────────────────────────────────────────────
p1 <- gf_histogram(~ b1, data = sdob1, fill = ~ middle(b1, .95))
save_plot(p1, "dist_parts_middle.png")

# ── 2. tails() ─────────────────────────────────────────────────────────────────
p2 <- gf_histogram(~ b1, data = sdob1, fill = ~ tails(b1, .05))
save_plot(p2, "dist_parts_tails.png")

# ── 3. upper() ─────────────────────────────────────────────────────────────────
p3 <- gf_histogram(~ b1, data = sdob1, fill = ~ upper(b1, .05))
save_plot(p3, "dist_parts_upper.png")

# ── 4. lower() ─────────────────────────────────────────────────────────────────
p4 <- gf_histogram(~ b1, data = sdob1, fill = ~ lower(b1, .05))
save_plot(p4, "dist_parts_lower.png")

# ── 5. With show_cutoffs() and observed b1 ────────────────────────────────────
p5 <- gf_histogram(~ b1, data = sdob1, fill = ~ middle(b1, .95)) %>%
  show_cutoffs(labels = TRUE) %>%
  gf_point(0 ~ obs_b1, color = "red", size = 4)
save_plot(p5, "dist_parts_with_cutoffs.png")

# ── 6. Confidence interval via resample() ─────────────────────────────────────
set.seed(42)
sdob1_boot <- do(1000) * b1(Tip ~ Condition, data = resample(TipExperiment))

p6 <- gf_histogram(~ b1, data = sdob1_boot, fill = ~ middle(b1, .95), bins = 100) +
  labs(title = "Bootstrap CI: middle 95% of resampled b1s")
save_plot(p6, "dist_parts_ci.png")

cat("\nAll images saved to", img_dir, "\n")
