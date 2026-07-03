#pak::pak("coursekata/coursekata-r")

library(coursekata)
library(ggformula)
library(ggplot2)
library(gridExtra)
library(here)

here::i_am("reference/img_generators/gen_img_gf_sd_ruler.R")
img_dir  <- here("reference", "img")
repo_dir <- here()

save_plot <- function(p, filename, width = 4.5, height = 3) {
  ggsave(file.path(img_dir, filename), plot = p, width = width, height = height, dpi = 150)
  cat("saved:", filename, "\n")
}

# Extended prototype: overrides coursekata-r version to add histogram support
source(file.path(repo_dir, "gf_sd_ruler.R"))

# Shared data used in notebook Ch 6.4-6.6
set.seed(141)
feedback_data <- data.frame(
  student_id  = 1:100,
  median_time = round(rnorm(100, 13, 3), 1)
)
set.seed(154)
no_feedback_data <- data.frame(
  student_id  = 1:100,
  median_time = round(rnorm(100, 13, 6), 1)
)

feedback_empty    <- lm(median_time ~ NULL, data = feedback_data)
no_feedback_empty <- lm(median_time ~ NULL, data = no_feedback_data)

# ── 1. Basic SD ruler ──────────────────────────────────────────────────────────
p1 <- gf_point(median_time ~ student_id, data = no_feedback_data) %>%
  gf_model(no_feedback_empty) %>%
  gf_sd_ruler(color = "red", size = 2) +
  labs(title = "no_feedback: SD ruler shows typical residual size")
save_plot(p1, "gf_sd_ruler_basic.png")

# ── 2. Side-by-side comparison ─────────────────────────────────────────────────
p_feedback <- gf_point(median_time ~ student_id, data = feedback_data) %>%
  gf_lims(y = c(0, 30)) %>%
  gf_labs(title = "feedback (SD ≈ 3 sec)") %>%
  gf_model(feedback_empty) %>%
  gf_sd_ruler(color = "red", size = 2)

p_no_feedback <- gf_point(median_time ~ student_id, data = no_feedback_data) %>%
  gf_lims(y = c(0, 30)) %>%
  gf_labs(title = "no_feedback (SD ≈ 6 sec)") %>%
  gf_model(no_feedback_empty) %>%
  gf_sd_ruler(color = "red", size = 2)

p2 <- gridExtra::arrangeGrob(p_feedback, p_no_feedback, ncol = 2)
ggsave(file.path(img_dir, "gf_sd_ruler_compare.png"), plot = p2, width = 9, height = 3, dpi = 150)
cat("saved: gf_sd_ruler_compare.png\n")

# ── 3. Categorical x ───────────────────────────────────────────────────────────
p3 <- gf_jitter(Thumb ~ Sex, data = Fingers, width = 0.1, alpha = 0.4) %>%
  gf_model(lm(Thumb ~ NULL, data = Fingers)) %>%
  gf_sd_ruler(where = "mean") +
  labs(title = "Categorical x: ruler placed at mean of x positions")
save_plot(p3, "gf_sd_ruler_cat.png")

# ── 4. Continuous x with where = "mean" ───────────────────────────────────────
p4 <- gf_point(Thumb ~ Height, data = Fingers, alpha = 0.4) %>%
  gf_model(lm(Thumb ~ NULL, data = Fingers)) %>%
  gf_sd_ruler(where = "mean") +
  labs(title = "Continuous x: ruler at mean of Height")
save_plot(p4, "gf_sd_ruler_x_placement.png")

# ── 5. Histogram: single distribution ─────────────────────────────────────────
p5 <- gf_histogram(~ Thumb, data = Fingers, binwidth = 3) %>%
  gf_model(lm(Thumb ~ NULL, data = Fingers)) %>%
  gf_sd_ruler(color = "red", size = 2) +
  labs(title = "Histogram: SD as horizontal segment from mean")
save_plot(p5, "gf_sd_ruler_histogram.png")

# ── 6. Histogram: side-by-side comparison ─────────────────────────────────────
p_h_feedback <- gf_histogram(~ median_time, data = feedback_data, binwidth = 1) %>%
  gf_lims(x = c(0, 30)) %>%
  gf_labs(title = "feedback (SD ≈ 3 sec)") %>%
  gf_model(feedback_empty) %>%
  gf_sd_ruler(color = "red", size = 2)

p_h_no_feedback <- gf_histogram(~ median_time, data = no_feedback_data, binwidth = 1) %>%
  gf_lims(x = c(0, 30)) %>%
  gf_labs(title = "no_feedback (SD ≈ 6 sec)") %>%
  gf_model(no_feedback_empty) %>%
  gf_sd_ruler(color = "red", size = 2)

p6 <- gridExtra::arrangeGrob(p_h_feedback, p_h_no_feedback, ncol = 2)
ggsave(file.path(img_dir, "gf_sd_ruler_histogram_compare.png"), plot = p6, width = 9, height = 3, dpi = 150)
cat("saved: gf_sd_ruler_histogram_compare.png\n")

cat("\nAll images saved to", img_dir, "\n")
